
source("code_0_header.R")

### stan sampling presets
REFRESH=100
NUM_ITERS = 2000
NUM_CHAINS=1
THIN = 2    # 2000 sampling / thin 2 = 1000 draws/chain -> 4000 total

### model spec: lag growth, no year effect (matches spec "4_lagGrowth" in code_5b)
USE_YEAR_EFFECT       = 0L
USE_AR_YEAR_EFFECT    = 0L
BETA_PRIOR_SD_COEF    = 10
REM_COLS              = c("var", "var_seas", "p")
ALWAYS_COLS           = c("growth_lag1")

#####################
### Validating the Bayesian model via out-of-sample testing
### Usage:  Rscript code_6b_... 15 19    # runs year indices 15-19
###         Rscript code_6b_... 20 24    # runs year indices 20-24
###         Rscript code_6b_...          # runs all (15:34)
#####################
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2) {
  MAX_TRAINING_YR_VEC <- as.integer(args[1]):as.integer(args[2])
} else {
  MAX_TRAINING_YR_VEC <- 15:34
}
message("fitting year indices: ", paste(MAX_TRAINING_YR_VEC, collapse = ", "))

### compile the stan model once (outside loop)
stan_filename = "fullyBayesianModel_A.stan"
mod <- cmdstan_model(stan_filename)

for (max_training_year_idx in MAX_TRAINING_YR_VEC) {
  message("**** max_training_year_idx = ", max_training_year_idx,"/",last(MAX_TRAINING_YR_VEC)," ****")

  csv_path <- paste0("results/results_bayesianOutOfSampleTest_lagGrowth", max_training_year_idx, ".csv")
  if (file.exists(csv_path)) { message("skip (cached): ", csv_path); next }

  testing_year_idx = max_training_year_idx+1
  years = sort(unique(dat1$time))
  train_years = years[1:max_training_year_idx]
  test_year = years[testing_year_idx]

  ### get data list for stan model
  stan_data_withClimateVars = get_data_list_for_stan(
    dat=dat1, train_years=train_years, test_year=test_year, useClimateVars=1,
    rem_cols=REM_COLS, always_cols=ALWAYS_COLS,
    useYearEffect=USE_YEAR_EFFECT, useARYearEffect=USE_AR_YEAR_EFFECT, beta_prior_sd_coef=BETA_PRIOR_SD_COEF
  )
  stan_data_withoutClimateVars = get_data_list_for_stan(
    dat=dat1, train_years=train_years, test_year=test_year, useClimateVars=0,
    rem_cols=REM_COLS, always_cols=ALWAYS_COLS,
    useYearEffect=USE_YEAR_EFFECT, useARYearEffect=USE_AR_YEAR_EFFECT, beta_prior_sd_coef=BETA_PRIOR_SD_COEF
  )

  ### overall mean baseline
  dtr = stan_data_withClimateVars$dat_train
  dte = stan_data_withClimateVars$dat_test
  rmse_overallMean = RMSE(mean(dtr$growth), dte$growth)

  ### fit
  fit_withClimateVars = fit_stan_model(
    stan_data_withClimateVars,
    iter_warmup=NUM_ITERS, iter_sampling=NUM_ITERS, refresh=REFRESH, num_chains=NUM_CHAINS, thin=THIN
  )
  fit_withoutClimateVars = fit_stan_model(
    stan_data_withoutClimateVars,
    iter_warmup=NUM_ITERS, iter_sampling=NUM_ITERS, refresh=REFRESH, num_chains=NUM_CHAINS, thin=THIN
  )

  ### RMSE from posterior mean predictions
  pred_wC <- colMeans(posterior::as_draws_matrix(
    fit_withClimateVars$draws(variables = "pred_test")))
  pred_woC <- colMeans(posterior::as_draws_matrix(
    fit_withoutClimateVars$draws(variables = "pred_test")))

  rmse_df = tibble(
    test_year          = test_year,
    withClimateVars    = sqrt(mean((pred_wC  - dte$growth)^2)),
    withoutClimateVars = sqrt(mean((pred_woC - dte$growth)^2)),
    overallMean        = rmse_overallMean
  )
  rmse_df

  write_csv(rmse_df, csv_path)
}

### Plotting moved to code_6c_BayesianModel_outOfSamplePlots.R
