
source("code_0_header.R")

### stan sampling presets
REFRESH    = 100
NUM_ITERS  = 10000
NUM_CHAINS = 4
THIN       = 10   # keep every 10th draw -> 1000 draws/chain -> 4000 total

### variables to save (omitting province/country/year REs keeps files small)
SAVE_VARS  = c("beta_tx5d", "beta_tx5d_t", "phi", "sigma",
               "sigma_country", "sigma_province", "sigma_time",
               "ME_tx5d_t5", "ME_tx5d_t25")

#####################
### Bayesian Model Specifications (Reviewer 2 point 4)
#####################
spec_grid <- tribble(
  ~spec,            ~useYearEffect, ~useARYearEffect, ~prior_sd_tx5d_coef,
  "0_original",     1L,             1L,               10,
  "1_noYearEff",    0L,             0L,               10,
  "2_indepYearRE",  1L,             0L,               10,
  "3_tightPrior",   1L,             1L,                1
)
spec_grid

#####################
### Compile the stan model once
#####################
stan_filename = "fullyBayesianModel_A.stan"
mod <- cmdstan_model(stan_filename)
mod

#####################
### Fit one spec (passed as command-line argument)
### Usage:  Rscript code_5b_BayesianSensitivity.R 0_original
###         Rscript code_5b_BayesianSensitivity.R 1_noYearEff
###         Rscript code_5b_BayesianSensitivity.R 2_indepYearRE
###         Rscript code_5b_BayesianSensitivity.R 3_tightPrior
#####################
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1 || !(args[1] %in% spec_grid$spec)) {
  stop("Usage: Rscript code_5b_BayesianSensitivity.R <spec>\n",
       "  where <spec> is one of: ", paste(spec_grid$spec, collapse=", "))
}

s <- spec_grid[spec_grid$spec == args[1], ]
rds_path <- paste0("fullyBayesianModel_A_spec_", s$spec, ".rds")

if (file.exists(rds_path)) {
  message("skip (cached): ", rds_path)
  quit(save = "no")
}
message("fitting spec: ", s$spec)

stan_data <- get_data_list_for_stan(
  dat = dat1,
  train_years = sort(unique(dat1$time)),
  useClimateVars = 1,
  rem_cols = c("var", "var_seas", "p"),
  useYearEffect      = s$useYearEffect,
  useARYearEffect    = s$useARYearEffect,
  prior_sd_tx5d_coef = s$prior_sd_tx5d_coef
)

fit <- fit_stan_model(
  stan_data,
  iter_warmup   = NUM_ITERS,
  iter_sampling = NUM_ITERS,
  refresh       = REFRESH,
  num_chains    = NUM_CHAINS,
  thin          = THIN
)

### save only the quantities we need (much smaller than save_object)
draws_to_save <- posterior::as_draws_df(fit$draws(variables = SAVE_VARS))
saveRDS(draws_to_save, rds_path)
message("saved: ", rds_path)
