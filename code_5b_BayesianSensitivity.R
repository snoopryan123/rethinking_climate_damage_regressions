
source("code_0_header.R")

### stan sampling presets
REFRESH    = 100
NUM_ITERS  = 10000
NUM_CHAINS = 4

#####################
### Sensitivity specs (Reviewer 2 point 4)
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
### Fit each spec, cache to its own .rds (skip if already fit)
#####################
for (i in seq_len(nrow(spec_grid))) {
  s <- spec_grid[i, ]
  rds_path <- paste0("fullyBayesianModel_A_spec_", s$spec, ".rds")

  if (file.exists(rds_path)) {
    message("skip (cached): ", rds_path)
    next
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

  fit <- mod$sample(
    data          = stan_data,
    seed          = 2025,
    chains        = NUM_CHAINS,
    iter_warmup   = NUM_ITERS,
    iter_sampling = NUM_ITERS,
    refresh       = REFRESH
  )
  fit$save_object(rds_path)
  message("saved: ", rds_path)
}
