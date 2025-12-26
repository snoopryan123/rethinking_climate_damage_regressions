
source("code_0_header.R")

# ### get the index of the maximum training year 
# args <- commandArgs(trailingOnly = TRUE)
# max_training_year_idx = as.numeric(args[1]) ### sim index

### look at the years in the dataset
MAX_TRAINING_YR_VEC = 15:34 # 15:34 # 20:34
table(dat1$time)
table(dat1$time)[MAX_TRAINING_YR_VEC]

### stan sampling presets
REFRESH=100
NUM_ITERS = 10000 #2500 #10000
NUM_CHAINS=4 #1 #4

#####################
### Validating the Bayesian model via out-of-sample testing
#####################

# # max_training_year_idx = 10 # just for testing this codefile
for (max_training_year_idx in MAX_TRAINING_YR_VEC) {
  print(paste0("**** max_training_year_idx = ", max_training_year_idx,"/",last(MAX_TRAINING_YR_VEC)," ****"))

  ###
  testing_year_idx = max_training_year_idx+1
  years = sort(unique(dat1$time))
  length(years)
  train_years = years[1:max_training_year_idx]
  test_year = years[testing_year_idx]
  train_years
  test_year
  
  ### get data list for stan model
  stan_data_withClimateVars = get_data_list_for_stan(
    dat=dat1, train_years=train_years, test_year=test_year, useClimateVars=1, rem_cols=c("var", "var_seas", "p")
  )
  stan_data_withoutClimateVars = get_data_list_for_stan(
    dat=dat1, train_years=train_years, test_year=test_year, useClimateVars=0, rem_cols=c("var", "var_seas", "p")
  )
  
  ### overall mean baseline
  dtr = stan_data_withClimateVars$dat_train
  dte = stan_data_withClimateVars$dat_test
  rmse_overallMean = RMSE(
    mean(dtr$growth)
    ,
    dte$growth
  )
  rmse_overallMean
  
  ### compile the stan model
  stan_filename = "fullyBayesianModel_A.stan"
  mod <- cmdstan_model(stan_filename)
  mod
  
  ### sanity check just to make sure the sampling runs
  fit_withClimateVars = fit_stan_model(
    stan_data_withClimateVars, 
    iter_warmup=NUM_ITERS, iter_sampling=NUM_ITERS, refresh=REFRESH, num_chains=NUM_CHAINS
  )
  fit_withoutClimateVars = fit_stan_model(
    stan_data_withoutClimateVars, 
    iter_warmup=NUM_ITERS, iter_sampling=NUM_ITERS, refresh=REFRESH, num_chains=NUM_CHAINS
  )
  
  fit_withClimateVars
  fit_withoutClimateVars
  
  ### RMSE result
  rmse_draws_withClimateVars = 
    posterior::as_draws_df(
      fit_withClimateVars$draws(variables = c("rmse_test"))
    ) %>% 
    select(-c(".chain",".draw",".iteration")) %>%
    rename(withClimateVars = rmse_test)
  
  rmse_draws_withoutClimateVars = 
    posterior::as_draws_df(
      fit_withoutClimateVars$draws(variables = c("rmse_test"))
    ) %>% 
    select(-c(".chain",".draw",".iteration")) %>%
    rename(withoutClimateVars = rmse_test)
  
  rmse_df = 
    bind_cols(
      rmse_draws_withClimateVars,
      rmse_draws_withoutClimateVars
    ) %>%
    mutate(
      iter = 1:n(),
      overallMean = rmse_overallMean,
      # percent_improvement_wC_woC = - (withClimateVars - withoutClimateVars) / withoutClimateVars,
      # percent_improvement_wC_Om = - (withClimateVars - overallMean) / overallMean,
      # percent_improvement_woC_Om = - (withoutClimateVars - overallMean) / overallMean,
    ) %>%
    mutate(test_year = test_year)
  rmse_df  
  
  write_csv(rmse_df, paste0("results/results_bayesianOutOfSampleTest",max_training_year_idx,".csv"))
}

#####################
### Visualize the results
#####################

df_results = tibble()
for (yr_idx in MAX_TRAINING_YR_VEC) {
  print(paste0("**** yr_idx = ", yr_idx,"/",last(MAX_TRAINING_YR_VEC)," ****"))
  df_yr = read_csv(paste0("results/results_bayesianOutOfSampleTest",yr_idx,".csv"), show_col_types = F)
  df_results = bind_rows(df_results, df_yr)
}
df_results

df_results_1 = 
  df_results %>%
  select(test_year, iter, overallMean, withClimateVars, withoutClimateVars)
df_results_1

df_results_posteriorMeans = 
  df_results_1 %>%
  group_by(test_year) %>%
  reframe(
    # rmse of the overall mean for each year
    overallMean = unique(overallMean),
    # posterior mean RMSEs within each year
    withClimateVars = mean(withClimateVars),
    withoutClimateVars = mean(withoutClimateVars),
  ) %>%
  mutate(
    # percent improvement of including climate variables above not
    percent_improvement_wC_woC = - (withClimateVars - withoutClimateVars) / withoutClimateVars,
    # percent improvement of including climate variables above the overall mean
    percent_improvement_wC_Om = - (withClimateVars - overallMean) / overallMean,
    # percent improvement of not including climate variables above the overall mean
    percent_improvement_woC_Om = - (withoutClimateVars - overallMean) / overallMean,
  ) 
df_results_posteriorMeans

### PLOT OUT-OF-SAMPLE TESTING RESULTS
ylab = "Reduction in Error (RMSE)\n(higher is better)"
plot_cv_predictNextYear_wCwoC = 
  df_results_posteriorMeans %>%
  select(percent_improvement_wC_woC) %>%
  ggplot(aes(x = 0, y = percent_improvement_wC_woC)) +
  # geom_boxplot(width=0.1) +
  geom_boxplot(outliers = F, width=0.1) +
  geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    axis.text.x = element_blank(),
    plot.subtitle = element_text(size = 14),
  ) +
  labs(
    x="", y=ylab,
    subtitle="Reduction in Error by\nIncluding Climate Variables Above Not"
    # subtitle="Reduction in\nError (RMSE)\nby Including\nClimate Variables\nAbove Not"
  )
# plot_cv_predictNextYear_wCwoC

plot_cv_predictNextYearAboveOvrMean = 
  df_results_posteriorMeans %>%
  select(all_of(starts_with("percent_improvement"))) %>%
  select(all_of(ends_with("Om"))) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=0.5) +
  # geom_boxplot() +
  geom_boxplot(outliers = F) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.subtitle = element_text(size = 14),
  ) +
  scale_y_continuous(labels = scales::percent)  +
  scale_x_discrete(labels = c(
    "percent_improvement_wC_Om" = "Yes",
    "percent_improvement_woC_Om" = "No"
  )) +
  labs(
    x="Include Climate Variables", y=ylab,
    subtitle="Reduction in Error\nAbove the Overall Mean"
    # subtitle="Reduction in\nError (RMSE)\nAbove the\nOverall Mean"
  )
# plot_cv_predictNextYearAboveOvrMean

plot_cv_predictNextYear = 
  plot_cv_predictNextYearAboveOvrMean + 
  plot_cv_predictNextYear_wCwoC +
  plot_annotation(title = "Rolling out-of-sample predictions of next year")
# plot_cv_predictNextYear
ggsave("plots/plot_cv_predictNextYear.png", width=9, height=4)

