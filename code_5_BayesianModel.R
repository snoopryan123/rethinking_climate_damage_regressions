
source("code_0_header.R")

### stan sampling presets
# NUM_ITERS = 5
# REFRESH=1
# NUM_CHAINS=1
NUM_ITERS = 2500 #1000
REFRESH=100
NUM_CHAINS=1

#####################
### Fit the Fully Bayesian Model
#####################

### get data list for stan model
stan_data = get_data_list_for_stan(
  dat=dat1, train_years=sort(unique(dat1$time)), useClimateVars=1, rem_cols=c("var", "var_seas", "p")
)
names(stan_data)

# ### check the data is kosher (expect TRUE)
# checkme  = function(x) { all(!is.na(x) & is.finite(x)) }
# suppressWarnings(all(lapply(stan_data, checkme)))

### compile the stan model
stan_filename = "fullyBayesianModel_A.stan"
mod <- cmdstan_model(stan_filename)
mod

### fit the stan model
fit <- mod$sample(
  data = stan_data,
  seed = 2025,
  chains = NUM_CHAINS,
  # parallel_chains = NUM_CHAINS,
  # variables = c("beta_t","beta_t_sq","beta_tx5d","beta_tx5d_t","ME_tx5d_t5","ME_tx5d_t25"),  # only save these
  iter_warmup = NUM_ITERS,
  iter_sampling = NUM_ITERS,
  refresh = REFRESH,
  # output_dir = "."
)
fit

# ### save the model
# fit$save_object("fullyBayesianModel_A.rds")

#####################
### Visualize the results
#####################

# re-load 
fit1 <- fit
# fit1 <- readRDS("fullyBayesianModel_A.rds")
fit1

# # Quick checks 
# print(fit1$summary(c("beta_0","beta_t","beta_t_sq","beta_tx5d","beta_tx5d_t","sigma",
#                     "sigma_country","sigma_province","sigma_time","phi")))

### Visualize Marginal Effects
df_plot_ME_posteriorCI = 
  posterior::as_draws_df(
    fit1$draws(variables = c("ME_tx5d_t5","ME_tx5d_t25"))
  ) %>% 
  select(-c(".chain",".draw",".iteration")) %>%
  mutate(
    ME_tx5d_t5_ppPerSd = ME_tx5d_t5 * sd_withinRegionTx5d,
    ME_tx5d_t25_ppPerSd = ME_tx5d_t25 * sd_withinRegionTx5d,
  ) %>%
  summarise(across(everything(), list(
    mean = mean,
    lower = ~quantile(.x, 0.025),
    upper = ~quantile(.x, 0.975)
  ))) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = c("param", ".value"),
    names_pattern = "(.*)_(mean|lower|upper)"
  ) %>%
  mutate(
    T = factor(str_remove_all(str_remove_all(param, "ME_tx5d_t"),"_ppPerSd"), levels=c("5","25")),
    method = "Our Bayesian model"
  )
df_plot_ME_posteriorCI

plot_bayesianMarginalEffects =
  df_plot_ME_posteriorCI %>%
  filter(str_detect(param, "ppPerSd")) %>%
  ggplot(aes(y = mean, x = method, color=T)) +
  geom_hline(yintercept = 0, linetype="dashed", color="gray50") +
  scale_color_manual(values=c("5"="blue","25"="red")) +
  geom_point(size=3, position=position_dodge(width = 0.1)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position="dodge")  +
  labs(
    x="",
    title="Posterior mean & 95% credible interval",
    y = "Marginal effect (p.p. per. s.d.)",
    ) 
  # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
ggsave("plots/plot_fullBayesianModelMarginalEffects.png",width=6,height=6)

### Visualize posterior dist of the autoregressive coefficient
plot_ARCoeff = 
  posterior::as_draws_df(
    fit1$draws(variables = c("phi"))
  ) %>% 
  select(-c(".chain",".draw",".iteration")) %>%
  ggplot(aes(x=phi)) +
  geom_histogram(fill="black") +
  geom_vline(xintercept=0, linetype="dashed", color="gray50")
ggsave("plots/plot_fullBayesianModelARCoeff.png",width=5,height=4)

