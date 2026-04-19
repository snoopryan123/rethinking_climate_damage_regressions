
source("code_0_header.R")

#####################
### Load posterior draws 
#####################
draws <- readRDS("fullyBayesianModel_A_spec_0_original.rds")

### Convergence summary on saved params (rhat, ess_bulk, ess_tail)
posterior::summarise_draws(draws, posterior::default_convergence_measures())

#####################
### Plot 1: marginal effects (mean & 95% CrI, scaled to p.p. per s.d.)
#####################
df_plot_ME_posteriorCI <-
  draws %>%
  select(ME_tx5d_t5, ME_tx5d_t25) %>%
  mutate(
    ME_tx5d_t5_ppPerSd  = ME_tx5d_t5  * sd_withinRegionTx5d,
    ME_tx5d_t25_ppPerSd = ME_tx5d_t25 * sd_withinRegionTx5d,
  ) %>%
  summarise(across(everything(), list(
    mean  = mean,
    lower = ~quantile(.x, 0.025),
    upper = ~quantile(.x, 0.975)
  ))) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = c("param", ".value"),
    names_pattern = "(.*)_(mean|lower|upper)"
  ) %>%
  mutate(
    T = factor(str_remove_all(str_remove_all(param, "ME_tx5d_t"), "_ppPerSd"),
               levels = c("5", "25")),
    method = "Bayesian model"
  )
df_plot_ME_posteriorCI

plot_bayesianMarginalEffects <-
  df_plot_ME_posteriorCI %>%
  filter(str_detect(param, "ppPerSd")) %>%
  ggplot(aes(y = mean, x = method, color = T)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("5" = "blue", "25" = "red")) +
  geom_point(size = 3, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1, position = "dodge") +
  theme(axis.text.x = element_text(size = 20)) +
  scale_y_continuous(breaks=seq(-10,10,b=0.1)) +
  labs(
    x = "",
    title = "Posterior mean & 95% credible interval",
    y = "Marginal effect (p.p. per. s.d.)"
  )
ggsave("plots/plot_fullBayesianModelMarginalEffects.png", width = 6, height = 6)

#####################
### Plot 2: posterior of the autoregressive coefficient phi
#####################
plot_ARCoeff <-
  ggplot(draws, aes(x = phi)) +
  geom_histogram(fill = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")
ggsave("plots/plot_fullBayesianModelARCoeff.png", width = 5, height = 4)
