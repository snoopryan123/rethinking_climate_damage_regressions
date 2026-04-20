
source("code_0_header.R")

#####################
### Load posterior draws from each Bayesian model spec
#####################

spec_ids <- c("0_original", "3_tightPrior", "2_indepYearRE", "1_noYearEff")

spec_labels <- c(
  "0_original"    = "Original Bayesian Model\n(AR(1) year, prior SD=10)",
  "3_tightPrior"  = "Tight prior (AR(1), prior SD=1)",
  "2_indepYearRE" = "Independent year RE (no AR)",
  "1_noYearEff"   = "No year effect"
)

df_post <- map_dfr(spec_ids, function(s) {
  rds_path <- paste0("fullyBayesianModel_A_spec_", s, ".rds")
  if (!file.exists(rds_path)) {
    warning("missing: ", rds_path, " (run code_5b first); skipping")
    return(NULL)
  }
  readRDS(rds_path) %>%    # already a draws_df
    select(starts_with("ME_")) %>%
    pivot_longer(starts_with("ME_"), names_to = "qoi", values_to = "draw") %>%
    mutate(spec = s, draw = draw * sd_withinRegionTx5d)
})

df_summary <- df_post %>%
  group_by(spec, qoi) %>%
  summarise(
    mean = mean(draw),
    lo   = quantile(draw, 0.025),
    hi   = quantile(draw, 0.975),
    .groups = "drop"
  ) %>%
  mutate(
    qoi      = recode(qoi, ME_tx5d_t5 = "ME at t = 5 C", ME_tx5d_t25 = "ME at t = 25 C"),
    spec_lab = factor(spec_labels[spec], levels = rev(unname(spec_labels)))
  )
df_summary

#####################
### Comparison plot
#####################

plot_bayes_sensitivity <-
  ggplot(df_summary, aes(x = mean, y = spec_lab, color = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.9) +
  geom_point(size = 3) +
  facet_wrap(~ qoi, scales = "free_x", ncol = 1) +
  guides(color = "none") +
  labs(
    x = "Marginal effect (posterior mean & 95% CrI, p.p. per s.d.)",
    y = NULL,
    title = "Bayesian Model Sensitivity Analysis: year-effect spec & prior",
    # subtitle = "Same data, different model structure / prior."
  ) +
  theme(plot.margin = margin(5, 30, 5, 5))
ggsave("plots/plot_bayes_sensitivity.png", plot_bayes_sensitivity,
       width = 11, height = 7, dpi = 200)
# plot_bayes_sensitivity
