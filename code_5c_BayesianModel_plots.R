
source("code_0_header.R")

#####################
### Helpers
#####################
summarise_ME <- function(draws) {
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
}

plot_ME <- function(df, out_path) {
  df %>%
    filter(str_detect(param, "ppPerSd")) %>%
    ggplot(aes(y = mean, x = method, color = T)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("5" = "blue", "25" = "red")) +
    geom_point(size = 3, position = position_dodge(width = 0.1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.1, position = "dodge", linewidth = 0.9) +
    theme(axis.text.x = element_text(size = 20)) +
    scale_y_continuous(breaks = seq(-10, 10, by = 0.1)) +
    labs(
      x = "",
      title = "Posterior mean & 95% credible interval",
      y = "Marginal effect (p.p. per. s.d.)"
    )
  ggsave(out_path, width = 6, height = 6, dpi = 300)
  ggsave(sub("\\.png$", ".jpeg", out_path), width = 6, height = 6, dpi = 300)
}

plot_phi <- function(draws, out_path) {
  ggplot(draws, aes(x = phi)) +
    geom_histogram(fill = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")
  ggsave(out_path, width = 5, height = 4, dpi = 300)
  ggsave(sub("\\.png$", ".jpeg", out_path), width = 5, height = 4, dpi = 300)
}

#####################
### Per-spec dispatch
### (no AR coefficient plot for spec 4: useYearEffect = 0, phi is unused)
#####################
specs <- tibble::tribble(
  ~label,       ~in_file,                                       ~me_out,                                                       ~phi_out,
  "original",   "fullyBayesianModel_A_spec_0_original.rds",    "plots/plot_fullBayesianModelMarginalEffects.png",            "plots/plot_fullBayesianModelARCoeff.png",
  "lagGrowth",  "fullyBayesianModel_A_spec_4_lagGrowth.rds",   "plots/plot_fullBayesianModelMarginalEffects_lagGrowth.png",  NA_character_,
)

for (i in seq_len(nrow(specs))) {
  draws <- readRDS(specs$in_file[i])
  print(posterior::summarise_draws(draws, posterior::default_convergence_measures()))
  plot_ME(summarise_ME(draws), specs$me_out[i])
  if (!is.na(specs$phi_out[i])) {
    plot_phi(draws, specs$phi_out[i])
  }
}
