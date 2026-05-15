
source("code_0_header.R")

#####################
### Read all OOS results and plot (run after code_6 and code_6b finish)
#####################

make_cv_plot <- function(csv_prefix, plot_title) {
  df_results = tibble()
  for (yr_idx in 15:34) {
    csv_path <- paste0("results/", csv_prefix, yr_idx, ".csv")
    if (!file.exists(csv_path)) { message("missing: ", csv_path); next }
    df_yr = read_csv(csv_path, show_col_types = F)
    df_results = bind_rows(df_results, df_yr)
  }
  message(nrow(df_results), " test years loaded for ", csv_prefix)

  df_results_1 =
    df_results %>%
    select(test_year, overallMean, withClimateVars, withoutClimateVars)

  df_results_posteriorMeans =
    df_results_1 %>%
    mutate(
      percent_improvement_wC_woC = - (withClimateVars - withoutClimateVars) / withoutClimateVars,
      percent_improvement_wC_Om = - (withClimateVars - overallMean) / overallMean,
      percent_improvement_woC_Om = - (withoutClimateVars - overallMean) / overallMean,
    )

  ylab = "Reduction in Error (RMSE)\n(higher is better)"

  plot_wCwoC =
    df_results_posteriorMeans %>%
    select(percent_improvement_wC_woC) %>%
    ggplot(aes(x = 0, y = percent_improvement_wC_woC)) +
    geom_boxplot(outliers = F, width=0.1) +
    geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
    scale_y_continuous(labels = scales::percent) +
    theme(
      axis.text.x = element_blank(),
      plot.subtitle = element_text(size = 14),
    ) +
    labs(
      x="", y=ylab,
      subtitle="Reduction in Error by\nIncluding Climate Variables Above Not"
    )

  plot_aboveOvrMean =
    df_results_posteriorMeans %>%
    select(all_of(starts_with("percent_improvement"))) %>%
    select(all_of(ends_with("Om"))) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = name, y = value)) +
    geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=0.5) +
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
    )

  plot_aboveOvrMean + plot_wCwoC +
    plot_annotation(title = plot_title)
}

### C&M model (code_6)
plot_cv_predictNextYear = make_cv_plot(
  "results_bayesianOutOfSampleTest",
  "Rolling out-of-sample predictions of next year"
)
ggsave("plots/plot_cv_predictNextYear.png", width=9, height=4)

### Lag growth model (code_6b)
plot_cv_predictNextYear_lagGrowth = make_cv_plot(
  "results_bayesianOutOfSampleTest_lagGrowth",
  "Rolling out-of-sample predictions of next year (lag growth model)"
)
ggsave("plots/plot_cv_predictNextYear_lagGrowth.png", width=9, height=4)
