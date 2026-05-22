
### Posterior predictive check that the model reproduces country- and
### region-level mean growth (Reviewer 2, Major Concern 4, sub-bullet iii).
### Run AFTER code_5b has produced
###   fullyBayesianModel_A_yrep_spec_0_original.rds
###   fullyBayesianModel_A_yrep_spec_4_lagGrowth.rds

source("code_0_header.R")
library(patchwork)
theme_set(theme_bw())
theme_update(text = element_text(size = 16))
theme_update(plot.title = element_text(hjust = 0.5))

### Minimum observations per group for level fit to be a meaningful PPC.
### Single-obs groups (e.g., one Serbian region observed once) have a
### "mean" that's just a single noisy realization, not a level.
MIN_OBS_PER_GROUP <- 5

### Replicate-vs-observed comparison at a chosen grouping level.
### Returns one row per group with: observed mean, rep posterior median,
### and rep 2.5/97.5 percentiles. Drops groups with too few obs and groups
### whose observed mean is a Tukey outlier (|x - median| > 1.5*IQR cutoff)
### of the observed-mean distribution.
ppc_group_means <- function(yrep, dat, group_col) {
  grp <- dat[[group_col]]
  group_n <- table(grp)
  keep_groups <- names(group_n)[group_n >= MIN_OBS_PER_GROUP]
  uniq <- sort(keep_groups)
  ### group means in observed data
  obs_means <- vapply(uniq, function(g) mean(dat$growth[grp == g]), numeric(1))
  ### drop Tukey outliers in observed-mean distribution (principled,
  ### distribution-driven, applied identically across specs)
  q1  <- quantile(obs_means, 0.25, na.rm = TRUE)
  q3  <- quantile(obs_means, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  in_range <- obs_means >= (q1 - 1.5 * iqr) & obs_means <= (q3 + 1.5 * iqr)
  uniq      <- uniq[in_range]
  obs_means <- obs_means[in_range]
  ### group means for each replicate (R x G matrix)
  rep_means <- vapply(uniq, function(g) {
    cols <- which(grp == g)
    rowMeans(yrep[, cols, drop = FALSE])
  }, numeric(nrow(yrep)))
  tibble(
    group = uniq,
    obs   = obs_means,
    rep_median = apply(rep_means, 2, median),
    rep_lo     = apply(rep_means, 2, quantile, probs = 0.025),
    rep_hi     = apply(rep_means, 2, quantile, probs = 0.975)
  )
}

### coverage = % of groups whose observed mean lies inside the PPC 95% band
ppc_coverage <- function(tab) {
  mean(tab$obs >= tab$rep_lo & tab$obs <= tab$rep_hi)
}

run_ppc_for_spec <- function(spec_label, yrep_path) {
  yrep <- readRDS(yrep_path)
  stopifnot(ncol(yrep) == nrow(dat1))
  tab_country <- ppc_group_means(yrep, dat1, "country")
  tab_region  <- ppc_group_means(yrep, dat1, "region")
  list(
    spec = spec_label,
    country = tab_country, region = tab_region,
    cov_country = ppc_coverage(tab_country),
    cov_region  = ppc_coverage(tab_region)
  )
}

ppc0 <- run_ppc_for_spec("AR(1) year effect",
                         "fullyBayesianModel_A_yrep_spec_0_original.rds")
ppc4 <- run_ppc_for_spec("Lag growth model",
                         "fullyBayesianModel_A_yrep_spec_4_lagGrowth.rds")

make_panel <- function(tab, panel_title, cov, ylab_show = TRUE) {
  lims <- range(c(tab$obs, tab$rep_lo, tab$rep_hi), na.rm = TRUE)
  p <- ggplot(tab, aes(x = obs, y = rep_median)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = rep_lo, ymax = rep_hi),
                  width = 0, alpha = 0.4, color = "steelblue") +
    geom_point(size = 1.6, color = "steelblue") +
    coord_equal(xlim = lims, ylim = lims) +
    labs(
      title    = panel_title,
      subtitle = sprintf("95%% PPC interval coverage: %.0f%%", 100 * cov),
      x = "Observed mean growth",
      y = if (ylab_show) "Replicated mean growth (posterior median)" else NULL
    )
  if (!ylab_show) p <- p + theme(axis.title.y = element_blank())
  p
}

p00 <- make_panel(ppc0$country, "Country Level",
                  ppc0$cov_country, ylab_show = TRUE)
p01 <- make_panel(ppc0$region,  "Region Level",
                  ppc0$cov_region, ylab_show = FALSE)
p10 <- make_panel(ppc4$country, "Country Level",
                  ppc4$cov_country, ylab_show = TRUE)
p11 <- make_panel(ppc4$region,  "Region Level",
                  ppc4$cov_region, ylab_show = FALSE)

ppc_plot_AR1        <- p00 + p01
ppc_plot_lagGrowth  <- p10 + p11

ggsave("plots/plot_PPC_groupMeans_AR1.png",
       ppc_plot_AR1, width = 12, height = 6)
ggsave("plots/plot_PPC_groupMeans_lagGrowth.png",
       ppc_plot_lagGrowth, width = 12, height = 6)

cat("\n=== PPC summary (coverage of 95% PPC intervals) ===\n")
cat(sprintf("%-20s | country: %.0f%%  | region: %.0f%%\n",
            ppc0$spec, 100*ppc0$cov_country, 100*ppc0$cov_region))
cat(sprintf("%-20s | country: %.0f%%  | region: %.0f%%\n",
            ppc4$spec, 100*ppc4$cov_country, 100*ppc4$cov_region))
