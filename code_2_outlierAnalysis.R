
source("code_0_header.R")

#### Leave one country out analysis ####

###
df_country_loo = tibble()
for (ctry in sort(unique(dat1$country))) {
  df_c = dat1 %>% filter(country != ctry)
  mdl1_c <- 
    feols(
      as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time"),
      data=df_c
    )
  summary(mdl1_c)
  coeffs_c = mdl1_c$coeftable
  coeffs_c = tibble(coeff=rownames(coeffs_c),coeffs_c,country_loo=ctry)
  df_country_loo = bind_rows(df_country_loo, coeffs_c)
}
df_country_loo

df_country_loo_1 = 
  df_country_loo %>%
  filter(coeff %in% c("t","t2","tx5d","t:tx5d")) %>%
  select(country_loo, coeff, Estimate) %>%
  group_by(country_loo) %>%
  mutate(
    ME_tx5d_t5 = Estimate[coeff=="tx5d"] + 5 * Estimate[coeff=="t:tx5d"],
    ME_tx5d_t25 = Estimate[coeff=="tx5d"] + 25 * Estimate[coeff=="t:tx5d"],
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = "coeff", values_from = "Estimate") %>%
  pivot_longer(-country_loo, names_to ="coeff",values_to = "Estimate")
df_country_loo_1

plot_country_loo = 
  df_country_loo_1 %>%
  # filter(coeff %in% c("t","t2","tx5d","t:tx5d")) %>%
  ggplot(aes(x = reorder(coeff, Estimate), y = Estimate, 
             # color=Model,ymin = conf.low, ymax = conf.high
             )) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dashed", color="gray50") +
  labs(
    x = NULL, y = "Estimate", 
    title = "Leave-one-country-out coefficient distribution",
  ) +
  geom_boxplot()
# plot_country_loo
ggsave("plots/plot_loo_country.png", plot_country_loo, width=8, height=5)

#### Leave one year out analysis ####

###
df_year_loo = tibble()
for (ctry in sort(unique(dat1$time))) {
  df_c = dat1 %>% filter(time != ctry)
  mdl1_c <- 
    feols(
      as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time"),
      data=df_c
    )
  summary(mdl1_c)
  coeffs_c = mdl1_c$coeftable
  coeffs_c = tibble(coeff=rownames(coeffs_c),coeffs_c,year_loo=ctry)
  df_year_loo = bind_rows(df_year_loo, coeffs_c)
}
df_year_loo

df_year_loo_1 = 
  df_year_loo %>%
  filter(coeff %in% c("t","t2","tx5d","t:tx5d")) %>%
  select(year_loo, coeff, Estimate) %>%
  group_by(year_loo) %>%
  mutate(
    ME_tx5d_t5 = Estimate[coeff=="tx5d"] + 5 * Estimate[coeff=="t:tx5d"],
    ME_tx5d_t25 = Estimate[coeff=="tx5d"] + 25 * Estimate[coeff=="t:tx5d"],
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = "coeff", values_from = "Estimate") %>%
  pivot_longer(-year_loo, names_to ="coeff",values_to = "Estimate") 
df_year_loo_1

plot_year_loo = 
  df_year_loo_1 %>%
  # filter(coeff %in% c("t","t2","tx5d","t:tx5d")) %>%
  ggplot(aes(
    x = reorder(coeff, Estimate), y = Estimate,
    # x = coeff, y = Estimate, 
    # color=Model,ymin = conf.low, ymax = conf.high
  )) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dashed", color="gray50") +
  labs(
    x = NULL, y = "Estimate", 
    title = "Leave-one-year-out coefficient distribution",
  ) +
  geom_boxplot()
# plot_year_loo
ggsave("plots/plot_loo_year.png", plot_year_loo, width=8, height=5)

#### Leave one out final effect size changes ####

k = 3
df_country_loo_1 %>%
  filter(str_detect(coeff, "ME")) %>%
  group_by(coeff) %>%
  arrange(coeff, Estimate) %>%
  mutate(bot = row_number() <= k) %>%
  arrange(coeff, -Estimate) %>%
  mutate(top = row_number() <= k) %>%
  filter(bot | top)

df_year_loo_1 %>%
  filter(str_detect(coeff, "ME")) %>%
  group_by(coeff) %>%
  arrange(coeff, Estimate) %>%
  mutate(bot = row_number() <= k) %>%
  arrange(coeff, -Estimate) %>%
  mutate(top = row_number() <= k) %>%
  filter(bot | top)

for (j in 1:3) {
  if (j==1) {
    ### only remove 2 countries
    extreme_countries = c("MNG","KEN")
    extreme_years = c()
  } else if (j==2) {
    ### only remove 2 years
    extreme_countries = c()
    extreme_years = c(1998, 2009)
  } else if (j ==3) {
    ### remove 2 countries and 2 years
    extreme_countries = c("MNG","KEN")
    extreme_years = c(1998, 2009)
  } else {
    stop(paste0("j=",j," is not supported."))
  }
  
  ## model: original
  model_og <- 
    feols(
      as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time"),
      data=dat1
    )
  summary(model_og)
  
  model_without_outliers <- 
    feols(
      as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time"),
      data = dat1 %>% filter(
        # !(time %in% extreme_years)
        !(country %in% extreme_countries) & !(time %in% extreme_years)
      )
    )
  model_without_outliers
  
  get_coeffs_with_MEs <- function(model) {
    df_coeffs_withoutOutliers = 
      tibble(coeff=rownames(model$coeftable), model$coeftable) %>%
      filter(coeff %in% c("t","t2","tx5d","t:tx5d")) %>%
      select(coeff, Estimate) %>%
      mutate(
        ME_tx5d_t5 = Estimate[coeff=="tx5d"] + 5 * Estimate[coeff=="t:tx5d"],
        ME_tx5d_t25 = Estimate[coeff=="tx5d"] + 25 * Estimate[coeff=="t:tx5d"],
        ME_tx5d_t5 = ME_tx5d_t5*sd_withinRegionTx5d,
        ME_tx5d_t25 = ME_tx5d_t25*sd_withinRegionTx5d,
      ) %>%
      pivot_wider(names_from = "coeff", values_from = "Estimate") %>%
      pivot_longer(everything(), names_to ="coeff",values_to = "Estimate") 
    df_coeffs_withoutOutliers
  }
  
  ME_og = get_coeffs_with_MEs(model_og)
  ME_withoutOutliers = get_coeffs_with_MEs(model_without_outliers)
  
  ME_tx5d_t5_withoutOutliers = ME_withoutOutliers$Estimate[ME_withoutOutliers$coeff=="ME_tx5d_t5"]
  ME_tx5d_t5_og = ME_og$Estimate[ME_og$coeff=="ME_tx5d_t5"]
  
  ME_tx5d_t25_withoutOutliers = ME_withoutOutliers$Estimate[ME_withoutOutliers$coeff=="ME_tx5d_t25"]
  ME_tx5d_t25_og = ME_og$Estimate[ME_og$coeff=="ME_tx5d_t25"]
  
  ### view results
  print("***********************************************************************")
  print("extreme_countries:")
  print(extreme_countries)
  print("extreme_years:")
  print(extreme_years)
  print("ME_og:")
  print(ME_og)
  print("ME_withoutOutliers:")
  print(ME_withoutOutliers)
  print("scales::percent( (ME_tx5d_t5_withoutOutliers - ME_tx5d_t5_og) / abs(ME_tx5d_t5_og) )")
  print(scales::percent( (ME_tx5d_t5_withoutOutliers - ME_tx5d_t5_og) / abs(ME_tx5d_t5_og) ))
  print("scales::percent( (ME_tx5d_t25_withoutOutliers - ME_tx5d_t25_og) / abs(ME_tx5d_t25_og) )")
  print(scales::percent( (ME_tx5d_t25_withoutOutliers - ME_tx5d_t25_og) / abs(ME_tx5d_t25_og) ))
}

#### Diagnostics & Outliers ####

## model 1: original
mdl1A <- 
  feols(
    as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time"),
    data=dat1
  )
summary(mdl1A)

### plot regression diagnostics and remove outliers
plot_diagnostics <- function(dat, model, outlier_abs_thresh=3, remove_outliers=F) {
  # Extract diagnostics
  diagnostics <- 
    dat %>%
    mutate(
      fitted = predict(model,.),
      residuals = growth-fitted,
      std_resid = scale(residuals)[,1],
      is_outlier = abs(std_resid) >= outlier_abs_thresh,
    ) 
  diagnostics
  
  # View(diagnostics %>% filter(is_outlier))
  
  # Optional: remove extreme outliers for plotting clarity
  if (remove_outliers) {
    # browser()
    diagnostics <- diagnostics %>% filter(!is_outlier)
  }
  
  # Residuals vs Fitted
  p1b = 
    ggplot(diagnostics, aes(x = fitted, y = residuals, color=is_outlier)) +
    geom_point() +
    scale_color_manual(values=c(`TRUE`="red",`FALSE`="black"), guide="none") +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Residuals vs Fitted")
  
  # QQ plot
  p2b = 
    ggplot(diagnostics, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Normal Q-Q")
  
  # Scale-Location plot
  p3b = 
    ggplot(diagnostics, aes(x = fitted, color=is_outlier, y = sqrt(abs(std_resid)))) +
    scale_color_manual(values=c(`TRUE`="red",`FALSE`="black"), guide="none") +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Scale-Location")
  
  # Histogram of residuals
  p4b = 
    ggplot(diagnostics, aes(x = residuals)) +
    geom_histogram(bins = 50, fill = "gray", color = "black") +
    labs(title = "Histogram of Residuals")
  
  plot_diag = p1b + p2b + p3b + p4b
  list(plot_diag, diagnostics$is_outlier)
}

# outlier_abs_thresh=3
outlier_abs_thresh=6

diag_og = plot_diagnostics(dat1, mdl1A, outlier_abs_thresh, remove_outliers = F)
ggsave("plots/plot_reg_diag_og.png", diag_og[[1]], width=8, height=6)
dat1$is_outlier = diag_og[[2]]
sum(dat1$is_outlier)

diag_og_ro = plot_diagnostics(dat1, mdl1A, outlier_abs_thresh, remove_outliers = T)
ggsave("plots/plot_reg_diag_og_ro.png", diag_og_ro[[1]], width=8, height=6)

###
mdl1A_removeOutliers <- 
  feols(
    as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time"),
    data = dat1 %>% filter(!is_outlier)
  )
summary(mdl1A_removeOutliers)
summary(mdl1A)

#### Jackknife influence diagnostics (Reviewer 2, point 3) ####
###  Reuses df_country_loo, df_year_loo, mdl1A, sd_withinRegionTx5d.
###  Three figures, no CSVs.

### Full-sample marginal effects from mdl1A
b <- coef(mdl1A)
df_full <- tibble(
  qoi    = factor(c("ME(t=5)", "ME(t=25)"), levels = c("ME(t=5)", "ME(t=25)")),
  Q_full = c((b["tx5d"] +  5 * b["t:tx5d"]) * sd_withinRegionTx5d,
             (b["tx5d"] + 25 * b["t:tx5d"]) * sd_withinRegionTx5d)
)

### Per-unit LOO marginal effects + jackknife stats
jack_table <- function(df_loo, unit_col) {
  df_loo %>%
    filter(coeff %in% c("tx5d", "t:tx5d")) %>%
    select(all_of(unit_col), coeff, Estimate) %>%
    pivot_wider(names_from = coeff, values_from = Estimate) %>%
    transmute(
      unit       = .data[[unit_col]],
      `ME(t=5)`  = (tx5d +  5 * `t:tx5d`) * sd_withinRegionTx5d,
      `ME(t=25)` = (tx5d + 25 * `t:tx5d`) * sd_withinRegionTx5d
    ) %>%
    pivot_longer(-unit, names_to = "qoi", values_to = "Q_loo") %>%
    mutate(qoi = factor(qoi, levels = c("ME(t=5)", "ME(t=25)"))) %>%
    left_join(df_full, by = "qoi") %>%
    group_by(qoi) %>%
    mutate(
      n       = n(),
      SE_jack = sqrt((n - 1) / n * sum((Q_loo - mean(Q_loo))^2)),
      dQ_std  = (Q_loo - Q_full) / SE_jack,
      flag    = abs(dQ_std) > 2 / sqrt(n)
    ) %>%
    ungroup()
}

df_country <- jack_table(df_country_loo, "country_loo")
df_year    <- jack_table(df_year_loo,    "year_loo")

### Figure 1: Country jackknife distribution
n_c <- length(unique(df_country$unit))
plot_jackknife_country <-
  df_country %>%
  group_by(qoi) %>% mutate(rk = rank(Q_loo, ties.method = "first")) %>% ungroup() %>%
  ggplot(aes(Q_loo, rk, color = flag)) +
  geom_vline(data = df_full, aes(xintercept = Q_full), color = "blue") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  geom_point(size = 1.2) +
  geom_text(aes(label = ifelse(flag, as.character(unit), NA)),
            hjust = -0.2, size = 3, color = "red", na.rm = TRUE) +
  scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "gray40"), guide = "none") +
  facet_wrap(~ qoi, scales = "free_x") +
  labs(
    x = "Marginal effect with country i removed",
    y = paste0("Country (rank, 1..", n_c, ")"),
    title = "Country-level jackknife: leave-one-out marginal effect",
    subtitle = sprintf("Blue: full-sample estimate. Red labels: |DFBETAS| > 2/sqrt(%d). All %d countries shown.",
                       n_c, n_c)
  ) +
  theme(plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 10, color = "gray30"))
ggsave("plots/plot_jackknife_country.png", plot_jackknife_country,
       width = 11, height = 7, dpi = 200)

### Figure 2: Year jackknife distribution
### Shared sort across both panels: by sum of |dQ| over ME(t=5) and ME(t=25).
n_y <- length(unique(df_year$unit))
df_year_total <- df_year %>%
  group_by(unit) %>%
  summarise(total_abs_dQ = sum(abs(Q_loo - Q_full)), .groups = "drop")
plot_jackknife_year <-
  df_year %>%
  left_join(df_year_total, by = "unit") %>%
  ggplot(aes(Q_loo, reorder(factor(unit), total_abs_dQ), color = flag)) +
  geom_vline(data = df_full, aes(xintercept = Q_full), color = "blue") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  geom_segment(aes(x = Q_full, xend = Q_loo,
                   yend = reorder(factor(unit), total_abs_dQ)),
               color = "gray70", linewidth = 0.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "gray25"), guide = "none") +
  facet_wrap(~ qoi, scales = "free_x") +
  labs(
    x = "Marginal effect with year i removed",
    y = "Year removed (sorted by total |dQ| across both MEs)",
    title = "Year-level jackknife: leave-one-out marginal effect",
    subtitle = sprintf("Blue: full-sample estimate. Red: |DFBETAS| > 2/sqrt(%d). All %d years shown.",
                       n_y, n_y)
  ) +
  theme(plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 10, color = "gray30"))
ggsave("plots/plot_jackknife_year.png", plot_jackknife_year,
       width = 11, height = 8, dpi = 200)

### Figure 3: SE comparison (analytic vs jackknife-country vs jackknife-year)
V <- vcov(mdl1A)
se_ME <- function(Tstar) sd_withinRegionTx5d * sqrt(
  V["tx5d","tx5d"] + Tstar^2 * V["t:tx5d","t:tx5d"] + 2 * Tstar * V["tx5d","t:tx5d"]
)
df_se <- df_full %>%
  mutate(SE_analytic = c(se_ME(5), se_ME(25))) %>%
  left_join(df_country %>% distinct(qoi, SE_jack) %>% rename(SE_country = SE_jack), by = "qoi") %>%
  left_join(df_year    %>% distinct(qoi, SE_jack) %>% rename(SE_year    = SE_jack), by = "qoi")

plot_jackknife_SE <-
  df_se %>%
  pivot_longer(c(SE_analytic, SE_country, SE_year),
               names_to = "method", values_to = "SE") %>%
  mutate(
    method = recode(method,
                    SE_analytic = "Analytic (C&M)",
                    SE_country  = "Jackknife (drop-1 country)",
                    SE_year     = "Jackknife (drop-1 year)"),
    method = factor(method, levels = c("Analytic (C&M)",
                                       "Jackknife (drop-1 country)",
                                       "Jackknife (drop-1 year)")),
    lo = Q_full - 1.96 * SE,
    hi = Q_full + 1.96 * SE
  ) %>%
  ggplot(aes(Q_full, method, color = method)) +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.9) +
  geom_point(size = 3) +
  geom_text(aes(x = hi, label = sprintf("SE=%.3f", SE)),
            hjust = -0.1, size = 3.3, color = "gray20") +
  scale_color_manual(values = c("#1f77b4", "#d62728", "#2ca02c"), guide = "none") +
  facet_wrap(~ qoi, scales = "free_x", ncol = 1) +
  labs(
    x = "Marginal effect (point estimate \u00B1 1.96\u00D7SE)",
    y = NULL,
    title = "Are the original SEs understated?  Analytic vs jackknife 95% CIs",
    subtitle = "Same point estimate; intervals use the SE from each method."
  ) +
  theme(plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 10, color = "gray30"),
        plot.margin = margin(5, 60, 5, 5))
ggsave("plots/plot_jackknife_SE.png", plot_jackknife_SE,
       width = 10, height = 5, dpi = 200)

### Figure 4: Top-20 most influential countries (bar plot of dME)
### Shared sort across both panels: by sum of |dQ| over ME(t=5) and ME(t=25).
df_country_total <- df_country %>%
  mutate(dQ = Q_loo - Q_full) %>%
  group_by(unit) %>%
  summarise(total_abs_dQ = sum(abs(dQ)), .groups = "drop")
top20_country <- df_country_total %>%
  slice_max(total_abs_dQ, n = 20, with_ties = FALSE) %>% pull(unit)
plot_jackknife_country_top20 <-
  df_country %>%
  mutate(dQ = Q_loo - Q_full) %>%
  filter(unit %in% top20_country) %>%
  left_join(df_country_total, by = "unit") %>%
  ggplot(aes(reorder(unit, total_abs_dQ), dQ)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ qoi, scales = "free_x") +
  labs(x = "Country (sorted by total |dQ| across both MEs)",
       y = "Change in marginal effect when country removed",
       title = "Top-20 most influential countries (jackknife)")
ggsave("plots/plot_jackknife_country_top20.png", plot_jackknife_country_top20,
       width = 11, height = 6, dpi = 200)

### Figure 5: All years (bar plot of dME)
plot_jackknife_year_bars <-
  df_year %>%
  mutate(dQ = Q_loo - Q_full) %>%
  ggplot(aes(factor(unit), dQ)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ qoi, scales = "free_y", ncol = 1) +
  labs(x = "Year removed", y = "Change in marginal effect",
       title = "Year-level jackknife influence (all years)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/plot_jackknife_year_bars.png", plot_jackknife_year_bars,
       width = 11, height = 7, dpi = 200)

#### Jackknife plots in Z-score units of C&M analytic SE ####

df_se_analytic <- tibble(
  qoi = factor(c("ME(t=5)", "ME(t=25)"), levels = c("ME(t=5)", "ME(t=25)")),
  SE_analytic = c(se_ME(5), se_ME(25))
)

df_country_z <- df_country %>%
  left_join(df_se_analytic, by = "qoi") %>%
  mutate(dQ_z = (Q_loo - Q_full) / SE_analytic,
         reverses_sig = case_when(
           qoi == "ME(t=5)"  ~ Q_loo - 1.96 * SE_analytic <= 0,
           qoi == "ME(t=25)" ~ Q_loo + 1.96 * SE_analytic >= 0
         ))

df_year_z <- df_year %>%
  left_join(df_se_analytic, by = "qoi") %>%
  mutate(dQ_z = (Q_loo - Q_full) / SE_analytic,
         reverses_sig = case_when(
           qoi == "ME(t=5)"  ~ Q_loo - 1.96 * SE_analytic <= 0,
           qoi == "ME(t=25)" ~ Q_loo + 1.96 * SE_analytic >= 0
         ))

### Figure 1z: Country rank dot plot (Z-scores)
plot_jackknife_country_z <-
  df_country_z %>%
  group_by(qoi) %>% mutate(rk = rank(dQ_z, ties.method = "first")) %>% ungroup() %>%
  ggplot(aes(dQ_z, rk, color = flag)) +
  geom_vline(xintercept = 0, color = "blue") +
  geom_vline(xintercept = c(-1, 1), color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = c(-2, 2), color = "gray60", linetype = "dotted") +
  geom_point(size = 1.2) +
  geom_text(aes(label = ifelse(flag, as.character(unit), NA)),
            hjust = -0.2, size = 3, color = "red", na.rm = TRUE) +
  scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "gray40"), guide = "none") +
  facet_wrap(~ qoi, scales = "free_x") +
  labs(
    x = "Shift in ME (Z-scores of C&M analytic SE)",
    y = paste0("Country (rank, 1..", n_c, ")"),
    title = "Country jackknife: LOO shift in Z-scores of original SE",
    subtitle = sprintf("Blue: full-sample. Dashed: ±1 SE. Dotted: ±2 SE. Red: |DFBETAS| > 2/√%d.", n_c)
  ) +
  theme(plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 10, color = "gray30"))
ggsave("plots/plot_jackknife_country_z.png", plot_jackknife_country_z,
       width = 11, height = 7, dpi = 200)

### Figure 2z: Year rank dot plot (Z-scores)
df_year_z_total <- df_year_z %>%
  group_by(unit) %>%
  summarise(total_abs_dQ_z = sum(abs(dQ_z)), .groups = "drop")
plot_jackknife_year_z <-
  df_year_z %>%
  left_join(df_year_z_total, by = "unit") %>%
  ggplot(aes(dQ_z, reorder(factor(unit), total_abs_dQ_z), color = flag)) +
  geom_vline(xintercept = 0, color = "blue") +
  geom_vline(xintercept = c(-1, 1), color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = c(-2, 2), color = "gray60", linetype = "dotted") +
  geom_segment(aes(x = 0, xend = dQ_z,
                   yend = reorder(factor(unit), total_abs_dQ_z)),
               color = "gray70", linewidth = 0.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "gray25"), guide = "none") +
  facet_wrap(~ qoi, scales = "free_x") +
  labs(
    x = "Shift in ME (Z-scores of C&M analytic SE)",
    y = "Year removed (sorted by total |Z| across both MEs)",
    title = "Year jackknife: LOO shift in Z-scores of original SE",
    subtitle = sprintf("Blue: full-sample. Dashed: ±1 SE. Dotted: ±2 SE. Red: |DFBETAS| > 2/√%d.", n_y)
  ) +
  theme(plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 10, color = "gray30"))
ggsave("plots/plot_jackknife_year_z.png", plot_jackknife_year_z,
       width = 11, height = 8, dpi = 200)

### Figure 4z: Top-20 countries bar plot (Z-scores)
df_country_z_total <- df_country_z %>%
  group_by(unit) %>%
  summarise(total_abs_dQ_z = sum(abs(dQ_z)), .groups = "drop")
top20_country_z <- df_country_z_total %>%
  slice_max(total_abs_dQ_z, n = 20, with_ties = FALSE) %>% pull(unit)
plot_jackknife_country_top20_z <-
  df_country_z %>%
  filter(unit %in% top20_country_z) %>%
  left_join(df_country_z_total, by = "unit") %>%
  ggplot(aes(reorder(unit, total_abs_dQ_z), dQ_z)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "gray60") +
  facet_wrap(~ qoi, scales = "free_x") +
  labs(x = "Country (sorted by total |Z| across both MEs)",
       y = "Shift in ME (Z-scores of C&M analytic SE)",
       title = "Top-20 most influential countries (Z-scores of original SE)")
ggsave("plots/plot_jackknife_country_top20_z.png", plot_jackknife_country_top20_z,
       width = 11, height = 6, dpi = 300)
ggsave("plots/plot_jackknife_country_top20_z.jpeg", plot_jackknife_country_top20_z,
       width = 11, height = 6, dpi = 300)

### Figure 5z: All years bar plot (Z-scores)
plot_jackknife_year_bars_z <-
  df_year_z %>%
  ggplot(aes(factor(unit), dQ_z)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "gray60") +
  facet_wrap(~ qoi, scales = "free_y", ncol = 1) +
  labs(x = "Year removed",
       y = "Shift in ME (Z-scores of C&M analytic SE)",
       title = "Year-level jackknife influence in Z-scores of original SE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/plot_jackknife_year_bars_z.png", plot_jackknife_year_bars_z,
       width = 11, height = 7, dpi = 300)
ggsave("plots/plot_jackknife_year_bars_z.jpeg", plot_jackknife_year_bars_z,
       width = 11, height = 7, dpi = 300)


