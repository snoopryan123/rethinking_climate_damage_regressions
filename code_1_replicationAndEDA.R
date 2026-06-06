
source("code_0_header.R")

#### Replicate their regression ####

## model 1: original
### see: Table S1 on page 7 of the Supplementary Materials of the 2022 paper
mdl_paper <- 
  felm(
    as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time | 0 | region"),
    data=dat
  )
summary(mdl_paper)
# getfe(mdl_paper)

## model 2: lag growth instead of year FE
dat_lag <- dat %>% drop_na(growth_lag1)
mdl_lagGrowth <-
  felm(
    as.formula("growth ~ growth_lag1 + t + t2 + tx5d + tx5d:t + var + var:seas + p | region | 0 | region"),
    data=dat_lag
  )
summary(mdl_lagGrowth)

#### Marginal effects of Tx5d (felm-based, standardized to p.p. per s.d.) ####
### Sanity-check the Bayesian lag-growth posterior against the felm point estimate.
ME_paper_5  = compute_ME(mdl_paper,     "t", "tx5d", Tstar=5)
ME_paper_25 = compute_ME(mdl_paper,     "t", "tx5d", Tstar=25)
ME_lag_5    = compute_ME(mdl_lagGrowth, "t", "tx5d", Tstar=5)
ME_lag_25   = compute_ME(mdl_lagGrowth, "t", "tx5d", Tstar=25)

results_ME_felm = tibble(
  model = c("C&M (year FE)", "C&M (year FE)", "lag growth", "lag growth"),
  T     = c(5, 25, 5, 25),
  ME    = signif(c(ME_paper_5$estimate, ME_paper_25$estimate, ME_lag_5$estimate, ME_lag_25$estimate), 4),
  SE    = signif(c(ME_paper_5$se,       ME_paper_25$se,       ME_lag_5$se,       ME_lag_25$se),       4),
  ci_lo = signif(ME - 1.96*SE, 4),
  ci_hi = signif(ME + 1.96*SE, 4)
)
results_ME_felm
write_csv(results_ME_felm, "plots/ME_felm_paperVsLagGrowth.csv")

#### In-sample predictiveness (R^2) with versus without the climate variables ####

### model with climate variables
lm_withClimateVars <- 
  lm(
    as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p + region + time"),
    data=dat
  )
# summary(lm_withClimateVars)

### model without climate variables
lm_woClimateVars <- 
  lm(
    as.formula("growth ~ 1 + region + time"),
    data=dat
  )
# summary(lm_woClimateVars)

### calculate change in R squared
summary_lm_withClimateVars = summary(lm_withClimateVars)
summary_lm_woClimateVars = summary(lm_woClimateVars)

r.sq.withClimateVars = summary_lm_withClimateVars$r.squared
r.sq.paper.woClimateVars = summary_lm_woClimateVars$r.squared

r.sq.pd = (r.sq.withClimateVars - r.sq.paper.woClimateVars) / r.sq.paper.woClimateVars
scales::percent(r.sq.pd)

r.sq.pd = (r.sq.withClimateVars - r.sq.paper.woClimateVars) / r.sq.paper.woClimateVars
scales::percent(r.sq.pd)

results = tibble(
  desc = c(
    "R^2 with climate vars (C&M model, year FE)",
    "R^2 without climate vars (C&M model, year FE)",
    "percent difference (C&M model, year FE)"
  ),
  val = c(
    signif(r.sq.withClimateVars, 5),
    signif(r.sq.paper.woClimateVars, 5),
    signif(r.sq.pd, 5)
  )
)
results
write_csv(results, "plots/R2_InSamplePredictiveComparison.csv")

### get the change in R squared between including and not including climate variables from FELM
felm_withClimateVars <- 
  felm(
    as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time | 0 | region"),
    data=dat
  )

summary_lm_withClimateVars = summary(lm_withClimateVars)
summary_lm_withClimateVars$P.r.squared
summary_lm_withClimateVars$P.adj.r.squared

### lag-growth model: R^2 with vs without climate variables
lm_lagGrowth_withClimate <-
  lm(
    as.formula("growth ~ growth_lag1 + t + t2 + tx5d + tx5d:t + var + var:seas + p + region"),
    data=dat_lag
  )
lm_lagGrowth_woClimate <-
  lm(
    as.formula("growth ~ growth_lag1 + region"),
    data=dat_lag
  )

r.sq.lagGrowth.withClimate = summary(lm_lagGrowth_withClimate)$r.squared
r.sq.lagGrowth.woClimate = summary(lm_lagGrowth_woClimate)$r.squared
r.sq.lagGrowth.pd = (r.sq.lagGrowth.withClimate - r.sq.lagGrowth.woClimate) / r.sq.lagGrowth.woClimate

results_lagGrowth = tibble(
  desc = c(
    "R^2 with climate vars (lag growth model)",
    "R^2 without climate vars (lag growth model)",
    "percent difference"
  ),
  val = c(
    signif(r.sq.lagGrowth.withClimate, 5),
    signif(r.sq.lagGrowth.woClimate, 5),
    signif(r.sq.lagGrowth.pd, 5)
  )
)
results_lagGrowth
write_csv(results_lagGrowth, "plots/R2_InSamplePredictiveComparison_lagGrowth.csv")

### cross-model comparison: lag growth vs year FE
results_crossModel = tibble(
  desc = c(
    "R^2 with climate vars (C&M model, year FE)",
    "R^2 with climate vars (lag growth model)",
    "pct diff (lag growth vs year FE, with climate)",
    "R^2 without climate vars (C&M model, year FE)",
    "R^2 without climate vars (lag growth model)",
    "pct diff (lag growth vs year FE, without climate)"
  ),
  val = c(
    signif(r.sq.withClimateVars, 5),
    signif(r.sq.lagGrowth.withClimate, 5),
    signif((r.sq.lagGrowth.withClimate - r.sq.withClimateVars) / r.sq.withClimateVars * 100, 5),
    signif(r.sq.paper.woClimateVars, 5),
    signif(r.sq.lagGrowth.woClimate, 5),
    signif((r.sq.lagGrowth.woClimate - r.sq.paper.woClimateVars) / r.sq.paper.woClimateVars * 100, 5)
  )
)
results_crossModel
write_csv(results_crossModel, "plots/R2_crossModelComparison.csv")

### R^2 after removing outliers (1998, 2009, MNG, KEN)
dat_ro <- dat %>% filter(!(time %in% c(1998, 2009)), !(iso %in% c("MNG", "KEN")))
dat_lag_ro <- dat_ro %>% drop_na(growth_lag1)

# C&M model without outliers
lm_ro_with <- lm(growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p + region + time, data = dat_ro)
lm_ro_wo   <- lm(growth ~ 1 + region + time, data = dat_ro)
r2_ro_with <- summary(lm_ro_with)$r.squared
r2_ro_wo   <- summary(lm_ro_wo)$r.squared
r2_ro_pd   <- (r2_ro_with - r2_ro_wo) / r2_ro_wo * 100

# lag growth model without outliers
lm_ro_lag_with <- lm(growth ~ growth_lag1 + t + t2 + tx5d + tx5d:t + var + var:seas + p + region, data = dat_lag_ro)
lm_ro_lag_wo   <- lm(growth ~ growth_lag1 + region, data = dat_lag_ro)
r2_ro_lag_with <- summary(lm_ro_lag_with)$r.squared
r2_ro_lag_wo   <- summary(lm_ro_lag_wo)$r.squared
r2_ro_lag_pd   <- (r2_ro_lag_with - r2_ro_lag_wo) / r2_ro_lag_wo * 100

results_removeOutliers = tibble(
  desc = c(
    "R^2 with climate vars (C&M model, no outliers)",
    "R^2 without climate vars (C&M model, no outliers)",
    "pct diff (C&M model, no outliers)",
    "R^2 with climate vars (lag growth, no outliers)",
    "R^2 without climate vars (lag growth, no outliers)",
    "pct diff (lag growth, no outliers)",
    "pct diff (lag growth vs year FE, with climate, no outliers)",
    "pct diff (lag growth vs year FE, without climate, no outliers)"
  ),
  val = c(
    signif(r2_ro_with, 5),
    signif(r2_ro_wo, 5),
    signif(r2_ro_pd, 5),
    signif(r2_ro_lag_with, 5),
    signif(r2_ro_lag_wo, 5),
    signif(r2_ro_lag_pd, 5),
    signif((r2_ro_lag_with - r2_ro_with) / r2_ro_with * 100, 5),
    signif((r2_ro_lag_wo   - r2_ro_wo)   / r2_ro_wo   * 100, 5)
  )
)
results_removeOutliers
write_csv(results_removeOutliers, "plots/R2_removeOutliers.csv")

#### Data summary ####

### Initial dataset: panel_in
panel_in_1

panel_in_summary = 
  panel_in_1 %>%
  select(
    growth, t, tx5d, var, seas, p, time, region, iso, subcontinent, continent
  ) %>%
  as_tibble()
panel_in_summary

dim(panel_in_summary)

sum(is.na(panel_in_summary$growth))

length(unique(panel_in_1$iso))
length(unique(panel_in_1$region))
panel_in_1 %>% group_by(iso) %>% reframe(n=length(unique(region))) %>% reframe(mean(n), median(n))

### Remove NAs in growth column: panel
panel

panel_summary = 
  panel %>%
  select(
    growth, t, tx5d, var, seas, p, time, region, iso, subcontinent, continent
  ) %>%
  as_tibble()
panel_summary

dim(panel_summary)

sapply(panel_summary, function(x) {
  c(NAs = sum(is.na(x)), pct = mean(is.na(x)))
})

length(unique(panel$iso))
length(unique(panel$region))
panel %>% group_by(iso) %>% reframe(n=length(unique(region))) %>% reframe(mean(n), median(n))

### which countries were removed?
table(panel_in$iso)

table(panel$iso)

table(setdiff(panel_in$iso, panel$iso))

length(table(setdiff(panel_in$iso, panel$iso)))

d0 = panel_in_1 %>% distinct(iso, subcontinent, continent) %>% as_tibble()
d1 = panel %>% distinct(iso, subcontinent, continent) 
d0
d1
# d0 %>% filter(!(iso %in% d1$iso)) %>% group_by(continent) %>% reframe(n())
### the subcontinents of countries removed bc they had NA growth
d0 %>% filter(!(iso %in% d1$iso)) %>% group_by(subcontinent) %>% reframe(n=n()) %>% arrange(-n)


###     as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time | 0 | region"),
dat_og_summary = 
  dat %>%
  select(
    growth, t, t2, tx5d, var, seas, p, time, region, iso
  )
dat_og_summary

dim(dat_og_summary)

sapply(dat_og_summary, function(x) {
  c(NAs = sum(is.na(x)), pct = mean(is.na(x)))
})

dat_og_summary_1 = dat_og_summary %>% drop_na()
dat_og_summary_1

sum(is.na(dat_og_summary_1))
dim(dat_og_summary_1)


#### EDA: Growth, t, tx5d are correlated within each country and across time ####

### plot growth over time by region within each country 
# View(dat %>% distinct(iso, region) %>% count(iso) %>% arrange(n))
plot_regionCorr <- 
  dat %>%
  filter(iso %in% c(
    "AUS", "CAN", "KOR", "FRA", "ARG", "CHE", "PAK", "BRA"
  )) %>%
  split(.$iso) %>%
  lapply(function(df) {
    ggplot(df, aes(x = time, y = growth, color = region)) +
      geom_line() +
      theme_minimal() +
      labs(title = unique(df$iso), x = "Time", y = "Growth", color = "Region") +
      scale_color_discrete(guide="none")
  })
plot_regionCorr = wrap_plots(plot_regionCorr, ncol = 4) +
  plot_annotation(title = "Growth Over Time by Region within each Country")
# plot_regionCorr
ggsave("plots/plot_EDA_regionCountryCorr.png", width=12, height=5, dpi=300)
ggsave("plots/plot_EDA_regionCountryCorr.jpeg", width=12, height=5, dpi=300)

### plot T over time by region within each country
plot_regionCorr_t <-
  dat %>%
  filter(iso %in% c(
    "AUS", "CAN", "KOR", "FRA", "ARG", "CHE", "PAK", "BRA"
  )) %>%
  split(.$iso) %>%
  lapply(function(df) {
    ggplot(df, aes(x = time, y = t, color = region)) +
      geom_line() +
      theme_minimal() +
      labs(title = unique(df$iso), x = "Time", y = "T", color = "Region") +
      scale_color_discrete(guide="none")
  })
plot_regionCorr_t = wrap_plots(plot_regionCorr_t, ncol = 4) +
  plot_annotation(title = "T Over Time by Region within each Country")
ggsave("plots/plot_EDA_regionCountryCorr_t.png", width=12, height=5)

### plot Tx5d over time by region within each country
plot_regionCorr_tx5d <-
  dat %>%
  filter(iso %in% c(
    "AUS", "CAN", "KOR", "FRA", "ARG", "CHE", "PAK", "BRA"
  )) %>%
  split(.$iso) %>%
  lapply(function(df) {
    ggplot(df, aes(x = time, y = tx5d, color = region)) +
      geom_line() +
      theme_minimal() +
      labs(title = unique(df$iso), x = "Time", y = "Tx5d", color = "Region") +
      scale_color_discrete(guide="none")
  })
plot_regionCorr_tx5d = wrap_plots(plot_regionCorr_tx5d, ncol = 4) +
  plot_annotation(title = "Tx5d Over Time by Region within each Country")
ggsave("plots/plot_EDA_regionCountryCorr_tx5d.png", width=12, height=5)

#### EDA: Neighboring countries correlation ####
neighbor_groups <- tribble(
  ~group,             ~iso,
  "North America",    "USA",
  "North America",    "CAN",
  "North America",    "MEX",
  "Western Europe",   "FRA",
  "Western Europe",   "DEU",
  "Western Europe",   "ESP",
  "Western Europe",   "ITA",
  "Western Europe",   "GBR",
  "Central Europe",   "CHE",
  "Central Europe",   "AUT",
  "Central Europe",   "POL",
  "Central Europe",   "CZE",
  "East Asia",        "CHN",
  "East Asia",        "JPN",
  "East Asia",        "KOR",
  "South Asia",       "IND",
  "South Asia",       "PAK",
  "South Asia",       "BGD",
  "Southern Cone",    "ARG",
  "Southern Cone",    "BRA",
  "Southern Cone",    "CHL",
  "Southern Cone",    "URY",
  "Oceania",          "AUS",
  "Oceania",          "NZL",
  "Nordics",          "SWE",
  "Nordics",          "NOR",
  "Nordics",          "FIN",
  "Nordics",          "DNK"
) %>%
  filter(iso %in% unique(dat$iso))

plot_neighbor_corr <- function(varname, ylab, title) {
  dat_grp <- dat %>%
    inner_join(neighbor_groups, by = "iso") %>%
    group_by(iso, group, time) %>%
    summarise(!!varname := mean(.data[[varname]], na.rm = TRUE), .groups = "drop")
  plots <-
    dat_grp %>%
    split(.$group) %>%
    lapply(function(df) {
      ggplot(df, aes(x = time, y = .data[[varname]], color = iso)) +
        geom_line() +
        theme_minimal() +
        labs(title = unique(df$group), x = "Time", y = ylab, color = "Country")
    })
  wrap_plots(plots, ncol = 4) + plot_annotation(title = title)
}

p_growth_neighbors = plot_neighbor_corr(
  "growth", "Growth",
  "Growth Over Time by Region within each Group of Neighboring Countries"
)
ggsave("plots/plot_EDA_regionNeighborCorr_growth.png", p_growth_neighbors, width=14, height=6)

p_t_neighbors = plot_neighbor_corr(
  "t", "Mean annual T (C)",
  "Temperature Over Time by Region within each Group of Neighboring Countries"
)
ggsave("plots/plot_EDA_regionNeighborCorr_t.png", p_t_neighbors, width=14, height=6)

p_tx5d_neighbors = plot_neighbor_corr(
  "tx5d", "tx5d",
  "Extreme Heat (tx5d) Over Time by Region within each Group of Neighboring Countries"
)
ggsave("plots/plot_EDA_regionNeighborCorr_tx5d.png", p_tx5d_neighbors, width=14, height=6)

# ### plot growth over time by country within each subcontinent 
# plot_subcontinentCorr0 <- 
#   dat %>%
#   arrange(iso, time) %>%
#   group_by(continent, subcontinent, iso, time) %>%
#   reframe(growth = mean(growth)) %>%
#   split(.$subcontinent) %>%
#   lapply(function(df) {
#     ggplot(df, aes(x = time, y = growth, color = iso)) +
#       geom_line() +
#       geom_abline(intercept=0, slope=0, color="gray60", linetype="dashed") +
#       theme_minimal() +
#       # scale_color_discrete(guide="right") +
#       labs(title = unique(df$subcontinent), x = "Time", 
#            y = "Mean Regional Growth", color = "Country") 
#   })
# plot_subcontinentCorr = 
#   wrap_plots(plot_subcontinentCorr0, nrow = 3) +
#   plot_annotation(title = "Growth Over Time by Country within each Subcontinent")
# # plot_subcontinentCorr
# ggsave("plots/plot_EDA_countrySubcontinentCorr.png", width=20, height=8)

# ### plot growth over time by subcontinent within each continent
# plot_continentCorr0 <- 
#   dat %>%
#   arrange(iso, time) %>%
#   # group_by(continent, subcontinent, iso, time) %>%
#   group_by(continent, subcontinent, time) %>%
#   reframe(growth = mean(growth)) %>%
#   split(.$continent) %>%
#   lapply(function(df) {
#     ggplot(df, aes(x = time, y = growth, color = subcontinent)) +
#       geom_line() +
#       geom_abline(intercept=0, slope=0, color="gray60", linetype="dashed") +
#       theme_minimal() +
#       # scale_color_discrete(guide="right") +
#       labs(title = unique(df$continent), x = "Time", 
#            y = "Mean Country Growth", color = "Subcontinent") 
#   })
# plot_continentCorr = 
#   wrap_plots(plot_continentCorr0, nrow = 2) +
#   plot_annotation(title = "Growth Over Time by Subcontinent within each Continent")
# # plot_continentCorr
# ggsave("plots/plot_EDA_subcontinentContinentCorr.png", width=11, height=5)

#### EDA: AR(1) Correlation Ridgeline by Country ####

plot_ar1_ridge <- function(varname, ylab) {
  df <- dat %>%
    arrange(region, time) %>%
    group_by(region) %>%
    mutate(var_lag1 = lag(.data[[varname]])) %>%
    drop_na(var_lag1) %>%
    group_by(iso, region) %>%
    filter(n() >= 15) %>%
    reframe(ar1 = cor(.data[[varname]], var_lag1)) %>%
    group_by(iso) %>%
    filter(n() >= 5) %>%
    ungroup() %>%
    mutate(iso = fct_reorder(iso, ar1, .fun = mean))
  df_means <- df %>% group_by(iso) %>% summarise(mean_ar1 = mean(ar1), .groups = "drop")
  ggplot(df, aes(x = ar1, y = iso, fill = after_stat(x))) +
    geom_density_ridges_gradient(scale = 1.2) +
    scale_fill_viridis_c(option = "magma") +
    geom_segment(data = df_means, aes(x = mean_ar1, xend = mean_ar1, y = as.numeric(iso), yend = as.numeric(iso) + 0.9),
                 inherit.aes = FALSE, color = "gray60", linewidth = 1.25) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)) +
    guides(fill = "none") +
    labs(
      x = "AR(1) Correlation",
      y = "Country",
      title = paste0("AR(1) ", ylab, " Correlation across Regions by Country")
    )
}

plot_ar1_ridge_growth = plot_ar1_ridge("growth", "Growth")
ggsave("plots/plot_EDA_ar1Ridge_growth.png", plot_ar1_ridge_growth, width=8, height=10)

plot_ar1_ridge_t = plot_ar1_ridge("t", "T")
ggsave("plots/plot_EDA_ar1Ridge_t.png", plot_ar1_ridge_t, width=8, height=10)

plot_ar1_ridge_tx5d = plot_ar1_ridge("tx5d", "Tx5d")
ggsave("plots/plot_EDA_ar1Ridge_tx5d.png", plot_ar1_ridge_tx5d, width=8, height=10)

#### EDA: Lag-k Growth Correlation Density ####

df_corLag = tibble()
for (i in 1:5) {
  df_corLag_i =
    dat %>%
    select(iso, region, time, growth) %>%
    arrange(region, time) %>%
    group_by(region) %>%
    mutate(
      growth_lag = lag(growth, n=i),
    ) %>%
    drop_na(growth_lag) %>%
    group_by(iso, region) %>%
    reframe(cor_growth_lag = cor(growth, growth_lag))
  df_corLag_i
  df_corLag_i$lag = i
  df_corLag = bind_rows(df_corLag, df_corLag_i)
}
df_corLag =
  df_corLag %>%
  group_by(iso,lag) %>%
  mutate(med_cor = median(cor_growth_lag)) %>%
  ungroup()
df_corLag

plot_timeCorLag =
  df_corLag %>%
  filter(lag <= 3) %>%
  group_by(lag) %>%
  mutate(med_cor_by_lag = median(cor_growth_lag, na.rm=T)) %>%
  ungroup() %>%
  ggplot(aes(
    x = cor_growth_lag,
    fill = factor(lag),
    color = factor(lag),
  )) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  geom_density(alpha=0.25) +
  labs(
    # x = "Cor{Growth (Current Year), Growth (Previous Year)}",
    x = "Cor",
    color = "k", fill = "k", y = "Density",
    title = "Density of Lag-k Growth Correlation across Regions"
  ) +
  theme_minimal() +
  geom_vline(linewidth=1, aes(color = factor(lag),xintercept = med_cor_by_lag)) +
  geom_vline(xintercept = 0, linetype="dashed", linewidth=1, color="gray40")
# plot_timeCorLag
ggsave("plots/plot_EDA_timeCorLagDensity.png",
       wrap_plots(plot_timeCorLag), width=6, height=4)

#### EDA: Rolling correlation between growth, T, tx5d over time ####

countries_for_plots <- c("AUS", "CAN", "KOR", "FRA", "ARG", "DEU", "PAK", "BRA")
roll_window <- 5

plot_rolling_cor <- function(var1, var2, ylab1, ylab2) {
  dat %>%
    filter(iso %in% countries_for_plots) %>%
    arrange(region, time) %>%
    group_by(region, iso) %>%
    mutate(roll_cor = zoo::rollapply(
      cbind(.data[[var1]], .data[[var2]]),
      width = roll_window, FUN = function(x) cor(x[,1], x[,2]),
      fill = NA, align = "center", by.column = FALSE
    )) %>%
    drop_na(roll_cor) %>%
    ungroup() %>%
    ggplot(aes(x = time, y = roll_cor, color = region)) +
    geom_line(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    facet_wrap(~ iso) +
    theme_bw() +
    guides(color = "none") +
    labs(x = "Time", y = paste0(roll_window, "-Year Rolling Cor"),
         title = paste0("Rolling Correlation: ", ylab1, " vs ", ylab2, " by Region"))
}

plot_rollcor_growth_t = plot_rolling_cor("growth", "t", "Growth", "T")
ggsave("plots/plot_EDA_rollCor_growth_t.png", plot_rollcor_growth_t, width=12, height=8)

plot_rollcor_growth_tx5d = plot_rolling_cor("growth", "tx5d", "Growth", "Tx5d")
ggsave("plots/plot_EDA_rollCor_growth_tx5d.png", plot_rollcor_growth_tx5d, width=12, height=8)

plot_rollcor_t_tx5d = plot_rolling_cor("t", "tx5d", "T", "Tx5d")
ggsave("plots/plot_EDA_rollCor_t_tx5d.png", plot_rollcor_t_tx5d, width=12, height=8)

#### EDA: Distribution of regional correlations by country (ridgeline) ####

plot_cor_ridge_by_country <- function(var1, var2, ylab1, ylab2) {
  df <- dat %>%
    group_by(iso, region) %>%
    filter(n() >= 15) %>%
    reframe(r = cor(.data[[var1]], .data[[var2]], use = "complete.obs")) %>%
    group_by(iso) %>%
    filter(n() >= 5) %>%
    ungroup() %>%
    mutate(iso = fct_reorder(iso, r, .fun = mean))
  df_means <- df %>% group_by(iso) %>% summarise(mean_r = mean(r), .groups = "drop")
  ggplot(df, aes(x = r, y = iso, fill = after_stat(x))) +
    geom_density_ridges_gradient(scale = 1.2) +
    scale_fill_viridis_c(option = "magma") +
    geom_segment(data = df_means, aes(x = mean_r, xend = mean_r, y = as.numeric(iso), yend = as.numeric(iso) + 0.9),
                 inherit.aes = FALSE, color = "gray60", linewidth = 1.25) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)) +
    guides(fill = "none") +
    labs(x = paste0("Cor(", ylab1, ", ", ylab2, ")"),
         y = "Country",
         title = paste0("Regional ", ylab1, "-", ylab2, " Correlation by Country"))
}

plot_cor_ridge_growth_t = plot_cor_ridge_by_country("growth", "t", "Growth", "T")
ggsave("plots/plot_EDA_corRidge_growth_t.png", plot_cor_ridge_growth_t, width=8, height=10)

plot_cor_ridge_growth_tx5d = plot_cor_ridge_by_country("growth", "tx5d", "Growth", "Tx5d")
ggsave("plots/plot_EDA_corRidge_growth_tx5d.png", plot_cor_ridge_growth_tx5d, width=8, height=10)

plot_cor_ridge_t_tx5d = plot_cor_ridge_by_country("t", "tx5d", "T", "Tx5d")
ggsave("plots/plot_EDA_corRidge_t_tx5d.png", plot_cor_ridge_t_tx5d, width=8, height=10)

#### EDA: Cor(global growth, regional T/tx5d) across regions ####

dat_global_growth <- dat %>%
  group_by(time) %>%
  summarise(global_growth = mean(growth, na.rm=TRUE), .groups="drop")

df_cor_global <- bind_rows(
  dat %>%
    left_join(dat_global_growth, by = "time") %>%
    group_by(region, iso) %>%
    filter(n() >= 25) %>%
    reframe(r = cor(global_growth, t, use = "complete.obs")) %>%
    mutate(var = "T"),
  dat %>%
    left_join(dat_global_growth, by = "time") %>%
    group_by(region, iso) %>%
    filter(n() >= 25) %>%
    reframe(r = cor(global_growth, tx5d, use = "complete.obs")) %>%
    mutate(var = "Tx5d")
)

df_cor_global_means <- df_cor_global %>% group_by(var) %>% summarise(mean_r = mean(r, na.rm=TRUE), .groups="drop")

plot_cor_global_growth_climate <-
  df_cor_global %>%
  ggplot(aes(x = r)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(data = df_cor_global_means, aes(xintercept = mean_r), color = "red", linewidth = 0.8) +
  facet_wrap(~ var) +
  theme_bw() +
  labs(x = "Cor(Global Growth, Regional Climate Variable)",
       y = "Number of Regions",
       title = "Correlation between Global Mean Growth and Regional T / Tx5d")
ggsave("plots/plot_EDA_corGlobalGrowthClimate.png", plot_cor_global_growth_climate, width=10, height=5)

#### SANDBOX ####


