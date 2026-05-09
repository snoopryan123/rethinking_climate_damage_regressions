
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
    "R^2 with climate vars",
    "R^2 without climate vars",
    "percent difference"
  ),
  val = c(
    round(r.sq.withClimateVars,4),
    round(r.sq.paper.woClimateVars,4),
    scales::percent(r.sq.pd)
  )
)
results

gt::gtsave(
  gt::gt(results)
  , "plots/plot_R^2InSamplePredictiveComparison.png"
)

### get the change in R squared between including and not including climate variables from FELM
felm_withClimateVars <- 
  felm(
    as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time | 0 | region"),
    data=dat
  )

summary_lm_withClimateVars = summary(lm_withClimateVars)
summary_lm_withClimateVars$P.r.squared
summary_lm_withClimateVars$P.adj.r.squared

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
ggsave("plots/plot_EDA_regionCountryCorr.png", width=12, height=5)

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
  dat %>%
    arrange(region, time) %>%
    group_by(region) %>%
    mutate(var_lag1 = lag(.data[[varname]])) %>%
    drop_na(var_lag1) %>%
    group_by(iso, region) %>%
    reframe(ar1 = cor(.data[[varname]], var_lag1)) %>%
    mutate(iso = fct_reorder(iso, ar1, .fun = mean)) %>%
    ggplot(aes(x = ar1, y = iso, fill = iso)) +
    geom_density_ridges(alpha = 0.5, scale = 1.2) +
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

#### SANDBOX ####


