
source("code_0_header.R")

#####################
### Leave one country out analysis
####################

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

#####################
### Leave one year out analysis
####################

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

################ 
### Leave one out final effect size changes
################

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

### what happens when removing these outliers
extreme_countries = c("MNG","KEN")
extreme_years = c(1998, 2009)

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
extreme_countries
extreme_years
ME_og
ME_withoutOutliers
scales::percent( (ME_tx5d_t5_withoutOutliers - ME_tx5d_t5_og) / abs(ME_tx5d_t5_og) )
scales::percent( (ME_tx5d_t25_withoutOutliers - ME_tx5d_t25_og) / abs(ME_tx5d_t25_og) )
  
################ 
#### Diagnostics & Outliers
################

## model 1: original
mdl1A <- 
  feols(
    as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time"),
    data=dat1
  )
summary(mdl1A)

### plot regression diagnostics and remove outliers
plot_diagnostics <- function(dat, model, outlier_abs_thresh=3, remove_outliers=F, ) {
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

##################


