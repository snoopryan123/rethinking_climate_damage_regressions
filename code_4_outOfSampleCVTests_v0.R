
source("code_0_header.R")

#####################
### Cross validation: randomly split the rows into K folds
#####################

CV_TYPE = "IID"
# CV_TYPE = "TIMEBLOCKS"

### models to test
{
  formula_lst_TIMEBLOCKS = 
    list(
      c("growth ~ 1", "overall mean", "tag", "FE"),
      c("growth ~ 1 | country", "country FE", "", "FE"),
      c("growth ~ 1 + (1 | country)", "country RE", "", "RE"),
      c("growth ~ 1 | region", "region FE", "", "FE"),
      c("growth ~ 1 + (1 | region)", "region RE", "", "RE"),
      c("growth ~ 1 + (1 | country/region)", "country/region hierarchical REs", "", "RE"),
      # c("growth ~ 1 + (1 | continent/subcontinent/country/region)", "spatial hierarchy REs", "", "RE"),
      c("growth ~ 1 + (1 | time)", "time RE", "tag", "RE"),
      c("growth ~ 1 + (1 | time) + (1 | country/region)", "time RE & country/region hierarchical REs", "", "RE"),
      c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p", "climate vars", "tag", "FE"),
      c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p + (1 | time)", "climate vars & time RE", "", "RE"),
      c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p + (1 | time) + (1 | country/region)", "climate vars & time RE & country/region hierarchical REs", "tag", "RE"),
      c("growth ~ growth_lag1", "lag(growth,1)", "", "FE"),
      c("growth ~ growth_lag1 + (1 | time)", "lag(growth,1) & time RE", "", "RE")
    )
  formula_lst_TIMEBLOCKS
  
  formula_lst_IID = 
    list(
      c("growth ~ 1", "overall mean", "tag", "FE"),
      c("growth ~ 1 | country", "country FE", "", "FE"),
      c("growth ~ 1 + (1 | country)", "country RE", "", "RE"),
      c("growth ~ 1 | region", "region FE", "", "FE"),
      c("growth ~ 1 + (1 | region)", "region RE", "", "RE"),
      c("growth ~ 1 | country + region", "country/region FEs", "", "FE"),
      c("growth ~ 1 + (1 | country/region)", "country/region hierarchical REs", "", "RE"),
      # c("growth ~ 1 + (1 | continent/subcontinent/country/region)", "spatial hierarchy REs", "", "RE"),
      c("growth ~ 1 | time", "time FE", "", "FE"),
      c("growth ~ 1 + (1 | time)", "time RE", "", "RE"),
      c("growth ~ 1 | region + time", "region/time FEs", "", "FE"),
      c("growth ~ 1 | country + time", "country/time FEs", "", "FE"),
      c("growth ~ 1 + (1 | time) + (1 | country/region)", "time RE & country/region hierarchical REs", "tag", "RE"),
      # 
      c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p", "climate vars", "tag", "FE"),
      c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p + (1 | time)", "climate vars & time RE", "", "RE"),
      c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time", "paper: climate vars & region/time FEs", "tag", "FE"),
      c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p + (1 | time) + (1 | country/region)", "climate vars & time RE & country/region hierarchical REs", "tag", "RE"),
      c("growth ~ growth_lag1", "lag(growth,1)", "", "FE"),
      c("growth ~ growth_lag1 + (1 | time)", "lag(growth,1) & time RE", "", "RE")
    )
  formula_lst_IID
  
  # formula_lst_IID = 
  #   list(
  #     c("growth ~ 1", "baseline", "tag"),
  #     c("growth ~ 1 | iso + time", "country_v1", ""),
  #     c("growth ~ 1 | iso", "country_v2a", ""),
  #     # c("growth ~ 1 | iso_time", "country_v2", ""),
  #     c("growth ~ 1 | iso_block2", "country_v3", ""),
  #     c("growth ~ 1 | iso_block3", "country_v4", ""),
  #     c("growth ~ 1 | iso_block", "country_v5", ""),
  #     c("growth ~ 1 | region + time", "region_v1", "tag"),
  #     c("growth ~ 1 | region_block2", "region_v2", ""),
  #     c("growth ~ 1 | region_block3", "region_v3", ""),
  #     c("growth ~ 1 | region_block", "region_v4", ""),
  #     c("growth ~ pop_in_mil", "pop_v1", ""),
  #     c("growth ~ pop_in_mil | region + time", "pop_v2", ""),
  #     # c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | iso_time", "temp_countryV2", ""),
  #     # c("growth ~ pop_in_mil + t + t2 + tx5d + tx5d:t + var + var:seas + p | iso_time", "temp_countryV2_pop", ""),
  #     c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time", "temp_paper_OG", "tag"),
  #     c("growth ~ pop_in_mil + t + t2 + tx5d + tx5d:t + var + var:seas + p | region + time", "temp_pop", "")
  #     
  #     # c("growth ~ 1", "overall mean", "tag"),
  #     # c("growth ~ 1 | country", "country FE", ""),
  #     # c("growth ~ 1 | region", "region FE", ""),
  #     # c("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p", "temp vars", ""),
  #     # c("growth ~ growth_lag1", "lag(growth,1)", ""),
  #     # c("growth ~ growth_lag1 + country", "lag(growth,1) & country FE", "")
  #     
  #     # c("growth ~ l(growth, 1)", "l", "")
  #     # c("growth ~ 1 | time", "time FE", ""),
  #     # c("growth ~ 1 | country + time", "country/time FEs", ""),
  #     # c("growth ~ 1 | region + time", "region/time FEs", "")
  #   )
  # formula_lst_IID
}

if (CV_TYPE == "IID") {
  dat1$fold = dat1$iid_fold
  plot_title = paste0("I.I.D. ", nfolds,"-fold cross validation")
  plot_name = "plots/plot_cv_iid.png"
  formula_lst = formula_lst_IID
} else if (CV_TYPE == "TIMEBLOCKS") {
  dat1$fold = dat1$blk
  plot_title = paste0("Cross validation with ", num_time_blocks," \n~",
                      round(length(table(dat1$time))/num_time_blocks),
                      "-contiguous-year holdout blocks")
  plot_name = "plots/plot_cv_yearBlocks.png"
  formula_lst = formula_lst_TIMEBLOCKS
} else {
  stop()
}

###
formula_tib <- 
  tibble(
    formula = sapply(formula_lst, `[[`, 1),
    label  = sapply(formula_lst, `[[`, 2),
    tag  = sapply(formula_lst, `[[`, 3),
    alg  = sapply(formula_lst, `[[`, 4),
  ) 
formula_tib

###
df_losses = tibble()
FOLDS = sort(unique(dat1$fold))
for (FOLD in FOLDS) {
  df_train_i = dat1 %>% filter(fold!=FOLD)
  df_test_i = dat1 %>% filter(fold==FOLD)
  
  # match test factor levels to training
  # drop rows with countries unseen in training (be explicit)
  df_train_i$country <- factor(df_train_i$country)
  df_test_i$country <- factor(df_test_i$country, levels = levels(df_train_i$country))
  df_test_i <- subset(df_test_i, !is.na(country))
  
  losses_i = c()
  labels_i = c()
  tags_i = c()
  for (j in 1:nrow(formula_tib)) {
    formula_ij = formula_tib[j,]$formula
    label_ij = formula_tib[j,]$label
    tag_ij = formula_tib[j,]$tag
    alg_ij = formula_tib[j,]$alg
    
    if (alg_ij == "FE") {
      m_ij = feols(as.formula(formula_ij), data=df_train_i)
      m_ij
      pred_ij = predict(m_ij, df_test_i)
    } else if (alg_ij == "RE") {
      m_ij = lmer(as.formula(formula_ij), data=df_train_i)
      m_ij
      
      # ### population-level prediction (ignore RE)
      # pred_ij  <- predict(m_ij, newdata = df_test_i, re.form = NA)
      
      ### include RE where available; new levels shrink to 0
      pred_ij <- predict(m_ij, newdata = df_test_i, allow.new.levels = TRUE) 
    } else {
      stop(paste0("alg_ij = ", alg_ij, " is not supported."))
    }
    
    rmse_ij = RMSE(df_test_i$growth, pred_ij, na.rm = T)
    losses_i = c(losses_i, rmse_ij)
    labels_i = c(labels_i, label_ij)
    tags_i = c(tags_i, tag_ij)
  }
  df_losses = bind_rows(
    df_losses,
    tibble(rmse=losses_i, model=labels_i, tag=tags_i, fold=FOLD)
  )
}
df_losses$model = factor(df_losses$model, levels=formula_tib$label)
df_losses

plot_cv =
  df_losses %>%
  arrange(fold, model) %>%
  group_by(fold) %>%
  mutate(reduction_in_error = -(rmse - first(rmse))/first(rmse) ) %>%
  ungroup() %>%
  mutate(model = factor(model, levels = rev(formula_tib$label))) %>%
  ggplot(aes(x=reduction_in_error, y=model, colour = tag!="")) +
  geom_vline(xintercept=0, linetype="dashed", color="gray60", linewidth=0.5) +
  geom_boxplot() +
  labs(
    title=plot_title,
    x = "Reduction in Error (RMSE)  (higher is better)", y = "Model"
  ) +
  scale_colour_manual(
    values = c(`TRUE` = "firebrick", `FALSE` = "black"), guide  = "none"
  ) +
  theme(
    axis.text.y = element_text(size = 13),
    axis.title  = element_text(size = 15),
    plot.title  = element_text(size = 16, hjust = 0.5)
  ) +
  scale_x_continuous(labels = scales::percent)
# plot_cv
ggsave(plot_name, plot_cv, width=11, height=8)

#####################
### Wald CIs for New model & Original Model
#####################

# ## new mixed effects model
# mdl_new <- 
#   lmer(
#     as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p + (1 | time) + (1 | continent/subcontinent/country/region)"),
#     data=dat1
#   )
# mdl_new_x98 <- 
#   lmer(
#     as.formula("growth ~ t + t2 + tx5d + tx5d:t + var + var:seas + p + (1 | time) + (1 | continent/subcontinent/country/region)"),
#     data=dat1%>%filter(time!=1998)
#   )
# summary(mdl_new)
# summary(mdl_new_x98)
# summry(mdl_paper)
# 
# # Fast Wald CIs
# ci_wald_og <- broom.mixed::tidy(
#   mdl_paper, effects = "fixed",conf.int = TRUE, conf.method = "Wald"
# )
# ci_wald_new <- broom.mixed::tidy(
#   mdl_new, effects = "fixed",conf.int = TRUE, conf.method = "Wald"
# )
# ci_wald_new_x98 <- broom.mixed::tidy(
#   mdl_new_x98, effects = "fixed",conf.int = TRUE, conf.method = "Wald"
# )
# ci_wald_og$Model = "Fixed effects model\nfrom the paper\n"
# ci_wald_new$Model = "New mixed effects\nmodel\n"
# ci_wald_new_x98$Model = "New mixed effects\nmodel (remove 1998)\n"
# 
# plot_ci_wald = 
#   bind_rows(ci_wald_og, ci_wald_new, ci_wald_new_x98)%>%
#   filter(term != "(Intercept)") %>% 
#   filter(term %in% c("t","t2","tx5d","t:tx5d")) %>%
#   ggplot(aes(x = reorder(term, estimate), y = estimate, color=Model,
#              ymin = conf.low, ymax = conf.high)) +
#   geom_pointrange(position = position_dodge(width = 0.2)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   coord_flip() +
#   labs(
#     x = NULL, y = "Estimate", title = "95% Wald CIs",
#   )
# # plot_ci_wald
# ggsave("plots/plot_ci_wald.png", plot_ci_wald, width=8, height=5)
# 
# # plot_wald_CI <- function(model, temp_vars_only=TRUE, plot_title="95% Wald CIs") {
# #   ci_wald <- broom.mixed::tidy(
# #     model, effects = "fixed",conf.int = TRUE, conf.method = "Wald"
# #   )
# #   if (temp_vars_only) {
# #     ci_wald = ci_wald %>% filter(term %in% c("t","t2","tx5d","t:tx5d")) 
# #   }
# #   # Nice coefficient plot
# #   ci_wald %>%
# #     filter(term != "(Intercept)") %>%
# #     ggplot(aes(x = reorder(term, estimate), y = estimate,
# #                ymin = conf.low, ymax = conf.high)) +
# #     geom_pointrange() +
# #     geom_hline(yintercept = 0, linetype = "dashed") +
# #     coord_flip() +
# #     labs(x = NULL, y = "Estimate (95% CI)",
# #          title = plot_title,
# #          )
# # }
# # 
# # plot_wald_CI(mdl_paper, plot_title="Fixed effects model\nfrom the paper ") +
# #   plot_wald_CI(mdl_new, plot_title="New mixed effects\nmodel") +
# #   plot_annotation(title = "95% Wald CIs")

##################


