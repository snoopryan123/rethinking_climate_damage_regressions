
source("code_0_header.R")

################ 
#### Placebo adjustment / permutation test
################

### initial configuration
# B = 15 $5 #15
B = 250
perm_tests = c(
  "full (tx5d)",
  "within-year (tx5d)",
  "within-region (tx5d)",
  "ours (tx5d)",
  "ours (t,tx5d)",
  "post-selection inference"#,
  # "post-selection inference & omit outliers"
)
extreme_temp_vars = c("txx", "tx3d", "tx5d", "tx7d", "tx15d", "tmonx")
yrs = y1:y2
countries = sort(unique(dat$iso))
model_formula = "growth ~ temp + I(temp^2) + tx5d + tx5d:temp + var + var:seas + p | region + time | 0 | region"
extreme_countries = c("MNG","KEN")
extreme_years = c(1998, 2009)
### generate random seeds
set.seed(18763)
seeds <- sample.int(.Machine$integer.max, B)
seeds
### run the permutation test
lst_coeffs = list()
counter = 1
for (b in 1:B) {
  for (perm_test in perm_tests) {
    print(paste0("b=",b,"/B=",B,", perm_test=",perm_test))
    
    ### get the dataset of permuted variables
    {
      if (str_detect(perm_test, "full")) {
        
        ### which variables are we permuting?
        if (perm_test == "full (tx5d)") {
          vars_to_permute = c("tx5d")
        } else if (perm_test == "full (t,tx5d)") {
          vars_to_permute = c("t", "tx5d")
        } else {
          stop()
        }
        
        ### get key variables and the variables that we'll be permuting
        df_varsToPermute = dat %>% select(iso, region, time, all_of(vars_to_permute))
        df_varsToPermute
        
        ### permute the relevant variables
        for (v in vars_to_permute) {
          set.seed(seeds[b])
          df_varsToPermute[[paste0(v,"_perm")]] = sample(df_varsToPermute[[v]])
        }
        df_varsToPermute
        df_permutedVars = df_varsToPermute %>% select(-all_of(vars_to_permute))
        df_permutedVars
        
      } else if (str_detect(perm_test, "within-year")) {
        
        ### which variables are we permuting?
        if (perm_test == "within-year (tx5d)") {
          vars_to_permute = c("tx5d")
        } else if (perm_test == "within-year (t,tx5d)") {
          vars_to_permute = c("t", "tx5d")
        } else {
          stop()
        }
        
        ### get key variables and the variables that we'll be permuting
        df_varsToPermute = dat %>% select(iso, region, time, all_of(vars_to_permute)) %>% mutate(i = 1:n())
        df_varsToPermute
        
        ### for each year, get the row indices that we'll be permuting
        yrs_rowidxs = split(df_varsToPermute$i, df_varsToPermute$time)
        df_yr_rowidxs = enframe(yrs_rowidxs, name = "time", value = "i") %>% unnest(i) 
        df_yr_rowidxs
        
        ### for each year, create a permutation of the row indices
        set.seed(seeds[b])
        permuted_yr_rowidxs <- lapply(yrs_rowidxs, function(x) if (length(x)==1) x else sample(x))
        df_permuted_yr_rowidxs = enframe(permuted_yr_rowidxs, name = "time", value = "i_perm") %>% unnest(i_perm) 
        df_permuted_yr_rowidxs
        
        df_yr_rowidxs_withPerm = df_yr_rowidxs
        df_yr_rowidxs_withPerm$i_perm = df_permuted_yr_rowidxs$i_perm
        df_yr_rowidxs_withPerm
        
        df_yr_rowidxs_withPerm_1 = df_yr_rowidxs_withPerm %>% select(-time)
        df_yr_rowidxs_withPerm_1
        
        df_varsToPermute_1 = df_varsToPermute %>% left_join(df_yr_rowidxs_withPerm_1, by="i")
        df_varsToPermute_1
        
        ### permute the variables
        df_permutedVars_0 = df_varsToPermute_1
        for (v in vars_to_permute) {
          df_permutedVars_0[[paste0(v,"_perm")]] = df_varsToPermute[df_permutedVars_0$i_perm,v][[v]]
        }
        df_permutedVars_0
        
        df_permutedVars = df_permutedVars_0 %>% select(-all_of(vars_to_permute), -i, -i_perm)
        df_permutedVars
  
      } else if (str_detect(perm_test, "within-region")) {
        
        ### which variables are we permuting?
        if (perm_test == "within-region (tx5d)") {
          vars_to_permute = c("tx5d")
        } else if (perm_test == "within-region (t,tx5d)") {
          vars_to_permute = c("t", "tx5d")
        } else {
          stop()
        }
        
        ### get key variables and the variables that we'll be permuting
        df_varsToPermute = dat %>% select(iso, region, time, all_of(vars_to_permute)) %>% mutate(i = 1:n())
        df_varsToPermute
        
        ### for each region, get the row indices that we'll be permuting
        rgs_rowidxs = split(df_varsToPermute$i, df_varsToPermute$region)
        df_rg_rowidxs = enframe(rgs_rowidxs, name = "region", value = "i") %>% unnest(i) 
        df_rg_rowidxs
        
        ### for each region, create a permutation of the row indices
        set.seed(seeds[b])
        permuted_rg_rowidxs <- lapply(rgs_rowidxs, function(x) if (length(x)==1) x else sample(x))
        df_permuted_rg_rowidxs = enframe(permuted_rg_rowidxs, name = "region", value = "i_perm") %>% unnest(i_perm) 
        df_permuted_rg_rowidxs
        
        df_rg_rowidxs_withPerm = df_rg_rowidxs
        df_rg_rowidxs_withPerm$i_perm = df_permuted_rg_rowidxs$i_perm
        df_rg_rowidxs_withPerm
        
        df_rg_rowidxs_withPerm_1 = df_rg_rowidxs_withPerm %>% select(-region)
        df_rg_rowidxs_withPerm_1
        
        df_varsToPermute_1 = df_varsToPermute %>% left_join(df_rg_rowidxs_withPerm_1, by="i")
        df_varsToPermute_1
        
        ### permute the variables
        df_permutedVars_0 = df_varsToPermute_1
        for (v in vars_to_permute) {
          df_permutedVars_0[[paste0(v,"_perm")]] = df_varsToPermute[df_permutedVars_0$i_perm,v][[v]]
        }
        df_permutedVars_0
        
        df_permutedVars = df_permutedVars_0 %>% select(-all_of(vars_to_permute), -i, -i_perm)
        df_permutedVars
        
      } else if (str_detect(perm_test, "ours") | str_detect(perm_test, "post-selection inference")) {
        
        ### which variables are we permuting?
        if (str_detect(perm_test, "post-selection inference")) {
          # vars_to_permute = c("t", extreme_temp_vars)
          vars_to_permute = c(extreme_temp_vars)
        } else if (perm_test == "ours (tx5d)") {
          vars_to_permute = c("tx5d")
        } else if (perm_test == "ours (t,tx5d)") {
          vars_to_permute = c("t", "tx5d")
        } else {
          stop()
        }
        vars_to_permute
        
        ### get key variables and the variables that we'll be permuting
        df_varsToPermute = dat %>% select(iso, region, time, all_of(vars_to_permute)) %>% mutate(i = 1:n())
        df_varsToPermute
        
        ### for each country, what years are available
        df_ctry_yr = df_varsToPermute %>% distinct(iso,time)
        df_ctry_yr
        country_yrs <- split(df_ctry_yr$time, df_ctry_yr$iso)
  
        ### for each country, create a permutation of the years
        set.seed(seeds[b])
        permuted_country_yrs <- lapply(country_yrs, sample)
        df_permuted_country_yr = enframe(permuted_country_yrs, name = "iso", value = "time_perm") %>% unnest(time_perm)
        df_permuted_country_yr
        df_ctry_yr_withPerm = df_ctry_yr
        df_ctry_yr_withPerm$time_perm = df_permuted_country_yr$time_perm
        df_ctry_yr_withPerm
        
        df_varsToPermute_1 =
          df_varsToPermute %>%
          left_join(df_ctry_yr_withPerm, by=c("iso","time")) %>%
          group_by(region) %>%
          mutate(i_perm = i[match(time_perm, time)]) %>%
          ungroup()
        df_varsToPermute_1

        ### permute the variables
        df_permutedVars_0 = df_varsToPermute_1
        for (v in vars_to_permute) {
          df_permutedVars_0[[paste0(v,"_perm")]] = df_varsToPermute[df_permutedVars_0$i_perm,v][[v]]
        }
        df_permutedVars_0

        df_permutedVars = df_permutedVars_0 %>% select(-all_of(vars_to_permute), -i, -i_perm, -time_perm)
        df_permutedVars
        
      } else {
        stop(paste0("perm_test = ", perm_test, " is not supported."))
      }
    }
    df_permutedVars
    
    ### add the permuted variables back into the dataframe
    dat_b = dat %>% left_join(df_permutedVars, by=c("iso", "region", "time"))
    c(nrow(dat), nrow(dat_b), NA, ncol(dat), ncol(dat_b))
    if (str_detect(perm_test, "omit outliers")) {
      ### remove outliers from the training dataset
      dat_b = dat_b %>% filter( !(iso %in% extreme_countries) & !(time %in% extreme_years) )
    }
    
    ### model formula for the model with permuted variables
    model_formula_perm = model_formula
    for (v in vars_to_permute) {
      if (v == "t") { v = "temp" }
      model_formula_perm = str_replace_all(model_formula_perm, v, paste0(v,"_perm"))
    }
    model_formula_b = if (b==0) model_formula else model_formula_perm
    model_formula_b = str_replace_all(model_formula_b, "temp", "t")
    model_formula_b
    
    ### fit the model
    if (perm_test == "post-selection inference") {
      ### select the extreme temperature variable with highest marginal effect size
      fitted_model_outputs = list()
      ME_5_vals = c(); ME_25_vals = c(); ME_5_se = c(); ME_25_se = c(); 
      for (extreme_temp_var in extreme_temp_vars) {
        # print(paste0("extreme_temp_var = ", extreme_temp_var))
        
        ### fit model with this extreme temperature variable
        model_formula_b_psi = str_replace_all(model_formula_b, "tx5d", extreme_temp_var)
        model_output_b_psi = fit_model_and_get_output(model_formula_b_psi, dat_b)
        model_b_psi = model_output_b_psi$model
        
        # Get marginal effects
        t_varname_b_psi = if ("t" %in% vars_to_permute) "t_perm" else "t"
        tx_varname_b_psi = paste0(extreme_temp_var, "_perm")
        ME_5_output = compute_ME(model_b_psi, t_varname_b_psi, tx_varname_b_psi, Tstar=5)
        ME_25_output = compute_ME(model_b_psi, t_varname_b_psi, tx_varname_b_psi, Tstar=25)
        ME_5_output$extreme_temp_var = extreme_temp_var
        ME_25_output$extreme_temp_var = extreme_temp_var
        
        ### Record output
        ME_5_vals = c(ME_5_vals, ME_5_output$estimate)
        ME_25_vals = c(ME_25_vals, ME_25_output$estimate)
        ME_5_se = c(ME_5_se, ME_5_output$se)
        ME_25_se = c(ME_25_se, ME_25_output$se)
        fitted_model_outputs[[extreme_temp_var]] = model_output_b_psi
      }
      df_tx_ME_results = 
        tibble(
          extreme_temp_var = extreme_temp_vars, 
          ME_5 = ME_5_vals, ME_25 = ME_25_vals,
          ME_5_se = ME_5_se, ME_25_se = ME_25_se,
        ) %>%
        mutate(
          magnitude = pmax(ME_5,0) - pmin(ME_25,0)
        ) %>% 
        arrange(desc(magnitude))
      df_tx_ME_results
      selected_extreme_temp_var = first(df_tx_ME_results$extreme_temp_var)
      selected_extreme_temp_var
      
      ### fit the model
      model_output_b = fitted_model_outputs[[selected_extreme_temp_var]]
    } else {
      ### fit the model
      model_output_b = fit_model_and_get_output(model_formula_b, dat_b)
    }
    model_output_b
    
    ### get fitted coefficients 
    coeffs_b = model_output_b$coeffs
    coeffs_b = coeffs_b %>% rename(estimate=Estimate, se = `Cluster s.e.`, pval = `Pr(>|t|)`, tval = `t value`)
    coeffs_b
    
    ### compute the marginal effects
    model_b = model_output_b$model
    t_varname_b = if ("t" %in% vars_to_permute) "t_perm" else "t"
    if (perm_test == "post-selection inference") {
      tx_varname_b = if (selected_extreme_temp_var %in% vars_to_permute) paste0(selected_extreme_temp_var,"_perm") else selected_extreme_temp_var
    } else {
      tx_varname_b = if ("tx5d" %in% vars_to_permute) "tx5d_perm" else "tx5d"
    }
    ME_5_output = compute_ME(model_b, t_varname_b, tx_varname_b, Tstar=5)
    ME_25_output = compute_ME(model_b, t_varname_b, tx_varname_b, Tstar=25)
    
    ### add the marginal effects to the coefficients dataframe
    coeffs_b = 
      bind_rows(
        coeffs_b
        ,
        tibble(
          estimate = c(ME_25_output$estimate_raw, ME_25_output$estimate),
          se = c(ME_25_output$se_raw, ME_25_output$se),
          coeff = c("ME_25_raw", "ME_25"),
        )
        ,
        tibble(
          estimate = c(ME_5_output$estimate_raw, ME_5_output$estimate),
          se = c(ME_5_output$se_raw, ME_5_output$se),
          coeff = c("ME_5_raw", "ME_5"),
        )
      )
    ### add metadata to coefficients dataframe
    coeffs_b$b = b
    coeffs_b$perm_test = perm_test
    coeffs_b
    
    ### re-name the extreme temperature variable as Tx, and temperature as T
    if (perm_test == "post-selection inference") {
      coeffs_b = coeffs_b %>% mutate(coeff = str_replace_all(coeff, selected_extreme_temp_var, "Tx"))
    } else {
      coeffs_b = coeffs_b %>% mutate(coeff = str_replace_all(coeff, "tx5d", "Tx"))
    }
    coeffs_b = coeffs_b %>% mutate(coeff = str_replace_all(coeff, "t_perm", "T_perm"))
    coeffs_b = coeffs_b %>% mutate(coeff = str_replace_all(coeff, "\\bt\\b", "T"))
    coeffs_b = coeffs_b %>% mutate(coeff = str_remove_all(coeff, "_perm"))
    coeffs_b
    
    ### save the coefficients
    lst_coeffs[[counter]] = coeffs_b
    counter = counter + 1
  }
}
df_coeffs = bind_rows(lst_coeffs)
df_coeffs$perm_test = factor(df_coeffs$perm_test, levels = perm_tests)
df_coeffs
table(df_coeffs$coeff)

################ 
#### Results
################

### get relevant coefficients and p values
df_coeffs_1 = 
  df_coeffs %>%
  filter(coeff != "var" & coeff != "p" & coeff != "var:seas") %>%
  filter(!str_detect(coeff, "ME_*[0-9]*[0-9]_raw")) %>%
  relocate(b, .before=estimate) %>%
  relocate(coeff, .after=b) %>%
  mutate(
    perm_test_lab = 
      case_when(
        perm_test=="post-selection inference" ~ "Post-selection\ninference",
        perm_test=="full (tx5d)" ~ "Full\nsample\nrandomization\nof Tx5d",
        perm_test=="within-year (tx5d)" ~ "Within-year\nrandomization\nof Tx5d",
        perm_test=="within-region (tx5d)" ~ "Within-region\nrandomization\nof Tx5d",
        perm_test=="ours (tx5d)" ~ "Country\nyear\nrandomization\nof Tx5d",
        perm_test=="ours (t,tx5d)" ~ "Country\nyear\njoint\nrandomization\nof (Tx5d,T)",
        TRUE ~ perm_test,
      )
  ) 
df_coeffs_1

###
temporary = df_coeffs_1 %>% filter(str_detect(coeff,"ME")) %>% 
  filter(str_detect(perm_test,"post-selection")) %>% arrange(coeff)

### get original model info
{
  ### fit the original model
  model_output_og = fit_model_and_get_output(str_replace_all(model_formula, "temp", "t"), dat)
  model_output_og
  
  ### get fitted coefficients 
  coeffs_og = model_output_og$coeffs
  coeffs_og = coeffs_og %>% rename(estimate=Estimate, se = `Cluster s.e.`, pval = `Pr(>|t|)`, tval = `t value`)
  coeffs_og
  
  ### compute the marginal effects
  model_og = model_output_og$model
  ME_5_output = compute_ME(model_og, "t", "tx5d", Tstar=5)
  ME_25_output = compute_ME(model_og, "t", "tx5d", Tstar=25)
  
  ### add the marginal effects to the coefficients dataframe
  coeffs_og = 
    bind_rows(
      coeffs_og
      ,
      tibble(
        estimate = c(ME_25_output$estimate_raw, ME_25_output$estimate),
        se = c(ME_25_output$se_raw, ME_25_output$se),
        coeff = c("ME_25_raw", "ME_25"),
      )
      ,
      tibble(
        estimate = c(ME_5_output$estimate_raw, ME_5_output$estimate),
        se = c(ME_5_output$se_raw, ME_5_output$se),
        coeff = c("ME_5_raw", "ME_5"),
      )
    )
  coeffs_og
  
  ### re-name the extreme temperature variable as Tx, and temperature as T
  coeffs_og = coeffs_og %>% mutate(coeff = str_replace_all(coeff, "tx5d", "Tx"))
  coeffs_og = coeffs_og %>% mutate(coeff = str_replace_all(coeff, "\\bt\\b", "T"))
  coeffs_og
  
  ###
  df_marginalEffectCIs_og =
    tibble(
      T = c("5", "25"),
      ME_L = c(
        coeffs_og %>% filter(coeff=="ME_5") %>% mutate(ME_L = estimate - 1.96*se) %>% pull(ME_L)
        ,
        coeffs_og %>% filter(coeff=="ME_25") %>% mutate(ME_L = estimate - 1.96*se) %>% pull(ME_L)
      ),
      ME_M = c(
        coeffs_og %>% filter(coeff=="ME_5") %>% pull(estimate)
        ,
        coeffs_og %>% filter(coeff=="ME_25") %>% pull(estimate)
      ),     
      ME_U = c(
        coeffs_og %>% filter(coeff=="ME_5") %>% mutate(ME_U = estimate + 1.96*se) %>% pull(ME_U)
        ,
        coeffs_og %>% filter(coeff=="ME_25") %>% mutate(ME_U = estimate + 1.96*se) %>% pull(ME_U)
      ),
    )
  df_marginalEffectCIs_og
  
  ###
  df_coeffs_og_CI = 
    coeffs_og %>%
    filter(coeff != "var" & coeff != "p" & coeff != "var:seas") %>%
    filter(!str_detect(coeff, "ME_*[0-9]*[0-9]_raw")) %>%
    group_by(coeff) %>%
    reframe(
      estimate_L = estimate - 1.96*se,
      estimate_M = estimate,
      estimate_U = estimate + 1.96*se,
    )
  df_coeffs_og_CI
}

### plot permutation distribution of the marginal effects
df_plot_permutationMarginalEffects = 
  df_coeffs_1 %>%
  filter(str_detect(coeff, "ME")) %>%
  select(b, perm_test, perm_test_lab, coeff, estimate) %>%
  # pivot_longer(-c(b,perm_test,perm_test_lab), values_to = "ME", names_to = "t") %>%
  mutate(T = str_remove(coeff, "ME_")) %>%
  rename(ME = estimate) %>%
  select(-coeff) %>%
  mutate(
    T = factor(T, levels=c("5","25")),
  ) %>%
  group_by(perm_test,perm_test_lab,T) %>%
  reframe(
    ME_L = quantile(ME, 0.025),
    ME_M = quantile(ME, 0.5),
    ME_U = quantile(ME, 0.975),
  ) 
df_plot_permutationMarginalEffects = bind_rows(
  df_marginalEffectCIs_og %>% mutate(perm_test = "Original model", perm_test_lab = "Original\nmodel"),
  df_plot_permutationMarginalEffects
)
df_plot_permutationMarginalEffects$perm_test = factor(
  df_plot_permutationMarginalEffects$perm_test,
  levels=c("Original model", levels(df_coeffs_1$perm_test))
)
df_plot_permutationMarginalEffects$T = factor(
  df_plot_permutationMarginalEffects$T,
  levels=c("5","25")
)
df_plot_permutationMarginalEffects

plot_permutationMarginalEffects =
  df_plot_permutationMarginalEffects %>%
  ggplot(aes(x = fct_reorder(perm_test_lab, as.numeric(perm_test)),
             ymin = ME_L, y = ME_M, ymax = ME_U, color = T)) +
  geom_hline(yintercept = 0, linetype="dashed", color="gray50") +
  scale_color_manual(values=c("25" = "red", "5" = "blue")) +
  geom_point(size=3, position=position_dodge(width = 0.4)) +
  geom_errorbar(width=0.25, position=position_dodge(width=0.4)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    plot.margin = margin(t=10, r=10, b=20, l=10)
  ) +
  labs(
    y = "Marginal effect (p.p. per. s.d.)", x=""
  )
# plot_permutationMarginalEffects
ggsave("plots/plot_permutationTestAMarginalEffects.png",
       plot_permutationMarginalEffects, width=14, height=7)

### three-panel split: Original | C&M's permutation tests | ours
{
  groupOriginal_perm = c("Original model")
  groupA_perm        = c("full (tx5d)","within-year (tx5d)","within-region (tx5d)")
  groupB_perm        = c("ours (tx5d)","ours (t,tx5d)","post-selection inference")
  shared_ylim_perm = range(
    c(df_plot_permutationMarginalEffects$ME_L,
      df_plot_permutationMarginalEffects$ME_U),
    na.rm = TRUE
  )

  make_perm_panel = function(df_sub, panel_title, show_y = TRUE) {
    p <- df_sub %>%
      ggplot(aes(x = fct_reorder(perm_test_lab, as.numeric(perm_test)),
                 ymin = ME_L, y = ME_M, ymax = ME_U, color = T)) +
      geom_hline(yintercept = 0, linetype="dashed", color="gray50") +
      scale_color_manual(values=c("25" = "red", "5" = "blue")) +
      geom_point(size=3, position=position_dodge(width = 0.4)) +
      geom_errorbar(width=0.25, position=position_dodge(width=0.4)) +
      coord_cartesian(ylim = shared_ylim_perm) +
      theme(
        axis.text.x  = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 16),
        axis.text.y  = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text  = element_text(size = 16),
        legend.title = element_text(size = 16),
        plot.title   = element_text(size = 14, hjust = 0.5),
        plot.margin  = margin(t=10, r=10, b=15, l=10)
      ) +
      labs(y = if (show_y) "Marginal effect (p.p. per. s.d.)" else NULL,
           x = "", title = panel_title)
    if (!show_y) p <- p + theme(axis.text.y = element_blank(),
                                axis.ticks.y = element_blank())
    p
  }

  panelOriginal = make_perm_panel(
    df_plot_permutationMarginalEffects %>% filter(perm_test %in% groupOriginal_perm),
    "C&M's Original Model",
    show_y = TRUE
  )
  panelA = make_perm_panel(
    df_plot_permutationMarginalEffects %>% filter(perm_test %in% groupA_perm),
    "C&M's permutation tests",
    show_y = FALSE
  )
  panelB = make_perm_panel(
    df_plot_permutationMarginalEffects %>% filter(perm_test %in% groupB_perm),
    "Our dependence-preserving permutation tests",
    show_y = FALSE
  )

  plot_permMarginalEffects_split = (panelOriginal + panelA + panelB) +
    patchwork::plot_layout(guides = "collect", widths = c(1, 3, 3)) &
    theme(legend.position = "right")

  ggsave("plots/plot_permutationTestAMarginalEffects_split.png",
         plot_permMarginalEffects_split, width=14, height=7)
}

### plot permutation p-value for post-selection inference
df_plot_permPValForPSI = 
  df_coeffs_1 %>% 
  filter(perm_test == "post-selection inference") %>%
  filter(str_detect(coeff, "ME")) %>% 
  select(b, perm_test, estimate, coeff) %>% 
  filter(str_detect(coeff, "ME")) %>% 
  filter(!str_detect(coeff, "raw")) %>%
  left_join(
    coeffs_og %>% 
      select(estimate, coeff) %>% 
      filter(str_detect(coeff, "ME")) %>% 
      filter(!str_detect(coeff, "raw")) %>%
      rename(estimate_og = estimate)
  ) %>%
  mutate(T = str_remove(coeff, "ME_")) %>%
  rename(ME = estimate, ME_og = estimate_og) %>%
  select(-coeff) %>%
  mutate(
    T = factor(T, levels=c("5","25")),
  ) %>%
  group_by(T) %>%
  mutate(
    ME_M = mean(ME)
  )
df_plot_permPValForPSI

tablePostSelectionPValue = 
  df_plot_permPValForPSI %>%
  group_by(T) %>%
  reframe(
    num = unique(
      ifelse(T=="25", sum(ME <= ME_og), sum(ME >= ME_og))
    ),
    n = n()
  ) %>%
  mutate(pval = num/n)
tablePostSelectionPValue

# gt::gtsave(
#   gt::gt(tablePostSelectionPValue)
#   , "plots/plot_permutationTestAPostSelectionPValueTable.png"
# )

plotPostSelectionPValue = 
  df_plot_permPValForPSI %>%
  ggplot(aes(x = ME)) +
  # facet_wrap(~T) +
  facet_wrap(~paste0("T=",T)) +
  geom_histogram(fill="black") +
  geom_vline(aes(xintercept = ME_og), color="firebrick", linewidth=1 ) +
  geom_vline(aes(xintercept = ME_M), color="dodgerblue2", linewidth=1 ) +
  geom_vline(xintercept = 0, linetype="dashed", color="gray50", linewidth=1) +
  labs(
    x = "Marginal effect of Tx (p.p. per. s.d.)", #y="Count"
  )
# plotPostSelectionPValue
ggsave("plots/plot_permutationTestAPostSelectionPValue.png",width=8,height=3.5)

### plot permutation distribution of the coefficients
df_plot_permutationCoeffs = 
  df_coeffs_1 %>%
  group_by(coeff) %>%
  mutate(coeff = str_remove(coeff, "estimate_")) %>%
  ungroup() %>%
  group_by(perm_test,perm_test_lab,coeff) %>%
  reframe(
    estimate_L = quantile(estimate, 0.025),
    estimate_M = quantile(estimate, 0.5),
    estimate_U = quantile(estimate, 0.975),
  ) 
df_plot_permutationCoeffs = bind_rows(
  df_coeffs_og_CI %>% mutate(perm_test = "Original model", perm_test_lab = "Original model"),
  df_plot_permutationCoeffs
)
df_plot_permutationCoeffs$perm_test = factor(
  df_plot_permutationCoeffs$perm_test,
  levels=c("Original model", levels(df_coeffs_1$perm_test))
)
df_plot_permutationCoeffs

plot_permutationCoeffs = 
  df_plot_permutationCoeffs %>%
  filter(!str_detect(coeff, "ME")) %>%
  ggplot(aes(x = fct_reorder(perm_test_lab, as.numeric(perm_test)), ymin=estimate_L,y=estimate_M,ymax=estimate_U)) +
  facet_wrap(~coeff, scales="free") +
  geom_hline(yintercept = 0, linetype="dashed", color="gray50") +
  geom_point(size=3) + 
  geom_errorbar(width=0.1, position="dodge") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x="", y="Coefficient value")
# plot_permutationCoeffs
ggsave("plots/plot_permutationTestACoeffs.png",width=15,height=7)

### plot permutation distribution of the p values
df_plot_permutationPvals = 
  df_coeffs_1 %>%
  group_by(coeff) %>%
  mutate(coeff = str_remove(coeff, "pval_")) %>%
  ungroup() %>%
  group_by(perm_test,perm_test_lab,coeff) %>%
  reframe(
    prop_sig = sum(pval <= 0.05) / n()
  ) 
plot_permutationPvals = 
  df_plot_permutationPvals %>%
  filter(!str_detect(coeff, "ME")) %>%
  ggplot(aes(x = fct_reorder(perm_test_lab, as.numeric(perm_test)), y=prop_sig)) +
  facet_wrap(~coeff) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="Percentage Of Tests Deemed Significant")
# plot_permutationPvals
ggsave("plots/plot_permutationTestAPvals.png",width=15,height=7)

##################


