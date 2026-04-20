
################ 
#### Packages 
################

# rm(list=ls())
# Libraries
library(tidyverse)
library(cmdstanr)
library(tidyr)
library(lfe)
library(dplyr)
library(lemon)
library(texreg)
library(cowplot)
library(gridExtra)
library(caret)
library(lmtest)
library(patchwork)
library(ggpmisc)
library(countrycode)
library(fixest)
library(lme4)
library(broom.mixed)
# plotting
theme_set(theme_bw())
theme_update(text = element_text(size=18))
theme_update(plot.title = element_text(hjust = 0.5))

################ 
#### Data
################

# locations

loc_data <- "" #"../Data/Panel/"
loc_save_reg <- "" #"..//Data/RegressionResults/"

# read in data
y1 <- 1979
y2 <- 2016
threshold_type <- "month"
panel_in <- read.csv(paste0(loc_data,"data_extremes_growth_panel_",threshold_type,"edd_",y1,"-",y2,".csv"))

# add continent & subcontinent variables
df_iso_continent <- 
  panel_in %>% distinct(iso) %>% filter(!is.na(iso)) %>%
  mutate(
    continent  = countrycode(iso, origin = "iso3c", destination = "un.region.name"),
    subcontinent = countrycode(iso, origin = "iso3c", destination = "un.regionsub.name"),
    continent = ifelse(continent=="Oceania","Asia",continent),
  ) %>%
  as_tibble() %>%
  suppressWarnings()
table(df_iso_continent$continent)
table(df_iso_continent$subcontinent)
df_iso_continent %>% distinct(continent, subcontinent) %>% arrange(continent, subcontinent)
panel_in_1 = 
  panel_in %>% 
  left_join(df_iso_continent) %>% 
  relocate(continent, .after=iso) %>% relocate(subcontinent, .after=continent)
panel_in_1

dim(panel_in)
dim(panel_in_1)

###
panel_in_1 %>% filter(time>=y1,time<=y2) %>% drop_na(growth) -> panel
panel = as_tibble(panel)

# create some variables
panel$t2 <- (panel$t)**2
panel$p2 <- (panel$p)**2
panel$tmean2 <- (panel$tmean)**2
panel$edd <- panel$edd98
panel$seas2 <- (panel$seas)**2
panel$seas2_ann <- (panel$seas_ann)**2
panel$t2_summer <- (panel$t_summer)**2
panel$t2_winter <- (panel$t_winter)**2

# five year blocks by country
panel %>% rowwise() %>% 
  mutate(block=round((time+2)/5)*5,
         block_2=round((time+2)/2)*2,
         block_3=round((time+2)/3)*3,
         yr_iso=paste0(iso,"_",time)) %>%
  mutate(year_block=paste0(iso,"_",block)) -> panel

# lags
vars_to_lag <- c("t","t2","edd","var","p","p2","growth",
                 "tx7d","t_summer","seas_ann","txx","tx5d",
                 "luminosity")
for (v in c(1:length(vars_to_lag))){
  var <- vars_to_lag[v]
  print(var)
  for (l in c(1:10)){
    panel %>% group_by(region) %>% 
      mutate(!!paste(var,"_lag",l,sep="") := lag((!!as.name(var)),l)) -> panel
  }
}

# dataset `dat` for doing regression with extra variables
panel = panel %>% ungroup()
panel %>% filter(t!=0) -> dat # when t is exactly 0 it's an error
dat$growth <- dat$growth*100
# dat$time <- as.factor(as.character(dat$time))
dat$region_time <- with(dat, interaction(region, time, sep = "_"))
dat$iso_time <- with(dat, interaction(iso, time, sep = "_"))
dat$iso_block <- with(dat, interaction(iso, block, sep = "_"))
dat$iso_block2 <- with(dat, interaction(iso, block_2, sep = "_"))
dat$iso_block3 <- with(dat, interaction(iso, block_3, sep = "_"))
dat$region_time <- with(dat, interaction(region, time, sep = "_"))
dat$region_block <- with(dat, interaction(region, block, sep = "_"))
dat$region_block2 <- with(dat, interaction(region, block_2, sep = "_"))
dat$region_block3 <- with(dat, interaction(region, block_3, sep = "_"))
dat$pop_in_100k = dat$population/100000
dat$pop_in_mil = dat$population/1000000
dat$iso_pop_in_mil = dat$iso_pop/1000000
# table(dat$region)

# # add continent & subcontinent variables
# df_iso_continent <- 
#   dat %>% distinct(iso) %>% filter(!is.na(iso)) %>%
#   mutate(
#     continent  = countrycode(iso, origin = "iso3c", destination = "un.region.name"),
#     subcontinent = countrycode(iso, origin = "iso3c", destination = "un.regionsub.name"),
#     continent = ifelse(continent=="Oceania","Asia",continent),
#   )
# table(df_iso_continent$continent)
# table(df_iso_continent$subcontinent)
# df_iso_continent %>% distinct(continent, subcontinent) %>% arrange(continent, subcontinent)
# dat = dat %>% left_join(df_iso_continent) %>% 
#   relocate(continent, .after=iso) %>% relocate(subcontinent, .after=continent)
# dat

#####################
### Functions for computing Marginal Effects of extreme temperature
#####################

### model fitting function
fit_model_and_get_output <- function(model_formula_b, dat_b) {
  ### fit the model
  model_b <- felm(as.formula(model_formula_b), data=dat_b)
  model_b
  
  ### get the coefficients
  model_b_summary = summary(model_b)
  coeffs_b = as_tibble(model_b_summary$coefficients)
  coeffs_b$coeff = rownames(model_b_summary$coefficients)
  coeffs_b
  
  list(model = model_b, coeffs = coeffs_b)
}

### compute ME directly from fitted model
compute_ME <- function(model, t_varname, tx_varname, Tstar, vcov_type = NULL) {
  # variable name
  tx_t_varname = paste0(t_varname,":",tx_varname)
  
  # variance-covariance matrix
  V <- vcov(model) 
  
  # coefficients
  b <- coef(model)[c(tx_varname, tx_t_varname)]
  
  # variance-covariance submatrix
  V_sub <- V[c(tx_varname, tx_t_varname), c(tx_varname, tx_t_varname), drop = FALSE]
  
  # linear combination vector
  L <- c(1, Tstar)
  
  # sd of tx for scaling
  sd_tx <- 
    df_withinRegionSdOfExtremeTempVar %>% 
    filter(extreme_temp_var == str_remove(tx_varname,"_perm")) %>% 
    pull(withinRegionSd)
  
  # estimate
  est_raw <- as.numeric((t(L) %*% b))
  est <- sd_tx * est_raw
  
  # standard error with covariance included
  se_raw  <- sqrt( as.numeric(t(L) %*% V_sub %*% L) )
  se  <- sd_tx * se_raw
  
  # # 95% confidence interval
  # ci95_raw <- c(est_raw - 1.96*se_raw, est_raw + 1.96*se_raw)
  # ci95 <- c(est - 1.96*se, est + 1.96*se)
  # return(list(estimate = est, se = se, ci95 = ci95, estimate_raw = est_raw, se_raw = se_raw, ci95_raw = ci95_raw))
  
  return(list(estimate = est, se = se, estimate_raw = est_raw, se_raw = se_raw))
}

###
df_withinRegionSdOfExtremeTempVar = 
  dat %>% 
  select(region, txx, tx3d, tx5d, tx7d, tx15d, tmonx) %>%
  pivot_longer(-region, names_to="extreme_temp_var") %>%
  group_by(region, extreme_temp_var) %>%
  reframe(sd = sd(value)) %>%
  group_by(extreme_temp_var) %>%
  reframe(withinRegionSd = mean(sd, na.rm=T))
df_withinRegionSdOfExtremeTempVar

### wider & marginal effect of tx5d
withinRegionTx5dSDs = dat %>% group_by(region) %>% reframe(sd_tx5d = sd(tx5d))
withinRegionTx5dSDs
sd_withinRegionTx5d = mean(withinRegionTx5dSDs$sd_tx5d, na.rm=T)
sd_withinRegionTx5d

#####################
### Smaller cleaner dataset & usable for cross validations & Bayesian model
#####################

### GET DATA FOR CROSS VALIDATION: select relevant variables and make time block
num_time_blocks = 6
years = unique(sort(dat$time))
year_cutpoints = seq(min(years), max(years), length.out = num_time_blocks+1)
# years
# year_cutpoints
dat1 = 
  dat %>% 
  select(
    growth,
    growth_lag1,
    time, t, region, iso, subcontinent, continent,  
    t, t2, tx5d, var, seas, p,
    iso_pop_in_mil,
  ) %>%
  # drop_na() %>%
  drop_na(growth_lag1, var) %>%
  rename(country = iso) %>%
  # filter(!(country %in% c("GTM", "LAO"))) %>% ### only occur for a few years,fucks up the CV
  mutate(blk = cut(time, year_cutpoints, include.lowest=T)) %>%
  relocate(blk, .after=time)
dat1

### Create the iid folds
nfolds = 10
set.seed(2846)
fold_list <- createFolds(1:nrow(dat1),k = nfolds,list = TRUE,returnTrain = FALSE)
dat1$iid_fold <- NA_integer_
for(i in seq_along(fold_list)) { dat1$iid_fold[ fold_list[[i]] ] <- i }
dat1 = dat1 %>% relocate(iid_fold, .after=blk)

###
sort(unique(dat1$iid_fold))
levels(dat1$blk)
sum(is.na(dat1))
dat1
# t(dat1 %>% summarise(across(everything(), ~ sum(is.na(.))))) ### check NA;s

### get data for stan
get_data_list_for_stan <- function(
    dat, train_years, test_year=NA, useClimateVars=1, rem_cols=c("var", "var_seas", "p"),
    useYearEffect=1, useARYearEffect=1, beta_prior_sd_coef=10
) {
  
  if (is.na(test_year)) {
    ### no test data
    dat = dat %>% filter(time %in% c(train_years)) # just keep relevant years
    dat_train = dat
    dat_test = tibble()
  } else {
    ### yes test data
    if (!(test_year %in% dat$time)) {
      stop(paste0("make sure ", test_year, " is a valid year in dat$time"))
    }
    
    ### just keep relevant years
    dat = dat %>% filter(time %in% c(train_years, test_year))
    
    # make sure every test country is present in the training set
    train_countries = sort(unique(dat %>% filter(time %in% train_years) %>% pull(country)))
    test_countries = sort(unique(dat %>% filter(time %in% test_year) %>% pull(country)))
    omit_countries = setdiff(train_countries, test_countries)
    print(paste0("we are omitting these countries that are only present in the test set: ", paste0(omit_countries,collapse=", ")))
    dat = dat %>% filter(!(country %in% omit_countries))
    
    # make sure every test region is present in the training set
    train_regions = sort(unique(dat %>% filter(time %in% train_years) %>% pull(region)))
    test_regions = sort(unique(dat %>% filter(time %in% test_year) %>% pull(region)))
    omit_regions = setdiff(train_regions, test_regions)
    print(paste0("we are omitting these regions that are only present in the test set: ", paste0(omit_regions,collapse=", ")))
    dat = dat %>% filter(!(region %in% omit_regions))
  }
  
  ### Create indices and variables for our Bayesian model
  dat <- 
    dat %>%
    mutate(
      province_idx = as.integer(fct_inorder(as.factor(region))),
      country_idx  = as.integer(fct_inorder(as.factor(country))),
      year_idx = as.integer(fct_reorder(factor(time), time)),
      t_sq   = if ("t_sq"   %in% names(.)) t_sq   else t^2,
      tx5d_t = if ("tx5d_t" %in% names(.)) tx5d_t else tx5d * t,
      var_seas = if ("var_seas" %in% names(.)) var_seas else var * seas,
    )
  dat
  # Check the year index
  data.frame(dat %>% distinct(time, year_idx) %>% arrange(time))
  
  #
  num_countries = max(dat$country_idx)
  num_provinces = max(dat$province_idx)
  num_years = max(dat$year_idx)
  
  # Province -> Country map (length = num_provinces)
  prov_map <- 
    dat %>%
    distinct(region, province_idx, country_idx) %>%
    arrange(province_idx) %>%
    pull(country_idx)
  head(prov_map, 30)
  
  # dat train and test
  dat_train = dat %>% filter(time %in% train_years)
  dat_test = dat %>% filter(time %in% test_year)
  n_test = nrow(dat_test)

  # Additional covariates (optional)
  k <- length(rem_cols)
  remMat <- if (k > 0) as.matrix(dat_train[, rem_cols, drop = FALSE]) else matrix(nrow = nrow(dat_train), ncol = 0)
  head(remMat)
  
  if (n_test > 0) {
    remMat_test <- if (k > 0) as.matrix(dat_test[, rem_cols, drop = FALSE]) else matrix(nrow = nrow(dat_test), ncol = 0)
  } else {
    remMat_test = matrix(nrow = 0, ncol = 0)
  }
  
  if (n_test > 0) {
    year_idx_test = unique(dat_test$year_idx)
    if (length(unique(dat_test$year_idx)) > 1) {
      stop("this model is only supported for ONE year of testing data.")
    }
  } else {
    year_idx_test = 0
  }
  
  # Assemble the Stan data list -------------------------------------------
  stan_data <- list(
    dat_train = dat_train, # just to have
    dat_test = dat_test, # just to have
    ###
    useClimateVars      = useClimateVars,
    useYearEffect       = useYearEffect,
    useARYearEffect     = useARYearEffect,
    beta_prior_sd_coef  = beta_prior_sd_coef,
    n                   = nrow(dat_train),
    num_countries       = num_countries,
    num_provinces       = num_provinces,
    num_years           = num_years,
    ###
    province_idx        = dat_train$province_idx,
    year_idx            = dat_train$year_idx,
    country_of_province = as.integer(prov_map),
    growth              = as.numeric(dat_train$growth),
    t                   = as.numeric(dat_train$t),
    t_sq                = as.numeric(dat_train$t_sq),
    tx5d                = as.numeric(dat_train$tx5d),
    tx5d_t              = as.numeric(dat_train$tx5d_t),
    k                   = k,
    remVarsMat          = remMat,
    ###
    n_test              = n_test,
    province_idx_test   = dat_test$province_idx,
    year_idx_test       = year_idx_test,
    growth_test         = as.numeric(dat_test$growth),
    t_test              = as.numeric(dat_test$t),
    t_sq_test           = as.numeric(dat_test$t_sq),
    tx5d_test           = as.numeric(dat_test$tx5d),
    tx5d_t_test         = as.numeric(dat_test$tx5d_t),
    remVarsMat_test     = remMat_test
  )
  
  stan_data
}

### fit stan model
fit_stan_model <- function(stan_data, iter_warmup, iter_sampling, refresh=100, num_chains=4, thin=10, seed=2025) {
  fit <- mod$sample(
    data = stan_data,
    seed = seed,
    chains = num_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    thin = thin,
    refresh = refresh
  )
  fit
}

##################


