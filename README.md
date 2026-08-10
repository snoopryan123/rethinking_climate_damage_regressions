# Correcting for Spatial and Temporal Dependence in Estimating the Economic Effect of Extreme Heat

## Ryan S. Brill and Abraham J. Wyner

`10.5281/zenodo.21866517`

### Dataset

* The dataset is `data_extremes_growth_panel_monthedd_1979-2016.csv`
* We originally got this dataset from Callahan and Mankin's repository, `https://github.com/ccallahan45/CallahanMankin_ExtremeHeatEconomics_2022/blob/main/Data/Panel/extremes_growth_panel_monthedd_1979-2016.csv` 

### Code

* Shared data prep and helpers in `code_0_header.R`
* Replication of the primary regression from Callahan & Mankin (2022), and EDA, in `code_1_replicationAndEDA.R`
* Outlier analysis in `code_2_outlierAnalysis.R`
* Permutation tests in `code_3_permutationTests.R`
* Out-of-sample cross validation tests in `code_4_outOfSampleCVTests_v0.R`
* Bayesian model:
  * Model fitting in `code_5b_fitBayesianModels.R`
  * Stan model in `fullyBayesianModel_A.stan`
  * Plots (marginal effects, AR coefficient) for the main Bayesian spec in `code_5c_BayesianModel_plots.R`
  * Sensitivity comparison plot across model specs in `code_5d_BayesianModels_compare.R`
  * Posterior predictive checks at the country and region level in `code_5e_BayesianModel_PPC.R`
* Out-of-sample tests for the Bayesian model:
  * AR(1) year-effect spec in `code_6a_BayesianModel_outOfSampleTesting_yearEffect.R`
  * Lag-growth spec in `code_6b_BayesianModel_outOfSampleTesting_lagGrowth.R`
  * Plots in `code_6c_BayesianModel_outOfSamplePlots.R`


