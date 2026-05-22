# No Detectable Economic Effect of Extreme Heat After Correcting for Dependence

## Ryan S. Brill and Abraham J. Wyner

### Dataset

* The dataset is `data_extremes_growth_panel_monthedd_1979-2016.csv`
* We originally got this dataset from Callahan and Mankin's repository, `https://github.com/ccallahan45/CallahanMankin_ExtremeHeatEconomics_2022/blob/main/Data/Panel/extremes_growth_panel_monthedd_1979-2016.csv` 

### Code

* Shared data prep and helpers in `code_0_header.R`
* Replication of the primary regression from Callahan & Mankin (2022), and EDA, in `code_1_replicationAndEDA.R`
* Bayesian model fitting in `code_5b_BayesianSensitivity.R`
* Outlier analysis in `code_2_outlierAnalysis.R`
* Permutation tests in `code_3_permutationTests.R`
* Out-of-sample cross validation tests in `code_4_outOfSampleCVTests_v0.R`
* Bayesian model plots (marginal effects, AR coefficient) for the main Bayesian spec in `code_5c_BayesianModel_plots.R`
* Bayesian model comparison plot in `code_5d_BayesianSensitivity_compare.R`
* Out-of-sample tests for the Bayesian model in `code_6_BayesianModel_outOfSampleTesting.R`


