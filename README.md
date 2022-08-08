# Rain Classification

This repository contains the R code related to the paper "Stratiform and convective rain classification using machine learning models and Micro Rain Radar".


More specifically, the R folder is structured as follows:
* data_preparation.R: prepares the raw dataset `pre_df_may.RData` for modelling. Therefore, the PIA adjustment, handling of missing values and feature engineering is carried out. The final datset `df_newdataprep2.rds` is stored in the data folder.
* benchmark.R: script where the model tuning and evaluation is carried out using nested cross-validation for all models that need to be tuned and a simple cross-validation for baseline models that are not tuned.
* analysis.R: analysis of tuning results for different models and finding explanation using IML methods on the final xgboost model. Also contains the creation of all model related figures shown in the paper.
