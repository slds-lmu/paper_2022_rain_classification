# Rain Classification

This repository contains the R code related to the paper "Stratiform and convective rain classification using machine learning models and Micro Rain Radar".


More specifically, the R folder is structured as follows:
* `data_preparation.R`: prepares the raw dataset `pre_df_may.RData` for modelling. Therefore, the PIA adjustment, handling of missing values and feature engineering is carried out. The final datset `df_newdataprep2.rds` is stored in the data folder.
* `benchmark.R`: script where the model tuning and evaluation is carried out using nested cross-validation for all models that need to be tuned and a simple cross-validation for baseline models that are not tuned.
* `analysis.R`: analysis of tuning results for different models and finding explanation using IML methods on the final xgboost model. Also contains the creation of all model related figures shown in the paper.

With regards to the used datasets: 
The raw data are currently only partially available at https://en.aeris-data.fr/catalogue-en/ This includes MRR and disdrometer records of the first few months of 2017 as a part of the Cerdanya-2017 field campaign. The rest of the raw data are available direct request to the Department of Applied Physics - Meteorology, University of Barcelona. 
* The disdrometer raw data are measured by “OTT Particle Size Velocity (PARSIVEL) disdrometer” which is an optical disdrometer that determines the particle size and velocity. The row data represents for each record the number of drops that fall within one minute of precipitation. The number of drops within each minute is aggregated by 32 * 32 different classes of diameter range and velocity range. Mounting to 1024 classes. 
* The MRR raw data are measured by a K band (24 GHz) FM–CW Doppler radar profiler manufactured by METEK which provides precipitation observations and records spectra at 32 range gates. In this case, MRR was configured to measure an observable height range between 100 and 3100 m with a vertical resolution of 100 m. MRR raw data files are in ASCII format and include different variables, such as the transfer function, the raw spectrum, and the constant of calibration.



The dataset `df_newdataprep2.rds` which is the dataset we are using for the modelling approach is provided in the data folder. Hence, the modelling and interpretation results of the paper can be replicated with this dataset and the `benchmark.R` and `analysis.R` script, respectively.
