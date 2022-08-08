library(gridExtra)
library(ggplot2)
library(reshape2)
library(checkmate)
library(dplyr)


#source("R/clean_classification_new_plan.R")
load("data/pre_df_may.RData")
source("R/helpers.R")



# data overview and handling of missing data

# subset pre_df with relevant variables 
var_rel = c("Date", "En", "height", "Z_n", "W_n", "SW_n", "PIA", "RTB_Br09")
pre_df = pre_df[, var_rel]

# adjust Z, W, SW values according to PIA: set all values of Z, W and SW to NA when PIA is equal to 10 (for same height and higher heights)
pre_df_adj = pia_adjustment(pre_df, c("Z_n", "W_n", "SW_n"))


# reshape pre_df to wide format
variables = colnames(pre_df_adj)[4:6]
df = reshape_long_to_wide(pre_df_adj, variables)


# reshape and rename data
dt = reshape_wide_to_long(df, variables)
colnames(dt)[which(colnames(dt) %in% c("SW_n","W_n", "Z_n"))] = c("SW", "W", "Z")
dt = dt[,-2]

# exclude higher heigts with na, impute only the rest and aggregated only over those
SLs = do.call(rbind, lapply(unique(dt$Date)[5000:7500],
                            function(my_date)
                              return(data.frame(
                                get_ML_V2(my_date = my_date, pre_df = dt)
                              ))))


# # combine raw data with aggregated data - 177 events left
df_combined <- merge.data.frame(df[,c("Date","En","RTB_Br09","Z_n100","Z_n200","Z_n300")],SLs,by = c("Date"),all.y = T)
df_combined = df_combined[!is.na(df_combined$RTB_Br09),]


# save data for modelling
saveRDS(df_combined, "data/df_newdataprep2.rds")

