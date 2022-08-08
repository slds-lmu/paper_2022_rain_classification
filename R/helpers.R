# function to summarize raw data with domain logic of melting layer
# update for new dataset: exclude heights with NA and impute directly
get_ML_V2 <- function(my_date, pre_df) {
  # for a quick test
  # my_date<-pre_df$Date[4000] # for a test
  #print(my_date)
  #browser()
  # get the 5 min window
  my_df <-
    pre_df[(pre_df$Date > my_date - 125) &
             (pre_df$Date < my_date + 125), 
           ] # 125 seconds is slightly over 2 min ;)
  
  # don't use data if more than 70% of 5min window is missing
  if(sum(is.na(my_df)) > length(unique(pre_df$height))*length(unique(my_df$Date))*3*0.7) return()
  else {
    for(feat in c("W","Z","SW")){
      df_sub = dcast(my_df, En + height ~ Date, value.var = feat)
      colnames(df_sub)[3:ncol(df_sub)] = paste0(feat,colnames(df_sub)[3:ncol(df_sub)])
      if(feat == "W") my_df_wide = df_sub
      else my_df_wide = merge(my_df_wide, df_sub, by.x = c("En","height"), by.y = c("En","height"))
    }
    my_df_wide = my_df_wide[order(my_df_wide$height),]
    
    # remove higher heights that contain NAs for the regarded time window
    ind_nas = which(is.na(rowMeans(my_df_wide[,3:ncol(my_df_wide)], na.rm = TRUE)))
    ind_diff = which(!(1:nrow(my_df_wide)) %in% ind_nas)
    if(nrow(my_df_wide) %in% ind_nas) my_df_wide = my_df_wide[1:max(ind_diff),]
    else my_df_wide = my_df_wide
    
    dt = melt(my_df_wide, id.vars = c("En", "height"))
    dt$var = NA
    dt$Date = NA
    for(feat in c("W","Z","SW")){
      dt$var[which(substr(dt$variable,1,nchar(feat))==feat)] = feat
      dt$Date[which(substr(dt$variable,1,nchar(feat))==feat)] = (substring(dt$variable[which(substr(dt$variable,1,nchar(feat))==feat)],(nchar(feat)+1)))
    }
    dt = dt[,-which(names(dt)=="variable")]
    my_df = reshape2::dcast(data = dt, formula = En + Date + height ~ var, value.var = "value")
    
    my_df_heights = reshape_long_to_wide(my_df,c("W","Z","SW"))
    
    # impute NAs that have not been excluded before
    features = colnames(my_df_heights)[3:ncol(my_df_heights)]
    while(any(is.na(my_df_heights[,features]))){
      my_df_heights = impute_missings(my_df_heights, features)
    }
    my_df = reshape_wide_to_long(my_df_heights, c("W","Z","SW"))
    
    # mean and sd parameters for each 5 min and height
    V1 <- my_df %>%
      dplyr::group_by(height) %>%
      dplyr::summarise(
        Date = my_date,
        W = mean(W, na.rm = T),
        Z = mean(Z, na.rm = T),
        SW = mean(SW, na.rm = T),
        W_sd = sd(W, na.rm = T),
        Z_sd = sd(Z, na.rm = T),
        SW_sd = sd(SW, na.rm = T)
      )
    
    
    # if the order is not correct by height, the function would be rubbish
    V1 <- V1[order(V1$height, decreasing = T), ]
    
    # the increase in velocity (W) when we move 200 m downwards (the cell below - the cell above)
    V1$W_lag <-
      c(c(V1$W, NA, NA) - c(NA, NA, V1$W))[2:(length(V1$W) + 1)] # the first and last values will always be NA
    
    # the height with the maximum increase
    SL1 <-
      V1$height[which.max(V1$W_lag)] # the separation level is where the velocity increases the moset
    
    if (length(SL1) == 0) {
      SL1 <- NA
    }
    
    
    # get the average values above and below SL1 and for the whole column
    
    my_df$h_relative <- NA
    my_df$h_relative[my_df$height > SL1] <- "upper"
    my_df$h_relative[my_df$height < SL1] <- "lower"
    
    V2 <- my_df %>%
      summarise(
        Date = my_date,
        SL = SL1,
        W_upper = mean(W[(h_relative == "upper")], na.rm = T),
        Z_upper = mean(Z[h_relative == "upper"], na.rm = T),
        SW_upper = mean(SW[h_relative == "upper"], na.rm = T),
        W_lower = mean(W[h_relative == "lower"], na.rm = T),
        Z_lower = mean(Z[h_relative == "lower"], na.rm = T),
        SW_lower = mean(SW[h_relative == "lower"], na.rm = T),
        W_all = mean(W, na.rm = T),
        Z_all = mean(Z, na.rm = T),
        SW_all = mean(SW, na.rm = T),
        W_upper_sd = sd(W[(h_relative == "upper")], na.rm = T),
        Z_upper_sd = sd(Z[h_relative == "upper"], na.rm = T),
        SW_upper_sd = sd(SW[h_relative == "upper"], na.rm = T),
        W_lower_sd = sd(W[h_relative == "lower"], na.rm = T),
        Z_lower_sd = sd(Z[h_relative == "lower"], na.rm = T),
        SW_lower_sd = sd(SW[h_relative == "lower"], na.rm = T),
        W_all_sd = sd(W, na.rm = T),
        Z_all_sd = sd(Z, na.rm = T),
        SW_all_sd = sd(SW, na.rm = T)
      )
    
    
    
    return(V2)
  }
}


# PIA adjustments
pia_adjustment = function(df, features){
  dates = df$Date[which(df$PIA==10)]
  for(date in unique(dates)){
    df_sub = df[df$Date==date,]
    indices = which(df$Date==date)
    if(length(which(df_sub$PIA==10)) == 0) df_sub = df_sub
    else{
      ind = which(df_sub$PIA == 10)[1]
      df_sub[ind:nrow(df_sub), features] = NA
    }
    df[indices,] = df_sub
  }
  return(df)
}



# imputation of missing feature values 
impute_missings = function(df, features){
  # create list with Z, W and SW features
  list = list(features[1:(length(features)/3)], 
              features[(length(features)/3 + 1):(2*length(features)/3)],
              features[(2*length(features)/3+1):length(features)])
  ## LB: I wouldn't call it "list" since there is already the function list()
  ##  I know R doesn't complain, but still ;)
  for(j in 1:length(list)){ # loop through Z, W and SW
   # for(k in unique(df$En)){ # loop through each event
      data.sub = df[,list[[j]]]
      ## LB: The notation x.y always reminds me of an object x with an attribute
      ##  y or a method y(). Again, R doesn't care, it's perhaps more of a personal
      ##  taste. But in python you can't do it, and underscores are always ok :)
      features.sub = colnames(data.sub)
      for(feat in 1:length(features.sub)){ # loop through heights of each variable
        ind.na.feat = which(is.na(data.sub[,feat]))
        for(i in ind.na.feat){ # loop through missing values to impute them
          
          if(i==1&feat==1) {
            data.sub[i,feat] = mean(as.numeric(unlist(data.sub[(i):(i+1),(feat):(feat+1)])), na.rm = TRUE) 
            #if(is.na(data.sub[i,feat])) data.sub[i,feat] = mean(as.numeric(unlist(data.sub[(i):(i+3),(feat):(feat+3)])), na.rm = TRUE) 
          }
          else if(i==1 & feat %in% 2:(length(features.sub)-1)) {
            data.sub[i,feat] = mean(as.numeric(unlist(data.sub[(i):(i+1),(feat-1):(feat+1)])), na.rm = TRUE)
          }
          else if(i > 1 & feat == 1) {
            data.sub[i,feat] = mean(as.numeric(unlist(data.sub[(i-1):(i+1),(feat):(feat+1)])), na.rm = TRUE)
          }
          else if(i == 1 & feat == length(features.sub)) {
            data.sub[i,feat] = mean(as.numeric(unlist(data.sub[(i):(i+1),(feat-1):(feat)])), na.rm = TRUE)  
          }
          else if(i > 1 & feat == length(features.sub)) {
            data.sub[i,feat] = mean(as.numeric(unlist(data.sub[(i-1):(i+1),(feat-1):(feat)])), na.rm = TRUE)  
          }
          else if(any(!is.na(data.sub[(i-1):(i+1),(feat-1):(feat+1)]))) {
            data.sub[i,feat] = mean(as.numeric(unlist(data.sub[(i-1):(i+1),(feat-1):(feat+1)])), na.rm = TRUE)
          }
          
        }
      }
      df[,list[[j]]] = data.sub
    #}
    
  }
  return(df)
}




# reshape functions
reshape_wide_to_long = function(df, features){
  if(is.null(df$RTB_Br09)) {
    id = c("En", "Date")
    formula = En + Date + height ~ var
  } 
  else {
    id = c("En", "RTB_Br09","Date")
    formula = En + RTB_Br09 + Date + height ~ var
  }
  dt = melt(df, id.vars = id)
  dt$var = NA
  dt$height = NA
  for(feat in features){
    dt$var[which(substr(dt$variable,1,nchar(feat))==feat)] = feat
    dt$height[which(substr(dt$variable,1,nchar(feat))==feat)] = as.numeric(substring(dt$variable[which(substr(dt$variable,1,nchar(feat))==feat)],(nchar(feat)+1)))
  }
  dt = dt[,-which(names(dt)=="variable")]
  dt = reshape2::dcast(data = dt, formula = formula, value.var = "value")
  return(dt)
}



reshape_long_to_wide <- function(pre_df, features){
  library(reshape2)
  #features = c("Z","W","SW")
  if(is.null(pre_df$RTB_Br09)) {
    id = c("En", "Date")
    formula = En + Date ~ height
  } 
  else {
    id = c("En", "RTB_Br09","Date")
    formula = En + RTB_Br09 + Date ~ height
  }
  
  for(feat in features){
    df_sub = dcast(pre_df, formula, value.var = feat)
    colnames(df_sub)[(length(id)+1):ncol(df_sub)] = paste0(feat,colnames(df_sub)[(length(id)+1):ncol(df_sub)])
    if(feat == features[1]) df = df_sub
    else df = merge(df, df_sub, by.x = id, by.y = id)
  }
  return(df)
}


