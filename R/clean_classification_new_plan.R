
#  load data and libraries:  ----------------------------------------------

library(dplyr)
library(ggplot2)
library(cowplot)

load("dat.RImage")


# dat contains combined disdrometer and MRR data 

# a total of 20979 min are included in the data set, where we have both disdrometer and MRR measurements
# those minutes are distributed over 293 events between 
min(dat$Date) # January 2017
max(dat$Date) # March 2019

# the 293 events were already classified by our expert "Enric" into 

# convective events (14)
con_ev_sel<-c(20,27, 43,102, 103,133, 141,154, 160,161, 169,176, 185,213)
# stratiform events (140)
str_ev_sel<-c(1,2,4,5,6,10,11,13,15,18,21,25,26,29,30,31,33,40,41,49,50,55,56,58,59,60,62,63,65,68,69,70,71,72,73,74,75,
              76,79,80,81,82,83,86,87,89,92,93,94,97,98,99,100,104,108,110,111,112,114,119,120,122,123,124,127,128,129,
              130,131,137,138,139,143,144,145,146,147,148,149,150,153,155,156,157,158,162,163,166,171,173,175,178,180,184,
              192,194,200,201,203,204,205,210,211,216,217,218,220,221,222,223,225,229,230,235,245,246,249,250,251,252,253,
              256,257,259,261,264,265,267,268,271,279,280,281,282,285,286,287,289,290,291)
# mixed events (44)
mix_ev_sel<-c(7,8,9,23,28,34,36,51,90,91,105,115,117,126,132,134,140,159,164,165,167,170,172,174,177,179,182,186,
              188,190,191,193,195,196,197,214,224,231,238,240,242,244,248,292)
# unknown (95)
unknown_ev_sel<-setdiff(1:293,c(con_ev_sel,str_ev_sel,mix_ev_sel))


# note that some minutes have no rain. However, they were included as short 
#   non-rainy intervals within the event 
# these do not exceed a duration of 15 min 
# in total, they sum up to 2981 minutes 




# the plan is: 
# 1- classify based on BR09 using disdrometer data  -----------------------

# we need: 
# En (event number - id),
# Date (interval),
# D0 (Median volume diameter)
# Nw (Normalized number of drops) we need the log10 of it, we already have the log10Nw as a column

# extracting the relevant columns
# the values are repeated for each height, thus we use unique because we only need one value per minute:
df_pre_clas <- unique(dat[, c("En", "Date", "D0", "logNw")])

# correct name
names(df_pre_clas)[4] <- "log10_Nw"


# the classification method of Br09 states that all points above the line:
# sep_Nw<-6.3-1.6D0
# are convective (see plot)

ggplot(df_pre_clas, aes(x = D0, y = log10_Nw)) +
  geom_point(color = "black",
             fill = "grey",
             shape = 21) +
  geom_abline(slope = -1.6,
              intercept = 6.3,
              color = "red") +
  theme_bw() +
  theme(text = element_text(face = "bold"))

# create a column of the class based on Br09
df_pre_clas$RTB_Br09 <- NA
df_pre_clas$RTB_Br09[df_pre_clas$log10_Nw >= (-1.6 * df_pre_clas$D0 + 6.3)] <-  "C"
df_pre_clas$RTB_Br09[df_pre_clas$log10_Nw < (-1.6 * df_pre_clas$D0 + 6.3)] <-  "S"


# after the classification:
ggplot(df_pre_clas, aes(x = D0, y = log10_Nw)) +
  geom_point(color = "black", aes(fill = RTB_Br09), shape = 21) +
  geom_abline(slope = -1.6,
              intercept = 6.3,
              color = "red") +
  theme_bw() +
  theme(text = element_text(face = "bold"))


# the ratio of convective to stratiform intervals:
100 * sum(df_pre_clas$RTB_Br09 == "C", na.rm = T) / sum(df_pre_clas$RTB_Br09 == "S", na.rm = T)
# ~ 10.6 % which is a reasonable ratio

# 2- compare with the subjective classification made by Enric: (sanity check, not needed for the paper) ------------

# on the interval level
# the percentage of stratiform intervals (according to BR09) within the stratiform events defined subjectively:
100 * table(df_pre_clas$RTB_Br09[df_pre_clas$En %in% str_ev_sel], useNA = "always") /
  length(df_pre_clas$RTB_Br09[df_pre_clas$En %in% str_ev_sel])
# only 2% are classified wrongly, while 15% contain no rain out of a total of
length(df_pre_clas$RTB_Br09[df_pre_clas$En %in% str_ev_sel])
# 10503 minutes of rain included in stratiform events


# the percentage of convective intervals within convective events:
100 * table(df_pre_clas$RTB_Br09[df_pre_clas$En %in% con_ev_sel], useNA = "always") /
  length(df_pre_clas$RTB_Br09[df_pre_clas$En %in% con_ev_sel])
# is only 32%, while 52% were stratiform out of
length(df_pre_clas$RTB_Br09[df_pre_clas$En %in% con_ev_sel])
# 720 minutes included in convective events
# this is expected as convection can be embedded within a stratiform event



# on the event level:
# add a column of the subjective classification:
df_pre_clas$RTB_Sub <- NA
df_pre_clas$RTB_Sub[df_pre_clas$En %in% con_ev_sel] <- "C"
df_pre_clas$RTB_Sub[df_pre_clas$En %in% str_ev_sel] <- "S"

summary_table_1 <- df_pre_clas %>%
  group_by(En) %>%
  summarise(
    event_start = min(Date),
    duration = n(),
    RTB_Sub = RTB_Sub[1],
    con_interval_dur = sum(RTB_Br09 == "C", na.rm = T),
    str_interval_dur = sum(RTB_Br09 == "S", na.rm = T),
    con_interval_percentage = 100 * con_interval_dur / duration,
    str_interval_percentage = 100 * str_interval_dur / duration
  )

summary_table_1

table(summary_table_1$RTB_Sub, useNA = "always")

ggplot(summary_table_1, aes(x = con_interval_percentage)) +
  geom_histogram(bins = 20) +
  facet_wrap( ~ RTB_Sub) +
  theme_bw()

ggplot(summary_table_1, aes(x = con_interval_percentage, y = duration)) +
  geom_point(aes(color = RTB_Sub)) +
  theme_bw()




# 3- create a dataframe which would be used for training and test --------

# in this dataframe we only need the
# id (Date and En)
# height (h)
# MRR output (Z, W, SW)
# the preclassification column based on the disdrometer from step1
# additionally I am including rain intensity and the classification by BR03 
#   to make some checks after the classification is done

pre_df <- dat[, c("Date", "En", "h", "Z_mrr", "SW", "W_mrr", "R", "RTB_BR_03")]


names(pre_df) <-
  c("Date",
    "En",
    "height",
    "Z",
    "SW",
    "W",
    "R_no_use",
    "RTB_Br_no_use")

pre_df <- merge.data.frame(pre_df,
                   df_pre_clas[, c("En", "Date", "RTB_Br09")],
                   by = c("Date", "En"),
                   all.x = T)
table(pre_df$RTB_Br09, useNA = "always")


# 4- some explanation --------------------------------------------------------

# the following function takes a date as an input
# and creates a simple plot of the MRR and the preclassification from the pre_df dataframe
# it requires the packages ggplot2 and cowplot

Mrr_plot <- function(my_date = NA,
                     En = NA,
                     half_range = 2,
                     my_df = pre_df) {
  if (!is.na(my_date)) {
    selection_df <-
      my_df[my_df$Date %in% seq(
        from = my_date - half_range * 60,
        to = my_date + half_range * 60,
        by = "mins"
      ), ]
  }
  if (!is.na(En)) {
    selection_df <- my_df[my_df$En == En, ]
  }
  
  cowplot::plot_grid(
    ggplot(selection_df, aes(x = Date, y = height)) +
      geom_tile(aes(fill = Z)) +
      scale_fill_gradient2(
        low = "blue",
        mid = "green",
        high = "red",
        na.value = NA
      ) +
      theme_bw() +
      theme(
        text = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ),
    ggplot(selection_df, aes(x = Date, y = height)) +
      geom_tile(aes(fill = W)) +
      scale_fill_gradient2(
        low = "blue",
        mid = "green",
        high = "red",
        na.value = NA
      ) +
      theme_bw() +
      theme(
        text = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ),
    ggplot(selection_df, aes(x = Date, y = height)) +
      geom_tile(aes(fill = SW)) +
      scale_fill_gradient2(
        low = "blue",
        mid = "green",
        high = "red",
        na.value = NA
      ) +
      theme_bw() +
      theme(
        text = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ),
    ggplot(selection_df, aes(x = Date, y = "type")) +
      geom_tile(aes(fill = RTB_Br09)) +
      geom_text(aes(label = round(R_no_use, 1))) +
      theme_bw() +
      theme(text = element_text(face = "bold")) +
      labs(y = NULL),
    #    theme_void(),
    nrow = 4,
    align = "hv",
    rel_heights = c(1, 1, 1, 0.5)
  )
  
}


# few examples of events
# typical stratiform
Mrr_plot(En = 60)
Mrr_plot(En = 70)

# Typical convective (there might be a delay of a couple of minutes!!)
Mrr_plot(En = 188)# the first quarter of the event
Mrr_plot(En = 197)# towards the end of first quarter
Mrr_plot(En = 292)# beginning of the last quarter of the event

# a couple of examples for dates:
# an example of a convection
Mrr_plot(my_date = dat$Date[390200])# the half range default is 2, we are hoping to get good results based on this range (convection can happen in very short duration ~ 5 minutes or even less)
Mrr_plot(my_date = dat$Date[390200], half_range = 5)# However we can also explore larger ranges
Mrr_plot(my_date = dat$Date[390200], half_range = 10)#


# an example of stratiform rain
Mrr_plot(my_date = dat$Date[157766])
Mrr_plot(my_date = dat$Date[157766], half_range = 5)#
Mrr_plot(my_date = dat$Date[157766], half_range = 10)#


# some explanation which might help the classification:

# W is the speed of meteors (rain drops, snow, or hail...) within a specific cell [m/s]
# SW is the variability of W within a specific cell [m/s]
# Z is the reflectivity, the energy reflected by the surface of meteors back to the device
# Z value is especially high in what is called the melting layer, where frozen particles start melting,
# the melting layer appears mostly in stratiform rain such as in
Mrr_plot(En = 70)
# this is because the particles are still frozen inside but filmed with a layer of melted water
# below the melting layer, usually both W and SW increase, compared to the area above the melting layer
# possible indicators of convection:
# in convective rain, turbulence, and air moving upwards and downwards may prevent the formation of the melting layer
Mrr_plot(En = 43)
Mrr_plot(En = 102)
Mrr_plot(En = 103)
Mrr_plot(En = 141)
Mrr_plot(En = 154)
Mrr_plot(En = 169)
Mrr_plot(En = 185)



# even if a melting layer exists, the turbulence may be indicated by vertical 
# traces in the values of the three variables which disturbs the continuity 
# of the Melting layer
Mrr_plot(En = 160)# 20,27, 43,102, 103,133, 141,154, 160,161, 169,176, 185,213
Mrr_plot(En = 177)# 20,27, 43,102, 103,133, 141,154, 160,161, 169,176, 185,213

# in some cases the melting layer might exist above the range of MRR
Mrr_plot(En = 176)# 20,27, 43,102, 103,133, 141,154, 160,161, 169,176, 185,213
Mrr_plot(En = 170)# 20,27, 43,102, 103,133, 141,154, 160,161, 169,176, 185,213


# in our paper, I defined something called the separation level,
# this is where the highest increase in W (speed of meteors) occurs
# then I made a summary of the three variables above and below the separation level,
# assuming that these will show different patterns in convective and stratiform rain.
# now you have a better picture of the whole dataset, and you know the concept.
# so I am leaving this step ( detection of melting layer/separation layer) for you

# this part is to keep an image of the necessary objects for the classification
# rm(list = setdiff(ls(),c("pre_df","Mrr_plot")))
# save.image(file ="C:/Barcelona_Py/new_classification_project_2021/pre_df.RImage" )



# 5- create ML methods to classify rain type  -----------------------------

# based on the input from MRR and the preclassification of BR09
# all needed variables are included in the dataframe pre_df

# in case you are starting here, you only need pre_df and the plot function is optional
# load("C:/Barcelona_Py/new_classification_project_2021/pre_df.RImage")



# 6- a summary on the machine learning performance within a set of testing events
