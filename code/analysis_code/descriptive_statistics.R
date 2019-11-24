#load needed packages. make sure they are installed.
library(tidyverse)
library(dplyr)
library(visdat)
library(lubridate)
library(corrplot)
library(plyr)
library(scales)
library(wesanderson)
library(viridis)
library(gridExtra)
library(broom)
library(caret)
library(Metrics)

#load data. path is relative to project directory.
environmental_vibrio = readRDS("./data/processed_data/environmental_vibrio.rds")
irl_environmental = readRDS("./data/processed_data/irl_environmental.rds")
sle_environmental = readRDS("./data/processed_data/sle_environmental.rds")
irl_dust = readRDS("./data/processed_data/irl_dust.rds")
sle_dust = readRDS("./data/processed_data/sle_dust.rds")


###Descriptive Statistics###

#When do we see Vibrio "blooms" in the lagoon? What is the average Vibrio count? Do we see peaks? 

which.max(irl_environmental[,19])
which.min(irl_environmental[,19])

which.max(sle_environmental[,20])
which.min(sle_environmental[,20])

#Do we see "bust" conditions?


#When do we see peak AOD levels?
names(irl_dust)

#IRL
ordered_irl_dust = order(irl_dust$AOD_1020nm, decreasing=T)
head(ordered_irl_dust)
irl_dust$date[17]
irl_dust$date[28]

#SLE
ordered_sle_dust = order(sle_dust$AOD_1020nm, decreasing=T)
head(ordered_sle_dust)
sle_dust$date[16]
sle_dust$date[30]


#Are the Vibrio counts normally distributed? 

#Null hypothesis: the data are normally distributed
#Alternative hypothesis: the data are not normally distributed

shapiro.test(environmental_vibrio$raw_vib)
# p-value = 3.166e-11, reject the null hypothesis. The Vibrio counts are not normally distributed. 

#What if we log-transform the Vibrio counts? 
shapiro.test(environmental_vibrio$log_raw_vib)
#p-value = 0.3158, we can assume normality if we log-transform the Vibrio counts.

