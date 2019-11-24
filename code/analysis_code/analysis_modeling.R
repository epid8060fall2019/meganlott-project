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


###CORRELATIONS###

#correlation with environmental variables 
d_cor = environmental_vibrio %>% select(raw_vib, log_raw_vib, previous_24, previous_48, previous_72, 
                                        salinity, ph, water_temp, precipitation, precipitation_prev24, air_temp_c) %>% drop_na() %>% cor()
png(height=1800, width=1800, file="./results/all/corr_plot_environmental.png")
corr_plot = corrplot(d_cor, method="color", tl.cex = 5, cl.cex = 2)
dev.off()

#correlation with methodological variables, like sampling time and holding time

d_cor = environmental_vibrio %>% select(raw_vib, log_raw_vib, holding_time, sample_time) %>% drop_na() %>% cor()
png(height=1800, width=1800, file="./results/all/corr_plot_methods.png")
corr_plot_methods = corrplot(d_cor, method="color", tl.cex = 5, cl.cex = 2)
dev.off()

###Linear Regression###

#Pearson’s correlation coefficient is often used to quantify the strength of the 
#linear association between two continuous variables. 

#We should examine the linear regression between variables in each basin, and then overall.

###Vibrio vs. Aerosol in the IRL###

log_vibrio_aod = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = AOD_1020nm, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("Day of Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod24 = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = previous_24, y = log_raw_vib)) + 
  geom_point() + 
  xlim(0,0.3)+
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod48 = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = previous_48, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("48h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod72 = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = previous_72, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("72h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

irl_aod_vibrio = grid.arrange(log_vibrio_aod, vibrio_aod24, vibrio_aod48, vibrio_aod72, ncol=2)
ggsave(filename="./results/irl/irl_aod_vibrio.png",plot=irl_aod_vibrio, width = 10, height = 10) 


###Vibrio vs. Water Quality in the IRL###

vibrio_ph = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = ph, y = log_raw_vib)) + 
  geom_point() + 
  xlab("pH") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_salinity = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = salinity, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Salinity (ppt)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_water_temp = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = water_temp, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Water Temperature (C)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_air_temp = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = air_temp_c, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Air Temperature (C)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_precip = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = precipitation, y = log_raw_vib)) + 
  geom_point() + 
  ylim(2,5) +
  xlim(0,2.5) +
  xlab("Precipitation (in)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Day of Sampling") +
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_precip24 = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = precipitation_prev24, y = log_raw_vib)) + 
  geom_point() + 
  ylim(2,5) +
  xlim(0,2.5) +
  xlab("Precipitation (in)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24hr Before Sampling") +
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

irl_env_vibrio = grid.arrange(vibrio_ph, vibrio_salinity, vibrio_water_temp, vibrio_air_temp, vibrio_precip, vibrio_precip24, ncol = 1)
ggsave(filename="./results/irl/irl_env_vibrio.png",plot=irl_env_vibrio, width = 6, height = 24) 

###Vibrio vs. Aerosol in the SLE###

log_vibrio_aod = environmental_vibrio %>% 
  filter(region == 2) %>%
  ggplot(aes(x = AOD_1020nm, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("Day of Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod24 = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = previous_24, y = log_raw_vib)) + 
  geom_point() + 
  xlim(0,0.3)+
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod48 = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = previous_48, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("48h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod72 = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = previous_72, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("72h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

sle_aod_vibrio = grid.arrange(log_vibrio_aod, vibrio_aod24, vibrio_aod48, vibrio_aod72, ncol=2)
ggsave(filename="./results/sle/sle_aod_vibrio.png",plot=sle_aod_vibrio, width = 10, height = 10) 


###Vibrio vs. Water Quality in the SLE###

vibrio_ph = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = ph, y = log_raw_vib)) + 
  geom_point() + 
  xlab("pH") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_salinity = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = salinity, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Salinity (ppt)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_salinity_0_15 = environmental_vibrio %>%
  filter(region == 2) %>%
  filter(salinity < 15) %>%
  ggplot(aes(x = salinity, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Salinity (ppt)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_salinity_15_30 = environmental_vibrio %>%
  filter(region == 2) %>%
  filter(salinity > 15) %>%
  ggplot(aes(x = salinity, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Salinity (ppt)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_water_temp = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = water_temp, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Water Temperature (C)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_air_temp = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = air_temp_c, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Air Temperature (C)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_precip = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = precipitation, y = log_raw_vib)) + 
  geom_point() + 
  ylim(2,5) +
  xlim(0,2.5) +
  xlab("Precipitation (in)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Day of Sampling") +
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_precip24 = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = precipitation_prev24, y = log_raw_vib)) + 
  geom_point() + 
  ylim(2,5) +
  xlim(0,2.5) +
  xlab("Precipitation (in)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24hr Before Sampling") +
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

sle_env_vibrio = grid.arrange(vibrio_ph, vibrio_salinity, vibrio_salinity_0_15, vibrio_salinity_15_30, vibrio_water_temp, vibrio_air_temp, vibrio_precip, vibrio_precip24, ncol = 1)
ggsave(filename="./results/sle/sle_env_vibrio.png",plot=sle_env_vibrio, width = 6, height = 24) 



###Vibrio vs. Aerosol ALL###

log_vibrio_aod = environmental_vibrio %>% 
  ggplot(aes(x = AOD_1020nm, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("Day of Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod24_regional = environmental_vibrio %>%
  filter(previous_24 != "NA") %>%
  ggplot(aes(x = previous_24, y = log_raw_vib, color = location_id)) + 
  geom_point() + 
  ylim(2,5) + 
  xlim(0,0.3) +
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("AOD Density 24h Before Sampling") 

vibrio_aod24 = environmental_vibrio %>%
  ggplot(aes(x = previous_24, y = log_raw_vib)) + 
  geom_point() + 
  xlim(0,0.3)+
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod48 = environmental_vibrio %>%
  ggplot(aes(x = previous_48, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("48h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod72 = environmental_vibrio %>%
  ggplot(aes(x = previous_72, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("72h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

ggsave(filename="./results/all/vibrio_aod.png",plot=vibrio_aod, width = 8, height = 7) 
ggsave(filename="./results/all/log_vibrio_aod.png",plot=log_vibrio_aod, width = 8, height = 7) 
ggsave(filename="./results/all/vibrio_aod24_regional.png",plot=vibrio_aod24_regional, width = 8, height = 7) 
ggsave(filename="./results/all/vibrio_aod24.png",plot=vibrio_aod24, width = 8, height = 7) 
ggsave(filename="./results/all/vibrio_aod48.png",plot=vibrio_aod48, width = 8, height = 7) 
ggsave(filename="./results/all/vibrio_aod72.png",plot=vibrio_aod72, width = 8, height = 7) 

aod = grid.arrange(log_vibrio_aod, vibrio_aod24, vibrio_aod48, vibrio_aod72, ncol=2)
ggsave(filename="./results/all/aod.png",plot=aod, width = 10, height = 10) 


###Vibrio vs. Water Quality###

vibrio_ph = environmental_vibrio %>%
  ggplot(aes(x = ph, y = log_raw_vib)) + 
  geom_point() + 
  xlab("pH") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_salinity = environmental_vibrio %>%
  ggplot(aes(x = salinity, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Salinity (ppt)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_temp = environmental_vibrio %>%
  ggplot(aes(x = water_temp, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Water Temperature (C)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")


env_var = grid.arrange(vibrio_ph, vibrio_salinity, vibrio_temp, ncol = 3)

ggsave(filename="./results/all/vibrio_ph.png",plot=vibrio_ph, width = 8, height = 7) 
ggsave(filename="./results/all/vibrio_salinity.png",plot=vibrio_salinity, width = 8, height = 7) 
ggsave(filename="./results/all/vibrio_temp.png",plot=vibrio_temp, width = 8, height = 7) 
ggsave(filename="./results/all/env_var.png",plot=env_var, width = 12, height = 3) 


vibrio_precip = environmental_vibrio %>%
  ggplot(aes(x = precipitation, y = log_raw_vib)) + 
  geom_point() + 
  ylim(2,5) +
  xlim(0,2.5) +
  xlab("Precipitation (in)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Day of Sampling") +
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_precip24 = environmental_vibrio %>%
  ggplot(aes(x = precipitation_prev24, y = log_raw_vib)) + 
  geom_point() + 
  ylim(2,5) +
  xlim(0,2.5) +
  xlab("Precipitation (in)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24hr Before Sampling") +
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

precip_vibrio = grid.arrange(vibrio_precip, vibrio_precip24, ncol = 1)
ggsave(filename="./results/precip_vibrio.png",plot=precip_vibrio, width = 6, height = 8) 

#Other variables, like sample time and holding time

holding_time = environmental_vibrio %>%
  ggplot(aes(x = holding_time, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Holding Time") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

holding_time

sample_time = environmental_vibrio %>%
  ggplot(aes(x = sample_time, y = log_raw_vib)) + 
  geom_point() + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

ggsave(filename="./results/all/sample_time.png",plot= sample_time, width = 6, height = 8) 

sample_time_irl = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = sample_time, y = log_raw_vib)) + 
  geom_point() + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

ggsave(filename="./results/irl/sample_time_irl.png",plot=sample_time_irl, width = 6, height = 8) 

sample_time_sle = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = sample_time, y = log_raw_vib)) + 
  geom_point() + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

ggsave(filename="./results/sle/sample_time_sle.png",plot=sample_time_sle, width = 6, height = 8) 




#ordinary least squares (OLS) regression

model1 <- lm(previous_24 ~ log_raw_vib, data = environmental_vibrio)
summary(model1)

#0.02631


#Multiple Linear Regression (MLR)
model2 <- lm(log_raw_vib ~ AOD_1020nm + sample_time, data = environmental_vibrio)
summary(model2)



ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

fit1 <- lm(log_raw_vib ~ previous_24, data = environmental_vibrio)
ggplotRegression(fit1)




#use RMSE and cross-validation 

caret::train()

###MODELING###

#Leading question: Can the variation in Vibrio counts be explained by any of the environemntal variables?


##Simple Linear Model##

# fit linear model
lm_log_prev24 = lm(previous_24~log_raw_vib, environmental_vibrio) 
lm_prev24 = lm(raw_vib ~ previous_24, environmental_vibrio)  
lm_ph = lm(raw_vib ~ ph, environmental_vibrio)  
lm_salinity = lm(raw_vib ~ salinity, environmental_vibrio)  
lm_water_temp = lm(raw_vib ~ water_temp, environmental_vibrio)  

# place results from fit into a data frame with the tidy function
lm_log_prev24 = broom::tidy(lm_log_prev24)
lm_prev24 = broom::tidy(lm_prev24)
lm_ph = broom::tidy(lm_ph)
lm_salinity = broom::tidy(lm_salinity)
lm_water_temp = broom::tidy(lm_water_temp)

lm_variables = bind_rows(lm_prev24, lm_ph, lm_salinity, lm_water_temp)
lm_variables = lm_variables %>% filter(term != "(Intercept)")

# save table  
saveRDS(lm_variables, file = "./results/resulttable.rds")


##Continuous Outcome Analysis##

#Let's only look at the varaibles (predictors) that have as much data as possible.Need to drop observations with "NA"
d = environmental_vibrio %>% select(log_raw_vib, previous_24, salinity, ph, water_temp, sample_time, precipitation, precipitation_prev24) %>% filter(log_raw_vib != "NA") %>% filter(previous_24 != "NA")

#this code does the data splitting.
set.seed(123)
trainset = caret::createDataPartition(y = d$log_raw_vib, p = 0.7, list = FALSE)
data_train = d[trainset,] #extract observations/rows for training, assign to new variable
data_test = d[-trainset,] #do the same for the test set

#Null Model
#The mean of the data
nullModel(y = data_train$log_raw_vib)

data_train %>%
  summarize(mean = mean(log_raw_vib))

##Single Predictor Model##

set.seed(1111) #makes each code block reproducible
fitControl <- trainControl(method="repeatedcv",number=5,repeats=5) #setting CV method for caret
Npred <- ncol(data_train)-1 # number of predictors
resultmat <- data.frame(Variable = names(data_train)[-1], RMSE = rep(0,Npred)) 
#store values for RMSE for each variable


for (n in 2:ncol(data_train)) #loop over each predictor. For this to work, outcome must be in 1st column
{
  fit1 <- train(as.formula(paste("log_raw_vib ~",names(data_train)[n])), 
                data = data_train, method = "lm", trControl = fitControl) 
  resultmat[n-1,2]= fit1$results$RMSE  
}
print(resultmat)

##Multi-Predictor Models##
set.seed(1111) #makes each code block reproducible
#write code that uses the train function in caret to fit the outcome to all predictors using the 3 methods specified.

fitControl <- trainControl(method="repeatedcv",number=5,repeats=5) #setting CV method for caret

linear_model = train(log_raw_vib ~ ., data = data_train, method = "lm", trControl = fitControl)
print(linear_model)


#bootstrap aggregating 