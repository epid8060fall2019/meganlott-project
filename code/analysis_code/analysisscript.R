###############################
# analysis script


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

#Examine Data 

visdat::vis_dat(environmental_vibrio)
#All of our continuous varaibles are saved as numeric or integers
#We're missing data from the AOD values. Let's check back on the Aeronet site before publishing.
#We're missing one Vibrio Count from the day the counts were too high. 


###Weekly Vibrio Counts###

environmental_vibrio$week = as.factor(environmental_vibrio$week)

all_vibrio = environmental_vibrio %>%
  ggplot(aes(x = week, y = log_raw_vib, group = location_id, color = location_id)) + 
  geom_point() + 
  geom_line() + 
  xlab("Sampling Period") + 
  geom_errorbar(aes(ymin = log10(raw_vib-rv_se), ymax = log10(raw_vib + rv_se)), width = 0.2) +
  ylab ("Vibrio Log(CFU/1mL)") + 
  theme(text = element_text(size = 14)) +
  scale_color_viridis(name = "Location", discrete = TRUE)

irl_vibrio = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = date, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  geom_line() + 
  xlab("Date") + 
  geom_errorbar(aes(ymin = log10(raw_vib-rv_se), ymax = log10(raw_vib + rv_se)), width = 0.2) +
  ylab ("Vibrio Log(CFU/1mL)") + 
  ggtitle("Enumeration of Vibrio spp. from the Northern Indian River Lagoon") +
  theme(axis.text = element_text(size = 10)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values=wes_palette("Darjeeling1"))


sle_vibrio = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = date, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = log10(raw_vib-rv_se), ymax = log10(raw_vib + rv_se)), width = 0.2) +
  xlab("Date") + 
  ylab ("Vibrio Log(CFU/1mL)") + 
  ggtitle("Enumeration of Vibrio spp. from the St. Lucie Estuary") +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values=wes_palette("Darjeeling1"))

ggsave(filename="./results/all_vibrio.png",plot=all_vibrio, width = 8, height = 7) 
ggsave(filename="./results/irl_vibrio.png",plot=irl_vibrio, width = 8, height = 7) 
ggsave(filename="./results/sle_vibrio.png",plot=sle_vibrio, width = 8, height = 7) 



###Environmental Variables Over Time###

#First, let's look at dust over time (AOD over time)

irl_dust_date = irl_dust %>% filter(AOD_1020nm > 0) %>%
  ggplot(aes(x = date, y = AOD_1020nm)) + 
  geom_point() + 
  xlab("Date") + 
  ylab ("AOD (1020nm)") + 
  ggtitle("Aerosol Optical Density in the Indian River Lagoon") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sle_dust_date = sle_dust %>% filter(AOD_1020nm > 0) %>%
  ggplot(aes(x = date, y = AOD_1020nm)) + 
  geom_point() + 
  xlab("Date") + 
  ylab ("AOD (1020nm)") + 
  ggtitle("Aerosol Optical Density in the St. Lucie Estuary") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dust_date = grid.arrange(irl_dust_date, sle_dust_date)
ggsave(filename="./results/dust_date.png",plot=dust_date, width = 8, height = 7) 

#Let's look at ph, salinity, and temperature over time

irl_ph_date = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = date, y = ph, group = location_id, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab ("pH") + 
  ggtitle("pH") + 
  theme(axis.text.x = element_text(angle = 45))

irl_salinity_date = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = date, y = salinity, group = location_id, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab ("Salinity (ppt)") + 
  ggtitle("Salinity") + 
  theme(axis.text.x = element_text(angle = 45))

irl_temp_date = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = date, y = water_temp, group = location_id, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab ("Temperature (C)") + 
  ggtitle("Temperature") + 
  theme(axis.text.x = element_text(angle = 45))

sle_ph_date = environmental_vibrio %>% 
  filter(region == 2) %>%
  ggplot(aes(x = date, y = ph, group = location_id, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab ("pH") + 
  ggtitle("pH") + 
  theme(axis.text.x = element_text(angle = 45))

sle_salinity_date = environmental_vibrio %>% 
  filter(region == 2) %>%
  ggplot(aes(x = date, y = salinity, group = location_id, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab ("Salinity (ppt)") + 
  ggtitle("Salinity") + 
  theme(axis.text.x = element_text(angle = 45))

sle_temp_date = environmental_vibrio %>% 
  filter(region == 2) %>%
  ggplot(aes(x = date, y = water_temp, group = location_id, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") + 
  ylab ("Temperature (C)") + 
  ggtitle("Temperature") + 
  theme(axis.text.x = element_text(angle = 45))


irl_env_date = grid.arrange(irl_ph_date, irl_salinity_date, irl_temp_date)
ggsave(filename="./results/irl_env_date.png",plot=irl_env_date, width = 10, height = 10) 


sle_env_date = grid.arrange(sle_ph_date, sle_salinity_date, sle_temp_date)
ggsave(filename="./results/sle_env_date.png", plot=sle_env_date, width = 10, height = 10) 


###Vibrio vs. Aerosol###

vibrio_aod = environmental_vibrio %>% 
  ggplot(aes(x = AOD_1020nm, y = raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("CFU/1mL") + 
  ggtitle("AOD on Day of Sampling") +
  theme(legend.title = element_blank()) 

log_vibrio_aod = environmental_vibrio %>% 
  ggplot(aes(x = AOD_1020nm, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("AOD Day of Sampling") +
  theme(legend.title = element_blank()) 

vibrio_aod24_regional = environmental_vibrio %>%
  filter(previous_24 != "NA") %>%
  ggplot(aes(x = previous_24, y = log_raw_vib, group = region, color = region)) + 
  geom_point() + 
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("AOD Density 24h Before Sampling") 

vibrio_aod24 = environmental_vibrio %>%
  filter(previous_24 != "NA") %>%
  ggplot(aes(x = previous_24, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  ylim(0,5) +
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("AOD 24h Before Sampling") 


vibrio_aod48 = environmental_vibrio %>%
  filter(previous_48 != "NA") %>%
  ggplot(aes(x = previous_48, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("AOD 48h Before Sampling") 


vibrio_aod72 = environmental_vibrio %>%
  ggplot(aes(x = previous_72, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("AOD 72h Before Sampling")

ggsave(filename="./results/vibrio_aod.png",plot=vibrio_aod, width = 8, height = 7) 
ggsave(filename="./results/log_vibrio_aod.png",plot=log_vibrio_aod, width = 8, height = 7) 
ggsave(filename="./results/vibrio_aod24_regional.png",plot=vibrio_aod24_regional, width = 8, height = 7) 
ggsave(filename="./results/vibrio_aod24.png",plot=vibrio_aod24, width = 8, height = 7) 
ggsave(filename="./results/vibrio_aod48.png",plot=vibrio_aod48, width = 8, height = 7) 
ggsave(filename="./results/vibrio_aod72.png",plot=vibrio_aod72, width = 8, height = 7) 

aod = grid.arrange(vibrio_aod, vibrio_aod24, vibrio_aod48, vibrio_aod72)
ggsave(filename="./results/aod.png",plot=aod, width = 8, height = 8) 


###Vibrio vs. Water Quality###

vibrio_ph = environmental_vibrio %>%
  ggplot(aes(x = ph, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  xlab("ph") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Effect of pH on Vibrio Abundance in the Indian River Lagoon and St. Lucie Estuary") +
  theme(legend.title = element_blank()) 

vibrio_salinity = environmental_vibrio %>%
  ggplot(aes(x = salinity, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  xlab("Salinity (ppt)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Effect of Salinity on Vibrio Abundance in the Indian River Lagoon and St. Lucie Estuary") +
  theme(legend.title = element_blank()) 

vibrio_temp = environmental_vibrio %>%
  ggplot(aes(x = water_temp, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  xlab("Temperature (C)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Effect of Water Temperature on Vibrio Abundance in the Indian River Lagoon and St. Lucie Estuary") +
  theme(legend.title = element_blank()) 

env_var = grid.arrange(vibrio_ph, vibrio_salinity, vibrio_temp)

ggsave(filename="./results/vibrio_ph.png",plot=vibrio_ph, width = 8, height = 7) 
ggsave(filename="./results/vibrio_salinity.png",plot=vibrio_salinity, width = 8, height = 7) 
ggsave(filename="./results/vibrio_temp.png",plot=vibrio_temp, width = 8, height = 7) 
ggsave(filename="./results/env_var.png",plot=env_var, width = 10, height = 10) 


###CORRELATIONS###

d_cor = environmental_vibrio %>% select(raw_vib, log_raw_vib, previous_24, previous_48, previous_72, 
                                        salinity, ph, water_temp) %>% drop_na() %>% cor()
png(height=1800, width=1800, file="./results/corr_plot.png")
corr_plot = corrplot(d_cor, method="color", tl.cex = 5)
dev.off()

###MODELING###

#Leading question: Can the variation in Vibrio counts be explained by any of the environemntal variables?


##Simple Linear Model##

# fit linear model
lm_log_prev24 = lm(log_raw_vib ~ previous_24, environmental_vibrio) 
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
d = environmental_vibrio %>% select(raw_vib, previous_24, salinity, ph, water_temp) %>% filter(raw_vib != "NA") %>% filter(previous_24 != "NA")

#this code does the data splitting.
set.seed(123)
trainset = caret::createDataPartition(y = d$raw_vib, p = 0.7, list = FALSE)
data_train = d[trainset,] #extract observations/rows for training, assign to new variable
data_test = d[-trainset,] #do the same for the test set

#Null Model
#The mean of the data
nullModel(y = data_train$raw_vib)

data_train %>%
  summarize(mean = mean(raw_vib))

##Single Predictor Model##

set.seed(1111) #makes each code block reproducible
fitControl <- trainControl(method="repeatedcv",number=5,repeats=5) #setting CV method for caret
Npred <- ncol(data_train)-1 # number of predictors
resultmat <- data.frame(Variable = names(data_train)[-1], RMSE = rep(0,Npred)) 
#store values for RMSE for each variable


for (n in 2:ncol(data_train)) #loop over each predictor. For this to work, outcome must be in 1st column
{
  fit1 <- train(as.formula(paste("raw_vib ~",names(data_train)[n])), 
                data = data_train, method = "lm", trControl = fitControl) 
  resultmat[n-1,2]= fit1$results$RMSE  
}
print(resultmat)

##Multi-Predictor Models##
set.seed(1111) #makes each code block reproducible
#write code that uses the train function in caret to fit the outcome to all predictors using the 3 methods specified.

fitControl <- trainControl(method="repeatedcv",number=5,repeats=5) #setting CV method for caret

linear_model = train(raw_vib ~ ., data = data_train, method = "lm", trControl = fitControl)
print(linear_model)
