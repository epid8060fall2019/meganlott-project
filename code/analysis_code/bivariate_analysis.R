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
library(ggpubr)


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
png(height=1000, width=1000, file="./results/all/corr_plot_environmental.png")
corr_plot = corrplot(d_cor, method="color", tl.cex = 2, cl.cex = 2)
dev.off()

#correlation with methodological variables, like sampling time and holding time

d_cor = environmental_vibrio %>% select(raw_vib, log_raw_vib, holding_time, sample_time) %>% drop_na() %>% cor()
png(height=1000, width=1000, file="./results/all/corr_plot_methods.png")
corr_plot_methods = corrplot(d_cor, method="color", tl.cex = 2, cl.cex = 2)
dev.off()

###Linear Regression###

#Pearson’s correlation coefficient is often used to quantify the strength of the 
#linear association between two continuous variables. 

#We should examine the linear regression between variables in each basin, and then overall.

###Vibrio vs. Aerosol in the IRL###

log_vibrio_aod = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = log(AOD_1020nm), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Day of Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod24 = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = log(previous_24), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod48 = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = log(previous_48), y = log_raw_vib)) + 
  geom_point() +   
  xlab("Log(AOD at 1020nm)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("48h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod72 = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = log(previous_72), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("72h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

irl_aod_vibrio = grid.arrange(log_vibrio_aod, vibrio_aod24, vibrio_aod48, vibrio_aod72, ncol=2)
ggsave(filename="./results/irl/irl_aod_vibrio.png",plot=irl_aod_vibrio, width = 8, height = 8) 


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

irl_env_vibrio = grid.arrange(vibrio_ph, vibrio_salinity, vibrio_water_temp, vibrio_air_temp, vibrio_precip, vibrio_precip24, ncol = 2)
ggsave(filename="./results/irl/irl_env_vibrio.png",plot=irl_env_vibrio, width = 8, height = 12) 

###Vibrio vs. Aerosol in the SLE###

log_vibrio_aod = environmental_vibrio %>% 
  filter(region == 2) %>%
  ggplot(aes(x = log(AOD_1020nm), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Day of Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod24 = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = log(previous_24), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") +  
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod48 = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = log(previous_48), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("48h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod72 = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = log(previous_72), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("72h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

sle_aod_vibrio = grid.arrange(log_vibrio_aod, vibrio_aod24, vibrio_aod48, vibrio_aod72, ncol=2)
ggsave(filename="./results/sle/sle_aod_vibrio.png",plot=sle_aod_vibrio, width = 8, height = 8) 


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

sle_env_vibrio = grid.arrange(vibrio_ph, vibrio_salinity, vibrio_salinity_0_15, vibrio_salinity_15_30, vibrio_water_temp, vibrio_air_temp, vibrio_precip, vibrio_precip24, ncol = 2)
ggsave(filename="./results/sle/sle_env_vibrio.png",plot=sle_env_vibrio, width = 8, height = 16) 



###Vibrio vs. Aerosol ALL###

#LINEAR#

log_vibrio_aod = environmental_vibrio %>% 
  ggplot(aes(x = AOD_1020nm, y = log_raw_vib)) + 
  geom_point() + 
  xlab("AOD at 1020nm") + 
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
  xlab("AOD at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("AOD Density 24h Before Sampling") 

vibrio_aod24 = environmental_vibrio %>%
  ggplot(aes(x = previous_24, y = log_raw_vib)) + 
  geom_point() + 
  xlim(0,0.3)+
  xlab("AOD at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod48 = environmental_vibrio %>%
  ggplot(aes(x = previous_48, y = log_raw_vib)) + 
  geom_point() + 
  xlab("AOD at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("48h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod72 = environmental_vibrio %>%
  ggplot(aes(x = previous_72, y = log_raw_vib)) + 
  geom_point() + 
  xlab("AOD at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  xlim(0,0.3)+
  ggtitle("72h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

#LOG TRANSFORMED#

log_vibrio_aod = environmental_vibrio %>% 
  ggplot(aes(x = log(AOD_1020nm), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") +
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Day of Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod24_regional = environmental_vibrio %>%
  filter(previous_24 != "NA") %>%
  ggplot(aes(x = log(previous_24), y = log_raw_vib, color = location_id)) + 
  geom_point() + 
  ylim(2,5) + 
  xlab("Log(AOD at 1020nm)") +
  ylab ("Log(CFU/1mL)") + 
  ggtitle("AOD Density 24h Before Sampling") 

vibrio_aod24 = environmental_vibrio %>%
  ggplot(aes(x = log(previous_24), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") +
  ylim(2,5) + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("24h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod48 = environmental_vibrio %>%
  ggplot(aes(x = log(previous_48), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") +
  ylim(2,5) + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("48h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_aod72 = environmental_vibrio %>%
  ggplot(aes(x = log(previous_72), y = log_raw_vib)) + 
  geom_point() + 
  xlab("Log(AOD at 1020nm)") +
  ylim(2,5) + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("72h Before Sampling") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

log_aod = grid.arrange(log_vibrio_aod, vibrio_aod24, vibrio_aod48, vibrio_aod72, ncol=2)
ggsave(filename="./results/all/log_aod.png",plot=log_aod, width = 8, height = 8) 


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

vibrio_salinity_0_15 = environmental_vibrio %>%
  filter(salinity < 15) %>%
  ggplot(aes(x = salinity, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Salinity (ppt)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

vibrio_salinity_15_30 = environmental_vibrio %>%
  filter(salinity > 15) %>%
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

vibrio_air_temp = environmental_vibrio %>%
  ggplot(aes(x = air_temp_c, y = log_raw_vib)) + 
  geom_point() + 
  ylim(2,5) +
  xlab("Air Temperature (C)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.title = element_blank()) +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

env_var_vib = grid.arrange(vibrio_ph, vibrio_salinity,vibrio_salinity_0_15,vibrio_salinity_15_30, vibrio_temp,  vibrio_air_temp, vibrio_precip, vibrio_precip24
                       ,ncol = 2)
ggsave(filename="./results/all/env_var_vib.png",plot=env_var_vib, width = 8, height = 16) 

#Other variables, like sample time and holding time

holding_time = environmental_vibrio %>%
  ggplot(aes(x = holding_time, y = log_raw_vib)) + 
  geom_point() + 
  xlab("Holding Time (h)") + 
  ylab ("Log(CFU/1mL)") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

sample_time = environmental_vibrio %>%
  ggplot(aes(x = sample_time, y = log_raw_vib)) + 
  geom_point() + 
  ylab ("Log(CFU/1mL)") + 
  xlab("Sample Time") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

sample_time_irl = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = sample_time, y = log_raw_vib)) + 
  geom_point() + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

sample_time_sle = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = sample_time, y = log_raw_vib)) + 
  geom_point() + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

sample_time_ph = environmental_vibrio %>%
  ggplot(aes(x = sample_time, y = ph)) + 
  geom_point() + 
  ylab ("pH") + 
  xlab("Sample Time") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

sample_time_water = environmental_vibrio %>%
  ggplot(aes(x = sample_time, y = water_temp)) + 
  geom_point() + 
  ylab ("Water Temperature (C)") + 
  xlab("Sample Time") + 
  theme(legend.position = "none") +
  stat_smooth(method = "lm", col = "black", se = FALSE) +
  stat_cor(method = "pearson")

method_vib = grid.arrange(sample_time, holding_time, sample_time_ph, sample_time_water, ncol = 2)
ggsave(filename="./results/all/method_vib.png",plot= method_vib, width = 8, height = 8) 
