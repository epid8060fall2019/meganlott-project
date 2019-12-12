###############################
# visualization

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

#load data. path is relative to project directory.
environmental_vibrio = readRDS("./data/processed_data/environmental_vibrio.rds")
irl_environmental = readRDS("./data/processed_data/irl_environmental.rds")
sle_environmental = readRDS("./data/processed_data/sle_environmental.rds")
irl_dust = readRDS("./data/processed_data/irl_dust.rds")
sle_dust = readRDS("./data/processed_data/sle_dust.rds")
irl_dust_vibrio = readRDS("./data/processed_data/irl_dust_vibrio.rds")
sle_dust_vibrio = readRDS("./data/processed_data/sle_dust_vibrio.rds")


#Examine Data 

visdat::vis_dat(environmental_vibrio)
#All of our continuous varaibles are saved as numeric or integers
#We're missing data from the AOD values. Let's check back on the Aeronet site before publishing.
#We're missing one Vibrio Count from the day the counts were too high. 


###Weekly Vibrio Counts###

all_vibrio = environmental_vibrio %>%
  filter(log_raw_vib != "NA") %>%
  ggplot(aes(x = date, y = log_raw_vib, color = location_id)) + 
  geom_point() + 
  geom_line() + 
  xlab("Date") + 
  geom_errorbar(aes(ymin = log10(raw_vib-rv_se), ymax = log10(raw_vib + rv_se)), width = 0.2) +
  ylab ("Vibrio Log(CFU/1mL)") + 
  theme(text = element_text(size = 14)) +
  scale_color_viridis(name = "Location", discrete = TRUE)

irl_vibrio = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = date, y = log_raw_vib, group = location_name, color = location_id)) + 
  geom_point() + 
  geom_line() + 
  xlab("Date") + 
  geom_errorbar(aes(ymin = log10(raw_vib-rv_se), ymax = log10(raw_vib + rv_se)), width = 0.2) +
  ylab ("Vibrio Log(CFU/1mL)") + 
  ggtitle("Enumeration of Vibrio spp. from the Northern Indian River Lagoon") +
  theme(axis.text = element_text(size = 10)) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values=wes_palette("Darjeeling1")) +
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) +
  ylim(0, 5)

sle_vibrio = environmental_vibrio %>%
  filter(log_raw_vib != "NA") %>%
  filter(region == 2) %>%
  ggplot(aes(x = date, y = log_raw_vib, color = location_id)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = log10(raw_vib-rv_se), ymax = log10(raw_vib + rv_se)), width = 0.2) +
  xlab("Date") + 
  ylab ("Vibrio Log(CFU/1mL)") + 
  ggtitle("Enumeration of Vibrio spp. from the St. Lucie Estuary") +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values=wes_palette("Cavalcanti1")) + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) +
  ylim(0, 5)

ggsave(filename="./results/all/all_vibrio.png",plot=all_vibrio, width = 8, height = 7) 

all_vibrio2 = grid.arrange(irl_vibrio, sle_vibrio, ncol = 1)
ggsave(filename="./results/all/all_vibrio2.png", plot=all_vibrio2, height = 12, width = 12) 

ggsave(filename="./results/irl/irl_vibrio.png",plot=irl_vibrio, width = 8, height = 7) 
ggsave(filename="./results/sle/sle_vibrio.png",plot=sle_vibrio, width = 8, height = 7) 



###Environmental Variables Over Time###

#First, let's look at dust over time (AOD over time)


irl_dust_date = irl_dust %>% filter(AOD_1020nm > 0) %>%
  ggplot(aes(x = date, y = log(AOD_1020nm))) + 
  geom_line() + 
  xlab("Date") + 
  ylab ("Log(AOD at 1020nm)") + 
  ggtitle("Aerosol Optical Density in the Indian River Lagoon") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


sle_dust_date = sle_dust %>% filter(AOD_1020nm > 0) %>%
  ggplot(aes(x = date, y = log(AOD_1020nm))) + 
  geom_line() + 
  xlab("Date") + 
  ylab ("Log(AOD at 1020nm)") + 
  ggtitle("Aerosol Optical Density in the St. Lucie Estuary") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dust_date = grid.arrange(irl_dust_date, sle_dust_date)
ggsave(filename="./results/all/dust_date.png",plot=dust_date, width = 8, height = 7) 

#Let's look at environmental variables over time. We have dust, ph, salinity, temperature, precipitation, and 
# air temperature over time

###IRL ENVIRONMENTAL VARIABLES###

irl_vibrio2 = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = date, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = log10(raw_vib-rv_se), ymax = log10(raw_vib + rv_se)), width = 0.2) +
  ylab ("Vibrio Log(CFU/1mL)") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(legend.position = "none") +
  xlab("") +
  scale_color_manual(values=wes_palette("Darjeeling1")) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  theme(text = element_text(size=20))


 irl_dust_date2 = irl_dust %>% filter(AOD_1020nm > 0) %>%
  ggplot(aes(x = date, y = log(AOD_1020nm))) + 
  geom_line() + 
  ylab ("Log(AOD at 1020nm)") + 
  xlab("") +
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  theme(text = element_text(size=20)) 


irl_salinity_date = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = date, y = salinity, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") +
  ylab ("Salinity (ppt)") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(legend.position ="bottom", legend.box = "horizontal", legend.title = element_blank()) +
  scale_color_manual(values=wes_palette("Darjeeling1")) + 
  theme(text = element_text(size=20)) +
  scale_y_continuous(labels = scales::number_format(accuracy = ))


irl_precip_date = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = date, y = precipitation)) + 
  geom_point() +
  geom_line() +
  ylab ("Precipitation (in)") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  xlab ("") +
  scale_color_manual(values=wes_palette("Darjeeling1")) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(text = element_text(size=20))


irl_ph_date = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = date, y = ph, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab("") +
  ylab ("pH") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  scale_color_manual(values=wes_palette("Darjeeling1")) +
  theme(text = element_text(size=20))


irl_temp_date = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = date, y = water_temp, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab ("Date") +
  ylab ("Water Temperature (C)") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(legend.position ="bottom", legend.box = "horizontal", legend.title = element_blank()) +
  scale_color_manual(values=wes_palette("Darjeeling1")) +
  theme(text = element_text(size=20))


irl_env_date = grid.arrange(irl_vibrio2, irl_ph_date, irl_dust_date2, irl_precip_date, irl_salinity_date, irl_temp_date, ncol = 2)
ggsave(filename="./results/irl/irl_env_date.png", plot=irl_env_date, height = 18, width = 20) 


#Notes: We see some strange patterns with the environmental varaibles that makes it seem like there are 
#fluxuating conditions, as though they were tidally influences. Let's look at the time of sample collection.

irl_sample_time = environmental_vibrio %>% 
  filter(region == 1) %>%
  ggplot(aes(x = date, y = sample_time, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab("Date") +
  ylab ("Time") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(legend.position ="bottom", legend.box = "horizontal") +
  scale_color_manual(values=wes_palette("Darjeeling1"))

ggsave(filename="./results/irl/irl_sample_time.png", plot=irl_sample_time, height = 4, width = 4) 

#We could certainly see that the variation in sample time could influence these varaibles. Let's make a note 
#and explore correlations later. 

###SLE ENVIRONMENTAL VARIABLES###


sle_vibrio2 = environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = date, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = log10(raw_vib-rv_se), ymax = log10(raw_vib + rv_se)), width = 0.2) +
  ylab ("Vibrio Log(CFU/1mL)") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(legend.position = "none") +
  xlab("") +
  scale_color_manual(values=wes_palette("Cavalcanti1")) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+ 
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))



sle_dust_date2 = sle_dust %>% filter(AOD_1020nm > 0) %>%
  ggplot(aes(x = date, y = log(AOD_1020nm))) + 
  geom_line() + 
  ylab ("Log(AOD at 1020nm)") + 
  xlab("") +
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  theme(text = element_text(size=20)) +
  scale_y_continuous(limits = c(-4, -1), labels = scales::number_format(accuracy = 1)) 



sle_salinity_date = environmental_vibrio %>% 
  filter(region == 2) %>%
  ggplot(aes(x = date, y = salinity, color = location_id)) + 
  geom_point() + 
  geom_line() +
  ylab ("Salinity (ppt)") + 
  xlab("Date") +
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(legend.position ="bottom", legend.box = "horizontal", legend.title = element_blank()) +
  scale_color_manual(values=wes_palette("Cavalcanti1")) + 
  theme(text = element_text(size=20)) +
  scale_y_continuous(labels = scales::number_format(accuracy = ))



sle_precip_date = environmental_vibrio %>% 
  filter(region == 2) %>%
  ggplot(aes(x = date, y = precipitation)) + 
  geom_point() +
  geom_line() +
  ylab ("Precipitation (in)") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  xlab ("") +
  scale_color_manual(values=wes_palette("Cavalcanti1")) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(text = element_text(size=20)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))



sle_ph_date = environmental_vibrio %>% 
  filter(region == 2) %>%
  ggplot(aes(x = date, y = ph, color = location_id)) + 
  geom_point() + 
  geom_line() +
  ylab ("pH") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  xlab ("") +
  scale_color_manual(values=wes_palette("Cavalcanti1")) +
  theme(text = element_text(size=20))



sle_temp_date = environmental_vibrio %>% 
  filter(region == 2) %>%
  ggplot(aes(x = date, y = water_temp, color = location_id)) + 
  geom_point() + 
  geom_line() +
  xlab ("Date") +
  ylab ("Water Temperature (C)") + 
  scale_x_date(limits = as.Date(c('2019-06-05','2019-07-31'))) + 
  theme(legend.position ="bottom", legend.box = "horizontal", legend.title = element_blank()) +
  scale_color_manual(values=wes_palette("Cavalcanti1")) +
  theme(text = element_text(size=20))


sle_env_date = grid.arrange(sle_vibrio2, sle_ph_date, sle_dust_date2, sle_precip_date, sle_salinity_date, sle_temp_date, ncol = 2)
ggsave(filename="./results/sle/sle_env_date.png", plot=sle_env_date, height = 18, width = 20) 


#Let's combine vibrio over time with dust over time. 

###IRL###

#plot showing dust over time
p = irl_dust_vibrio %>% filter(AOD_1020nm.x > 0) %>% ggplot(aes(x = date))
p = p + geom_point(aes(y = log_raw_vib, color = location_name))
p = p + geom_line(aes(y = log_raw_vib, color = location_name)) 
p = p + geom_line(aes(y = log(AOD_1020nm.x)+2))
p = p + scale_y_continuous(sec.axis = sec_axis(~.-2, name = "Log(AOD at 1020nm)")) + xlab("Date") + ylab("Vibrio Log(CFU/1mL)") + scale_color_manual(values=wes_palette("Darjeeling1"))
p

ggsave(filename="./results/irl/irl_dust_vibrio.png", plot=p, height = 8, width = 8) 


###SLE###

s = sle_dust_vibrio %>% filter(AOD_1020nm.x > 0) %>% ggplot(aes(x = date))
s = s + geom_point(aes(y = log_raw_vib, color = location_name))
s = s + geom_line(aes(y = log_raw_vib, color = location_name)) 
s = s + geom_line(aes(y = log(AOD_1020nm.x)+2))
s = s + scale_y_continuous(sec.axis = sec_axis(~.-2, name = "Log(AOD at 1020nm)")) + xlab("Date") + ylab("Vibrio Log(CFU/1mL)") + scale_color_manual(values=wes_palette("Cavalcanti1"))
s

ggsave(filename="./results/sle/sle_dust_vibrio.png", plot=s, height = 8, width = 8) 

#At some point, I need to come back to this and change location_name to location_id
