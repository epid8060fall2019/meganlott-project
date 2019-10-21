###############################
# analysis script


#load needed packages. make sure they are installed.
library(tidyverse)
library(dplyr)
library(lubridate)
library(plyr)
library(scales)
library(wesanderson)

#load data. path is relative to project directory.
environmental_vibrio = readRDS("./data/processed_data/environmental_vibrio.rds")
irl_environmental = readRDS("./data/processed_data/irl_environmental.rds")
sle_environmental = readRDS("./data/processed_data/sle_environmental.rds")


#save figure
#ggsave(filename="./results/resultfigure.png",plot=p1) 

  
# fit linear model
#lmfit <- lm(Weight ~ Height, mydata)  

# place results from fit into a data frame with the tidy function
#lmtable <- broom::tidy(lmfit)

# save table  
#saveRDS(lmtable, file = "./results/resulttable.rds")


#Visualization

irl_environmental %>%
  filter(location_num == 3) %>%
  ggplot(aes(x = date, y = log(raw_vib))) + 
  geom_point() +
  ylim(0,8)


irl_environmental %>%
  ggplot(aes(x = date, y = raw_vib, group = location_name, color = location_name)) + 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin = raw_vib-rv_se, ymax = raw_vib + rv_se), width = 0.2) 


irl_environmental %>% 
  filter(location_name == "Titusville Pier") %>%
  ggplot(aes(x = date, y = raw_vib)) + 
  geom_point()

irl_environmental %>% 
  ggplot(aes(x = AOD_1020nm, y = raw_vib, group = location_name, color = location_name)) + 
  geom_point() 

sle_environmental %>%
  ggplot(aes(x = date, y = raw_vib, group = location_name, color = location_name)) + 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin = raw_vib-rv_se, ymax = raw_vib + rv_se), width = 0.2) 


environmental_vibrio %>%
  ggplot(aes(x = date, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  geom_line()


environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = date, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  geom_line()


###Vibrio counts###

irl_vibrio = environmental_vibrio %>%
  filter(region == 1) %>%
  ggplot(aes(x = date, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  geom_line() + 
  xlab("Date") + 
  ylab ("Vibrio Log(CFU/1mL)") + 
  ggtitle("Enumeration of Vibrio spp. from the Northern Indian River Lagoon") +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values=wes_palette("Darjeeling1"))


environmental_vibrio %>%
  filter(region == 2) %>%
  ggplot(aes(x = date, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  geom_line() + 
  xlab("Date") + 
  ylab ("Vibrio Log(CFU/1mL)") + 
  ggtitle("Enumeration of Vibrio spp. from the St. Lucie Estuary") +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values=wes_palette("Darjeeling1"))

ggsave(filename="./results/irl_vibrio.png",plot=irl_vibrio, width = 8, height = 7) 
ggsave(filename="./results/sle_vibrio.png",plot=sle_vibrio, width = 8, height = 7) 


###Vibrio vs. Aerosol###

vibrio_aod = environmental_vibrio %>% 
  ggplot(aes(x = AOD_1020nm, y = raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("CFU/1mL") + 
  ggtitle("Vibrio spp. vs. Aerosol Optical Density on Day of Sampling") +
  theme(legend.title = element_blank()) 

log_vibrio_aod = environmental_vibrio %>% 
  ggplot(aes(x = AOD_1020nm, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Vibrio spp. vs. Aerosol Optical Density on Day of Sampling") +
  theme(legend.title = element_blank()) 

vibrio_aod24_regional = environmental_vibrio %>%
  filter(previous_24 != "NA") %>%
  ggplot(aes(x = previous_24, y = log_raw_vib, group = region, color = region)) + 
  geom_point() + 
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Vibrio spp. vs. Aerosol Optical Density 24h Before Sampling") 

vibrio_aod24 = environmental_vibrio %>%
  filter(previous_24 != "NA") %>%
  ggplot(aes(x = previous_24, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  ylim(0,5) +
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Vibrio spp. vs. Aerosol Optical Density 24h Before Sampling") 


vibrio_aod48 = environmental_vibrio %>%
  filter(previous_48 != "NA") %>%
  ggplot(aes(x = previous_48, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Vibrio spp. vs. Aerosol Optical Density 48h Before Sampling") 


vibrio_aod72 = environmental_vibrio %>%
  ggplot(aes(x = previous_72, y = log_raw_vib, group = location_name, color = location_name)) + 
  geom_point() + 
  ylim(0,5) + 
  xlab("Aeorosol Optical Density at 1020nm") + 
  ylab ("Log(CFU/1mL)") + 
  ggtitle("Vibrio spp. vs. Aerosol Optical Density 72h Before Sampling")

ggsave(filename="./results/vibrio_aod.png",plot=vibrio_aod, width = 8, height = 7) 
ggsave(filename="./results/log_vibrio_aod.png",plot=log_vibrio_aod, width = 8, height = 7) 
ggsave(filename="./results/vibrio_aod24_regional.png",plot=vibrio_aod24_regional, width = 8, height = 7) 
ggsave(filename="./results/vibrio_aod24.png",plot=vibrio_aod24, width = 8, height = 7) 
ggsave(filename="./results/vibrio_aod48.png",plot=vibrio_aod48, width = 8, height = 7) 
ggsave(filename="./results/vibrio_aod72.png",plot=vibrio_aod72, width = 8, height = 7) 
