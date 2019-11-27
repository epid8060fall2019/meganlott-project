#load needed packages. make sure they are installed.
library(tidyverse)
library(dplyr)
library(plyr)

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

#summarize all environmental variables by region
env_var_region = environmental_vibrio %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarise(meansal = mean(salinity, na.rm = TRUE), minsal = min(salinity, na.rm = TRUE), maxsal = max(salinity, na.rm = TRUE), 
                   meantemp = mean(water_temp, na.rm = TRUE), mintemp = min(water_temp, na.rm = TRUE), maxtemp = max(water_temp, na.rm = TRUE), 
                   meanph = mean(ph, na.rm = TRUE), minph = min(ph, na.rm = TRUE), maxph = max(ph, na.rm = TRUE), 
                   meanair = mean(air_temp_c, na.rm = TRUE), minair = min(air_temp_c, na.rm = TRUE), maxair = max(air_temp_c, na.rm = TRUE),
                   meanvib = mean(log_raw_vib, na.rm = TRUE), mivib = min(log_raw_vib, na.rm = TRUE), maxvib = max(log_raw_vib, na.rm = TRUE), 
                   meanprecip = mean(precipitation, na.rm = TRUE), minprecip = min(precipitation, na.rm = TRUE), maxprecip = max(precipitation, na.rm = TRUE))

#Summarize all environmental variables by location 
env_var_location = environmental_vibrio %>% 
  dplyr::group_by(location_id) %>% 
  dplyr::summarise(meansal = mean(salinity, na.rm = TRUE), minsal = min(salinity, na.rm = TRUE), maxsal = max(salinity, na.rm = TRUE), 
                   meantemp = mean(water_temp, na.rm = TRUE), mintemp = min(water_temp, na.rm = TRUE), maxtemp = max(water_temp, na.rm = TRUE), 
                   meanph = mean(ph, na.rm = TRUE), minph = min(ph, na.rm = TRUE), maxph = max(ph, na.rm = TRUE), 
                   meanair = mean(air_temp_c, na.rm = TRUE), minair = min(air_temp_c, na.rm = TRUE), maxair = max(air_temp_c, na.rm = TRUE),
                   meanvib = mean(log_raw_vib, na.rm = TRUE), mivib = min(log_raw_vib, na.rm = TRUE), maxvib = max(log_raw_vib, na.rm = TRUE))

#Summarize all environmental variables by lo

#IRL 

shapiro.test(irl_environmental$raw_vib) #p = 1.524e-07

irl = environmental_vibrio %>% filter(region == 1) 
shapiro.test((irl$log_raw_vib)) #p = 0.03046



#What is the average dust over this period of time? 
irl_dust%>%
  filter(AOD_1020nm > 0) %>%
  summarize(mean = mean(AOD_1020nm))


##Order IRL dust values, then determine the corresponding dust dates and values. 
ordered_irl_dust = order(irl_dust$AOD_1020nm, decreasing=T)
head(ordered_irl_dust)
irl_dust$date[17] #"2019-06-25"
irl_dust$AOD_1020nm[17] #0.27413
irl_dust$date[28] #"2019-07-14"
irl_dust$AOD_1020nm[28] #0.229634

tail(ordered_irl_dust)

irl_dust$AOD_1020nm[39] # 0.025018

irl_dust = irl_dust %>% filter(AOD_1020nm > 0)
shapiro.test(irl_dust$AOD_1020nm)
shapiro.test(log10(irl_dust$AOD_1020nm))

#AOD is not normally distributed! We should examine correlations with log transformed data.

##Salinity 

###Average salinity per site

###What is the range of salinity? 
ordered_irl_salinity = order(irl_environmental$salinity, decreasing = T)
head(ordered_irl_salinity)
irl_environmental[9,]

###What was the average salinity per site?

salinity = environmental_vibrio %>% 
  dplyr::group_by(region, location_id) %>% 
  dplyr::summarise(meanppt = mean(salinity, na.rm = TRUE))

### Is salinity normally distributed?
shapiro.test(irl_environmental$salinity) #p = 0.22, yes, normally distributed. 

### Are the sites distinctly different based on salinity? 

#Let's do an ANOVA to compare the salinity between sites

#Null hypothesis: the means of the different groups are the same
#Alternative hypothesis: At least one sample mean is not equal to the others.

irl_salinity_anova = aov(salinity ~ location_name, data = irl_environmental)
summary(irl_salinity_anova) # yes, p = 0.000112

##pH 
ordered_irl_ph = order(irl_environmental$ph, decreasing = T)
head(ordered_irl_ph)
tail(ordered_irl_ph)
irl_environmental$ph[16]
irl_environmental$ph[22]


###What was the average ph per site?

ph_mean = environmental_vibrio %>% 
  dplyr::group_by(region, location_id) %>% 
  dplyr::summarise(meanph = mean(ph, na.rm = TRUE))

mean(irl_environmental$ph)

### Is ph normally distributed?
shapiro.test(irl_environmental$ph) #p = 0.137, yes, normally distributed. 

### Are the sites distinctly different based on salinity? 

#Let's do an ANOVA to compare the salinity between sites

#Null hypothesis: the means of the different groups are the same
#Alternative hypothesis: At least one sample mean is not equal to the others.

irl_ph_anova = aov(ph ~ location_name, data = irl_environmental)
summary(irl_ph_anova) # no, not distinctly different, p = 0.446

##Water Temperature

###What was the average water temperature per site?

water_temp_mean = environmental_vibrio %>% 
  dplyr::group_by(region, location_id) %>% 
  dplyr::summarise(meantemp = mean(water_temp, na.rm = TRUE))

mean(irl_environmental$water_temp)

#What was the range of temperatures? 
ordered_irl_water_temp = order(irl_environmental$water_temp, decreasing = T)
head(ordered_irl_water_temp)
tail(ordered_irl_water_temp)
irl_environmental$water_temp[16]
irl_environmental$water_temp[1]

### Is water temp normally distributed?
shapiro.test(irl_environmental$water_temp) #p = 0.8089, yes, normally distributed. 

### Are the sites distinctly different based on water temp? 

#Let's do an ANOVA to compare the water temp between sites

#Null hypothesis: the means of the different groups are the same
#Alternative hypothesis: At least one sample mean is not equal to the others.

irl_water_temp_anova = aov(water_temp ~ location_name, data = irl_environmental)
summary(irl_water_temp_anova) # no, not distinctly different, p = 0.4

#Sampling Time
irl_environmental %>% filter(location_num == 1) %>% dplyr::summarize(mean = mean(sample_time))
ordered_irl_time= irl_environmental %>% filter(location_num == 1) 
ordered_irl_time = order(ordered_irl_time$sample_time, decreasing = T)
head(ordered_irl_time)
tail(ordered_irl_time)
irl_environmental$date[6]
irl_environmental$sample_time[1]


#SLE

##Vibrio Counts 

shapiro.test(sle_environmental$raw_vib) #p = 7.103e-07, raw vibrio is not normally distributed

sle = environmental_vibrio %>% filter(region == 2) 
shapiro.test((sle$log_raw_vib)) #p = 0.4061, vibrio is normally distributed when transformed


sle_vibrio_anova = aov(log_raw_vib ~ location_name, data = sle)
summary(sle_vibrio_anova) # no, not distinctly different, p = 0.281


#What is the average dust over this period of time? 
sle_dust%>%
  filter(AOD_1020nm > 0) %>%
  summarize(mean = mean(AOD_1020nm))


##Order SLE dust values, then determine the corresponding dust dates and values. 
ordered_sle_dust = order(sle_dust$AOD_1020nm, decreasing=T)
head(ordered_sle_dust)
sle_dust$date[16] #"2019-06-25"
sle_dust$AOD_1020nm[16] #0.29713
sle_dust$date[30] #"2019-07-13"
sle_dust$AOD_1020nm[30] #0.240665

log10(sle_dust$AOD_1020nm[46])
log10(sle_dust$AOD_1020nm[16])


tail(ordered_sle_dust)
sle_dust$AOD_1020nm[46] #0.027799

sle_dust = sle_dust %>% filter(AOD_1020nm > 0)
shapiro.test(sle_dust$AOD_1020nm)

#AOD is not normally distributed! We should examine correlations with log transformed data.

##Salinity 

###Average salinity per site

### Is salinity normally distributed?
shapiro.test(sle_environmental$salinity) #0.1191, yes

### Are the sites distinctly different based on salinity? 

#Let's do an ANOVA to compare the salinity between sites

#Null hypothesis: the means of the different groups are the same
#Alternative hypothesis: At least one sample mean is not equal to the others.

sle_salinity_anova = aov(salinity ~ location_name, data = sle_environmental)
summary(sle_salinity_anova) #p = 8.53e-07, yes distinctly different

##pH 

###What was the average ph per site?


### Is ph normally distributed?
shapiro.test(sle_environmental$ph) #0.09147, yes, normally distributed. 

### Are the sites distinctly different based on salinity? 

#Let's do an ANOVA to compare the salinity between sites

#Null hypothesis: the means of the different groups are the same
#Alternative hypothesis: At least one sample mean is not equal to the others.

sle_ph_anova = aov(ph ~ location_name, data = sle_environmental)
summary(sle_ph_anova) # no, not distinctly different, p = 0.115

##Water Temperature

###What was the average water temperature per site?

water_temp_mean = environmental_vibrio %>% 
  dplyr::group_by(region, location_id) %>% 
  dplyr::summarise(meantemp = mean(water_temp, na.rm = TRUE))

mean(irl_environmental$water_temp)

#What was the range of temperatures? 

### Is water temp normally distributed?
shapiro.test(sle_environmental$water_temp) #p =  0.3535, yes, normally distributed. 

### Are the sites distinctly different based on water temp? 

#Let's do an ANOVA to compare the water temp between sites

#Null hypothesis: the means of the different groups are the same
#Alternative hypothesis: At least one sample mean is not equal to the others.

sle_water_temp_anova = aov(water_temp ~ location_name, data = sle_environmental)
summary(sle_water_temp_anova) # no, not distinctly different, p = 0.0819 

#Sampling Time
sle_environmental %>% filter(location_num == 1) %>% dplyr::summarize(mean = mean(sample_time))
ordered_irl_time= irl_environmental %>% filter(location_num == 1) 
ordered_irl_time = order(ordered_irl_time$sample_time, decreasing = T)
head(ordered_irl_time)
tail(ordered_irl_time)
irl_environmental$date[6]
irl_environmental$sample_time[1]
