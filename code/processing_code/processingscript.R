#Processing Script

#load libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(plyr)
library(scales)
library(wesanderson)

#Data Set

##Vibrio Data - Culturable Vibrio were enumerated from surface waters by spread plate onto TCBS.  

##Environmental Data - Water temperature and pH were determined using a YSI sonde. 
#Salinity was determined using a refractometer. Data was collected on site at the time of sampling.  

##Dust Data - Daily averages for aerosol optical depth (AOD) were obtained from [AERONET Version 3](https://aeronet.gsfc.nasa.gov/new_web/data.html). 
#The AOD values were obtained for stations at NASA Kennedy Space Center and Lake Okeechobee 
#to approximate the aerosol optical depth in the Northern Indian River Lagoon (IRL), and the St. Lucie Estuary (sle), respectively. 
#The following data are cloud cleared and quality controls have been applied but these data may not have final calibration applied. 
#For more information on this data, contact: PI Nima_Pahlevan at nima.pahlevan@nasa.gov

#Vibrio Data
irl_vibrio = read.csv("./data/raw_data/irl_raw_vibrio_counts.csv", stringsAsFactors=FALSE)
sle_vibrio = read.csv("./data/raw_data/sle_raw_vibrio_counts.csv", stringsAsFactors=FALSE)


#Environmental Data
irl_environmental = read.csv("./data/raw_data/irl_environmental_data.csv", stringsAsFactors=FALSE)
sle_environmental = read.csv("./data/raw_data/sle_environmental_data.csv", stringsAsFactors=FALSE)

#Dust Data
irl_dust = read.csv("./data/raw_data/ksc_dust.csv", stringsAsFactors=FALSE)
sle_dust = read.csv("./data/raw_data/lake_okeechobee_dust.csv", stringsAsFactors=FALSE)


#When observing the AOD Dust data, there appears to be many observations gor AOD at different wavelengths. 
#We should retain the data at a single wavelength, and save this data to the environmental variables data table.    
#It appears that the KSC and Lake Okeechobee data maintain data at different wavelengths, but do have data at 1020nm

#rename date columm in Dust Data 
names(irl_dust)[1] = "date"
names(sle_dust)[1] = "date"

#rename date column in Vibrio data
names(irl_vibrio)[2] = "date"
names(sle_vibrio)[2] = "date"


#keep dust data at 1020nm
irl_dust = irl_dust %>%
  select(date, AOD_1020nm)

sle_dust = sle_dust %>%
  select(date, AOD_1020nm)


#date for environmental variables is different than that of the other observations. We want MM/DD/YYYY. 
#Right now, the dates in the dust table, environmental table, and vibrio table are saved as characters.

class(irl_dust$date)
class(irl_environmental$date)
class(irl_vibrio$date)



#DUST DATA
##There doesn't seem to be an association between Vibrio counts and the dust data, based on this data set. 
#We examined the AOD on the day of sampling. What if we examine the AOD the day or days prior to sampling? 

#We may want to create a function that looks at the date of sampling from the vibrio data set, and then 
#stores the data from the day before sampling from the dust data set. 

#For example, we sampled on 6/17/2019. We want to take the data from the Dust Data Set from 6/16/2019. 


#We can force this by hand. We sampled the SLE on the following dates: 
#SLE: 6/5, 6/12, 6/19, 6/26, 7/3,  7/17, 7/24, 7/31
#24 hours before these dates correspond with the following indeces from dust data: 
#5, 10, 16, 22, 33, 39, 46

sle_dust$previous_24 = "NA"
sle_dust$previous_24[6] = sle_dust$AOD_1020nm[5]
sle_dust$previous_24[17] = sle_dust$AOD_1020nm[16]
sle_dust$previous_24[23] = sle_dust$AOD_1020nm[22]
sle_dust$previous_24[34] = sle_dust$AOD_1020nm[33]
sle_dust$previous_24[40] = sle_dust$AOD_1020nm[39]
sle_dust$previous_24[47] = sle_dust$AOD_1020nm[46]

sle_dust$previous_24 = as.numeric(sle_dust$previous_24)
class(sle_dust$previous_24)

sle_dust$previous_48 = "NA"
sle_dust$previous_48[17] = sle_dust$AOD_1020nm[15]
sle_dust$previous_48[23] = sle_dust$AOD_1020nm[21]
sle_dust$previous_48[34] = sle_dust$AOD_1020nm[32]
sle_dust$previous_48[40] = sle_dust$AOD_1020nm[38]
sle_dust$previous_48[47] = sle_dust$AOD_1020nm[45]

sle_dust$previous_48 = as.numeric(sle_dust$previous_48)
class(sle_dust$previous_48)

sle_dust$previous_72 = "NA"
sle_dust$previous_72[6] = sle_dust$AOD_1020nm[4]
sle_dust$previous_72[17] = sle_dust$AOD_1020nm[14]
sle_dust$previous_72[23] = sle_dust$AOD_1020nm[20]
sle_dust$previous_72[34] = sle_dust$AOD_1020nm[31]
sle_dust$previous_72[40] = sle_dust$AOD_1020nm[37]
sle_dust$previous_72[47] = sle_dust$AOD_1020nm[44]

sle_dust$previous_72 = as.numeric(sle_dust$previous_72)
class(sle_dust$previous_72)

#IRL: 6/10, 6/17, 6/24, 7/1, 7/8, 7/15, 7/22, 7/29
#24 hours before these dates correspond with the following indeces from dust data:

irl_dust$previous_24 = "NA"
irl_dust$previous_24[16] = irl_dust$AOD_1020nm[15]
irl_dust$previous_24[21] = irl_dust$AOD_1020nm[20]
irl_dust$previous_24[29] = irl_dust$AOD_1020nm[28]
irl_dust$previous_24[36] = irl_dust$AOD_1020nm[35]
irl_dust$previous_24[43] = irl_dust$AOD_1020nm[42]

irl_dust$previous_24 = as.numeric(irl_dust$previous_24)
class(irl_dust$previous_24)

irl_dust$previous_48 = "NA"
irl_dust$previous_48[4] = irl_dust$AOD_1020nm[3]
irl_dust$previous_48[16] = irl_dust$AOD_1020nm[14]
irl_dust$previous_48[36] = irl_dust$AOD_1020nm[34]
irl_dust$previous_48[43] = irl_dust$AOD_1020nm[41]

irl_dust$previous_48 = as.numeric(irl_dust$previous_48)
class(irl_dust$previous_48)

irl_dust$previous_72 = "NA"
irl_dust$previous_72[16] = irl_dust$AOD_1020nm[13]
irl_dust$previous_72[36] = irl_dust$AOD_1020nm[33]
irl_dust$previous_72[43] = irl_dust$AOD_1020nm[40]
irl_dust$previous_72 = as.numeric(irl_dust$previous_72)
class(irl_dust$previous_72)

#add dust data into enviornmental varibales
irl_environmental = left_join(irl_environmental, irl_dust, by = "date")

#there is dust data missing from the irl data set for 6/17 and 7/8

sle_environmental = left_join(sle_environmental, sle_dust, by = "date")

#there is dust data missing from the sle data set for 6/19


#Let's bind all of the environmental data together 

environmental = bind_rows(irl_environmental, sle_environmental, .id = "region")


#EXPLORATORY DATA ANALYSIS 

#Can we visualize the Vibrio data by week? 
#Ideally, we would want to include SD for each observation, based on the variation between the data sets. 
#We may need to add (mutate) a new column for SD. 

#We need to learn how to average the replicates. 
#First, let's remove the last letter from the Sample ID, since this identifies the replicate. 

irl_vibrio2 = irl_vibrio %>% 
  separate(col = sample_id, into = c("sample_id","rep_id"), sep = c(8))

sle_vibrio2 = sle_vibrio %>% 
  separate(col = sample_id, into = c("sample_id","rep_id"), sep = c(8))


irl_vibrio2 = ddply(irl_vibrio2,.(sample_id),summarize, raw_vib = mean(raw_total), rv_se = 2*sqrt(raw_vib)) 

sle_vibrio2 = ddply(sle_vibrio2,.(sample_id),summarize, raw_vib = mean(raw_total), rv_se = 2*sqrt(raw_vib)) 


irl_environmental = left_join(irl_environmental, irl_vibrio2, by = "sample_id")

sle_environmental = left_join(sle_environmental, sle_vibrio2, by = "sample_id")



vibrio = bind_rows(irl_vibrio2, sle_vibrio2)
environmental_vibrio = left_join(environmental, vibrio, by = "sample_id")
environmental_vibrio = mutate(environmental_vibrio, log_raw_vib = log10(raw_vib))
environmental_vibrio = mutate(environmental_vibrio, log_rv_se = log10(rv_se))

  
id_df <- data.frame(location_name=c("Scottsmoore Landing", "Titusville Pier", "Beacon 42 Boat Ramp", "Snug Harbor", "Stuart Boardwalk", "Leighton Park"), location_id=c("IRL 1", "IRL 2", "IRL 3", "SLE 1", "SLE 2", "SLE 3"))
environmental_vibrio = left_join(environmental_vibrio, id_df, by = "location_name")



saveRDS(environmental_vibrio, file = "./data/processed_data/environmental_vibrio.rds")
saveRDS(sle_environmental, file = "./data/processed_data/sle_environmental.rds")
saveRDS(irl_environmental, file = "./data/processed_data/irl_environmental.rds")


