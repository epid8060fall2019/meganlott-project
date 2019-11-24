#Processing Script

#load libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(plyr)
library(scales)

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
irl_dust = irl_dust %>% select(date, AOD_1020nm)
sle_dust = sle_dust %>% select(date, AOD_1020nm) 


#date for environmental variables is different than that of the other observations. We want MM/DD/YYYY. 
#Right now, the dates in the dust table, environmental table, and vibrio table are saved as characters.

class(irl_dust$date)
class(irl_environmental$date)
class(irl_vibrio$date)

irl_dust$date <- as.Date(irl_dust$date, format = "%m/%d/2019")
irl_environmental$date <- as.Date(irl_environmental$date, format = "%m/%d/2019")
irl_vibrio$date <- as.Date(irl_vibrio$date, format = "%m/%d/2019")

class(irl_dust$date)
class(irl_environmental$date)
class(irl_vibrio$date)

sle_dust$date <- as.Date(sle_dust$date, format = "%m/%d/2019")
sle_environmental$date <- as.Date(sle_environmental$date, format = "%m/%d/2019")
sle_vibrio$date <- as.Date(sle_vibrio$date, format = "%m/%d/2019")


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


sle_dust$previous_48 = "NA"
sle_dust$previous_48[17] = sle_dust$AOD_1020nm[15]
sle_dust$previous_48[23] = sle_dust$AOD_1020nm[21]
sle_dust$previous_48[34] = sle_dust$AOD_1020nm[32]
sle_dust$previous_48[40] = sle_dust$AOD_1020nm[38]
sle_dust$previous_48[47] = sle_dust$AOD_1020nm[45]


sle_dust$previous_72 = "NA"
sle_dust$previous_72[6] = sle_dust$AOD_1020nm[4]
sle_dust$previous_72[17] = sle_dust$AOD_1020nm[14]
sle_dust$previous_72[23] = sle_dust$AOD_1020nm[20]
sle_dust$previous_72[34] = sle_dust$AOD_1020nm[31]
sle_dust$previous_72[40] = sle_dust$AOD_1020nm[37]
sle_dust$previous_72[47] = sle_dust$AOD_1020nm[44]


#IRL: 6/10, 6/17, 6/24, 7/1, 7/8, 7/15, 7/22, 7/29
#24 hours before these dates correspond with the following indeces from dust data:

irl_dust$previous_24 = "NA"
irl_dust$previous_24[16] = irl_dust$AOD_1020nm[15]
irl_dust$previous_24[21] = irl_dust$AOD_1020nm[20]
irl_dust$previous_24[29] = irl_dust$AOD_1020nm[28]
irl_dust$previous_24[36] = irl_dust$AOD_1020nm[35]
irl_dust$previous_24[43] = irl_dust$AOD_1020nm[42]

irl_dust$previous_48 = "NA"
irl_dust$previous_48[4] = irl_dust$AOD_1020nm[3]
irl_dust$previous_48[16] = irl_dust$AOD_1020nm[14]
irl_dust$previous_48[36] = irl_dust$AOD_1020nm[34]
irl_dust$previous_48[43] = irl_dust$AOD_1020nm[41]

irl_dust$previous_72 = "NA"
irl_dust$previous_72[16] = irl_dust$AOD_1020nm[13]
irl_dust$previous_72[36] = irl_dust$AOD_1020nm[33]
irl_dust$previous_72[43] = irl_dust$AOD_1020nm[40]


#add dust data into enviornmental varibales
irl_environmental = left_join(irl_environmental, irl_dust, by = "date")
sle_environmental = left_join(sle_environmental, sle_dust, by = "date")


###MISSING DUST DATA###

#there is dust data missing from the irl data set for 6/17 and 7/8
irl_environmental$previous_24[4:6] = irl_dust$AOD_1020nm[9] #Sample Date 6/17, Dust Date 6/16
irl_environmental$previous_48[4:6] = irl_dust$AOD_1020nm[8] #Sample Date 6/17, Dust Date 6/15
irl_environmental$previous_72[13:15] = irl_dust$AOD_1020nm[25] #Sample Date 7/8, Dust Date 7/5

#there is dust data missing from the sle data set for 6/19

sle_environmental$previous_24[7:9] = sle_dust$AOD_1020nm[10] #Sample Date 6/19, Dust Date 6/18

#Ensure that dust data is numeric
sle_environmental$previous_24 = as.numeric(sle_environmental$previous_24)
sle_environmental$previous_48 = as.numeric(sle_environmental$previous_48)
sle_environmental$previous_72 = as.numeric(sle_environmental$previous_72)

irl_environmental$previous_24 = as.numeric(irl_environmental$previous_24)
irl_environmental$previous_48 = as.numeric(irl_environmental$previous_48)
irl_environmental$previous_72 = as.numeric(irl_environmental$previous_72)



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

sle_vibrio2 = sle_vibrio %>% filter(raw_total != "NA") %>%
  separate(col = sample_id, into = c("sample_id","rep_id"), sep = c(8))

#Take the average of the replicates and the standard error of the counts.
irl_vibrio2 = ddply(irl_vibrio2,.(sample_id),summarize, raw_vib = mean(raw_total), rv_se = 2*sqrt(raw_vib)) 
sle_vibrio2 = ddply(sle_vibrio2,.(sample_id),summarize, raw_vib = mean(raw_total), rv_se = 2*sqrt(raw_vib)) 

#Now, bind the enviornmental and vibrio data together.
irl_environmental = left_join(irl_environmental, irl_vibrio2, by = "sample_id")
sle_environmental = left_join(sle_environmental, sle_vibrio2, by = "sample_id")

#Bind all of the Vibrio data into one df.
vibrio = bind_rows(irl_vibrio2, sle_vibrio2)

#combine all data into a final data frame.
environmental_vibrio = left_join(environmental, vibrio, by = "sample_id")

#Take the log of the mean Vibrio counts and Standard Error.
environmental_vibrio = mutate(environmental_vibrio, log_raw_vib = log10(raw_vib))
environmental_vibrio = mutate(environmental_vibrio, log_rv_se = log10(rv_se))

#calculate the holding time for samples 
environmental_vibrio = mutate(environmental_vibrio, holding_time = (filter_time - sample_time)/100)
environmental_vibrio$holding_time =  round(environmental_vibrio$holding_time, digits = 0)

#calculate the median air temp in degrees C 
environmental_vibrio = mutate(environmental_vibrio, air_temp_c = (((air_high + air_low)/2)-32)*5/9)

#Create a dust_vibrio data frame
irl_dust_vibrio = full_join(irl_dust, irl_environmental, by = "date")
irl_dust_vibrio = irl_dust_vibrio %>% select(date, AOD_1020nm.x, raw_vib, rv_se, location_name)
irl_dust_vibrio = mutate(irl_dust_vibrio, log_raw_vib = log10(raw_vib))
irl_dust_vibrio = mutate(irl_dust_vibrio, log_rv_se = log10(rv_se))

sle_dust_vibrio = full_join(sle_dust, sle_environmental, by = "date")
sle_dust_vibrio = sle_dust_vibrio %>% select(date, AOD_1020nm.x, raw_vib, rv_se, location_name)
sle_dust_vibrio = mutate(sle_dust_vibrio, log_raw_vib = log10(raw_vib))
sle_dust_vibrio = mutate(sle_dust_vibrio, log_rv_se = log10(rv_se))


#Add IDs for each location: IRL 1:3, SLE 1:3
id_df <- data.frame(location_name=c("Scottsmoore Landing", "Titusville Pier", "Beacon 42 Boat Ramp", "Snug Harbor", "Stuart Boardwalk", "Leighton Park"), location_id=c("IRL 1", "IRL 2", "IRL 3", "SLE 1", "SLE 2", "SLE 3"))
environmental_vibrio = left_join(environmental_vibrio, id_df, by = "location_name")

#Dates are saved as factors. Reorder dates so that they appear in order in the figures.
#SLE: 6/5, 6/12, 6/19, 6/26, 7/3,  7/17, 7/24, 7/31
#IRL: 6/10, 6/17, 6/24, 7/1, 7/8, 7/15, 7/22, 7/29

#environmental_vibrio$date = fct_recode(environmental_vibrio$date, "6/5" = "6/5/2019", "6/10" = "6/10/2019", 
                                      #"6/12"= "6/12/2019", "6/17" = "6/17/2019", "6/19" =  "6/19/2019", 
                                      #"6/24" = "6/24/2019", "6/26" = "6/26/2019", "7/1" = "7/1/2019", 
                                      #"7/3" = "7/3/2019", "7/8" = "7/8/2019", "7/15" = "7/15/2019", 
                                      #"7/17" = "7/17/2019", "7/22" = "7/22/2019", "7/24" = "7/24/2019", 
                                      #"7/29" = "7/29/2019", "7/31" = "7/31/2019" )
                                                                         
#environmental_vibrio$date = factor(environmental_vibrio$date, levels = c("6/5", "6/10", "6/12", "6/17" , "6/19", 
                                                                         #"6/24" , "6/26", "7/1", 
                                                                         #"7/3" , "7/8", "7/15", 
                                                                         #"7/17", "7/22", "7/24", 
                                                                         #"7/29", "7/31"))
#Scottsmoor Landing is spelled incorrectly.
environmental_vibrio$location_name = fct_recode(environmental_vibrio$location_name, "Scottsmoor Landing" = "Scottsmoore Landing")

saveRDS(environmental_vibrio, file = "./data/processed_data/environmental_vibrio.rds")
saveRDS(sle_environmental, file = "./data/processed_data/sle_environmental.rds")
saveRDS(irl_environmental, file = "./data/processed_data/irl_environmental.rds")
saveRDS(irl_dust, file = "./data/processed_data/irl_dust.rds")
saveRDS(sle_dust, file = "./data/processed_data/sle_dust.rds")
saveRDS(irl_dust_vibrio, file = "./data/processed_data/irl_dust_vibrio.rds")
saveRDS(sle_dust_vibrio, file = "./data/processed_data/sle_dust_vibrio.rds")
