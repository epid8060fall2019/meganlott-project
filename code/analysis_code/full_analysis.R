
#load needed packages. make sure they are installed.
library('tidyr')
library('dplyr')
library('forcats')
library('ggplot2')
library('knitr')
library('caret')
library('doParallel')
library('rpart')
library('rpart.plot')
library('mda')
library('ranger')
library('e1071')
library('visdat')
library('gridExtra')
library('mlr')
library('gbm')
library('mda')
library('Metrics')

#load data. path is relative to project directory.
environmental_vibrio = readRDS("./data/processed_data/environmental_vibrio.rds")
irl_environmental = readRDS("./data/processed_data/irl_environmental.rds")
sle_environmental = readRDS("./data/processed_data/sle_environmental.rds")
irl_dust = readRDS("./data/processed_data/irl_dust.rds")
sle_dust = readRDS("./data/processed_data/sle_dust.rds")


###MODELING###

#Leading question: Can the variation in Vibrio counts be explained by any of the environemntal variables?


##Simple Linear Model##

# fit linear model
lm_aod = lm(log_raw_vib ~ log(AOD_1020nm), environmental_vibrio)
lm_prev24 = lm(log_raw_vib ~ log(previous_24), environmental_vibrio) 
lm_ph = lm(log_raw_vib ~ ph, environmental_vibrio)  
lm_salinity = lm(log_raw_vib ~ salinity, environmental_vibrio)  
lm_water_temp = lm(log_raw_vib ~ water_temp, environmental_vibrio)  
lm_precip = lm(log_raw_vib ~ precipitation, environmental_vibrio) 

# place results from fit into a data frame with the tidy function
lm_aod = broom::tidy(lm_aod)
lm_prev24 = broom::tidy(lm_prev24)
lm_ph = broom::tidy(lm_ph)
lm_salinity = broom::tidy(lm_salinity)
lm_water_temp = broom::tidy(lm_water_temp)
lm_precip = broom::tidy(lm_precip) 

lm_variables = bind_rows(lm_aod, lm_prev24, lm_ph, lm_salinity, lm_water_temp, lm_precip)

# save table  
saveRDS(lm_variables, file = "./results/all/resulttable.rds")


##Continuous Outcome Analysis##

#log-transform the AOD
environmental_vibrio$previous_24 = log(environmental_vibrio$previous_24)
environmental_vibrio$AOD_1020nm = log(environmental_vibrio$AOD_1020nm)

#Let's only look at the varaibles (predictors) that have as much data as possible.Need to drop observations with "NA"
d = environmental_vibrio %>% select(log_raw_vib, AOD_1020nm, previous_24, salinity, ph, water_temp, sample_time, precipitation) %>% filter(log_raw_vib != "NA") %>% filter(previous_24 != "NA") %>% filter(AOD_1020nm != "NA")

#Now, there's 33 entries

#this code does the data splitting.
set.seed(123)
trainset = caret::createDataPartition(y = d$log_raw_vib, p = 0.7, list = FALSE)
data_train = d[trainset,] #25 observations, 9 varaibles
data_test = d[-trainset,] #8 observations, 9 variables

#Null Model
#The mean of the outcome data
nullModel(y = data_train$log_raw_vib)

data_train %>%
  summarize(mean = mean(log_raw_vib))

##Single Predictor Model##

set.seed(1111) #makes each code block reproducible
fitControl <- caret::trainControl(method="repeatedcv",number=5,repeats=5) #setting CV method for caret
Npred <- ncol(data_train)-1 # number of predictors
singlepredictor <- data.frame(Variable = names(data_train)[-1], RMSE = rep(0,Npred)) 
#store values for RMSE for each variable

for (n in 2:ncol(data_train)) #loop over each predictor. For this to work, outcome must be in 1st column
{
  fit1 <- caret::train(as.formula(paste("log_raw_vib ~",names(data_train)[n])), 
                       data = data_train, method = "lm", trControl = fitControl) 
  singlepredictor[n-1,2]= fit1$results$RMSE  
}
print(singlepredictor)
saveRDS(singlepredictor, file = "./results/all/singlepredictor.rds")



##Multi-Predictor Models##
set.seed(1111) #makes each code block reproducible
#write code that uses the train function in caret to fit the outcome to all predictors using the 3 methods specified.

fitControl <- caret::trainControl(method="repeatedcv",number=5,repeats=5) #setting CV method for caret

linear_model = caret::train(log_raw_vib ~ ., data = data_train, method = "lm", trControl = fitControl)
print(linear_model)
saveRDS(linear_model, file = "./results/all/fullpredictors.rds")

test_predictions = predict(linear_model, data_test)
rmse(data_test$log_raw_vib, test_predictions)

test_prediction_plot = ggplot() + geom_point(aes(x = data_test$log_raw_vib, y = test_predictions)) + 
  ylab("Test Prediction Log(CFU/mL)") + 
  xlab("Test Data Log(CFU/mL)")
ggsave(test_prediction_plot, file = "./results/all/test_prediction_plot.png" )

##Tree Model 

#Let's only look at the varaibles (predictors) that have as much data as possible.Need to drop observations with "NA"
d = environmental_vibrio %>% select(log_raw_vib, sample_time, salinity, ph, water_temp, previous_24, precipitation) %>% filter(previous_24 != "NA") 

##Data Splitting 
set.seed(123)
trainset = caret::createDataPartition(y = d$log_raw_vib, p = 0.7, list = FALSE)
data_train = d[trainset,] #extract observations/rows for training, assign to new variable
data_test = d[-trainset,] #do the same for the test set


##Parallelization
n_cores <- 4 #number of cores to use
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

#Set Seed
set.seed(1111) #makes each code block reproducible
outcomename = "TotalPath"
Npred <- ncol(d-1) # number of predictors
resultmat <- data.frame(Variable = names(d)[-1], Accuracy = rep(0,Npred)) #store performance for each variable

#Tree
fitControl = trainControl(method="repeatedcv",number=5,repeats=5) 
fit1 = caret::train(log_raw_vib ~ ., data=d, method="rpart",  trControl = fitControl, na.action = na.pass, tuneLength = 10) 
print(fit1$results)

prp(fit1$finalModel, extra = 1, type = 1)

ww=17.8/2.54; wh=ww; #for saving plot
dev.print(device=png,width=ww,height=wh,units="in",res=600,file=("./results/all/rparttree.png")) #save tree to file


##Subset Selection 

#In this analysis, we have a continuous outcome and seven continuous predictors. We will want to use filter methods for feature selection. 
#Features will be hand-selected based on Pearson's Correlation 
#Unfortunately, none of our variables are "strongly" correlated with the outcome (R > 0.7 or R < -0.7)

#Let's only look at the varaibles (predictors) that have as much data as possible.Need to drop observations with "NA"
d = environmental_vibrio %>% select(log_raw_vib, AOD_1020nm, previous_24, salinity, ph, water_temp, sample_time, precipitation) %>% filter(log_raw_vib != "NA") %>% filter(previous_24 != "NA") %>% filter(AOD_1020nm != "NA")

boruta_output = Boruta(log_raw_vib ~ ., data=na.omit(d), doTrace=0)  
boruta_signif = getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
png(width=400,height=400, file="./results/all/variable_importance.png")
variable_importance = plot(boruta_output, las= 2, xlab="", main="Variable Importance")
dev.off()

#We see that AOD on day before and day of sampling are important features. Does the use of these combined improve the lm? 

fit2 = lm(d$log_raw_vib ~ d$AOD_1020nm + d$previous_24)
fit3 = lm(d$log_raw_vib ~ d$AOD_1020nm + d$previous_24 + d$sample_time)
fit4 = lm(d$log_raw_vib ~ d$AOD_1020nm + d$sample_time)


test_predictions2 = predict(fit2, d)
test_predictions3 = predict(fit3, d)
test_predictions4 = predict(fit4, d)


rmse(d$log_raw_vib, test_predictions2)
rmse(d$log_raw_vib, test_predictions3)
rmse(d$log_raw_vib, test_predictions4)