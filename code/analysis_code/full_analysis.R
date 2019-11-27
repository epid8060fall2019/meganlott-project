#load needed packages. make sure they are installed.
library('tidyr')
library('dplyr')
library('forcats')
library('ggplot2')
library('knitr')
library('mlr') #for model fitting.
library('parallelMap') #for using multiple processors when running models through mlr
library('visdat')
library('gridExtra')
library('pROC')

#load data. path is relative to project directory.
environmental_vibrio = readRDS("./data/processed_data/environmental_vibrio.rds")
irl_environmental = readRDS("./data/processed_data/irl_environmental.rds")
sle_environmental = readRDS("./data/processed_data/sle_environmental.rds")
irl_dust = readRDS("./data/processed_data/irl_dust.rds")
sle_dust = readRDS("./data/processed_data/sle_dust.rds")



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
#The mean of the outcome data
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

linear_model = caret::train(log_raw_vib ~ ., data = data_train, method = "lm", trControl = fitControl)
print(linear_model)

regression_splines = caret::train(log_raw_vib ~ ., data = data_train, method = "earth", trControl = fitControl)
print(regression_splines)

knn_model = caret::train(log_raw_vib ~ ., data = data_train, method = "knn", trControl = fitControl)
print(knn_model)



##Tree Model 

#Let's only look at the varaibles (predictors) that have as much data as possible.Need to drop observations with "NA"
d = environmental_vibrio %>% select(log_raw_vib, region, location_id, previous_24, salinity, ph, water_temp, sample_time, precipitation, precipitation_prev24) %>% 
  filter(log_raw_vib != "NA") %>% filter(previous_24 != "NA")


#Data Splitting 
set.seed(123)
trainset = caret::createDataPartition(y = d$log_raw_vib, p = 0.7, list = FALSE)
data_train = d[trainset,] #extract observations/rows for training, assign to new variable
data_test = d[-trainset,] #do the same for the test set


###Parallelization
ncpu=4;
parallelStartSocket(ncpu, show.info=FALSE)


###Set Up
outcome = d$log_raw_vib 
outcomename = "Log(CFU/1mL)"
predictors = d[,-1]
npred=ncol(predictors)
#set sampling method for performance evaluation
#here, we use 5-fold cross-validation, 5-times repeated
sampling_choice = makeResampleDesc("RepCV", reps = 5, folds = 5)



#copy and paste the code from above that fits the single tree. set tuneLength to 20. 
set.seed(1111) #makes each code block reproducible
fitControl <- trainControl(method="repeatedcv",number=5,repeats=5) 
fit1 = caret::train(log_raw_vib~., data=data_train, method="rpart",  trControl = fitControl, na.action = na.pass, tuneLength = 20) 
print(fit1$results)

ww=17.8/2.54; wh=ww; #for saving plot
dev.print(device=png,width=ww,height=wh,units="in",res=600,file="rparttree.png") #save tree to file


#subset/variable selection with e.g. cross-validation is a good idea (or alternatively regularization) 
#should allow you to find a model that might perform better than the single-predictor ones without overfitting that seems to go on with your full model.

#bootstrap aggregating 