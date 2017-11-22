library(Quandl)
library(quantmod)
library(tseries)
library(CausalImpact)
library(mlbench)
library(caret)
library(e1071)

Sys.setenv(TZ = "UTC")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")

MSCI.EEM <- Quandl("CHRIS/EUREX_FMEA1", type = "xts", start_date = "2013-09-09")
AUDUSD <- Quandl("CURRFX/AUDUSD", type = "xts", start_date = "2013-09-09")
USD <- Quandl("CHRIS/ICE_DX1", type = "xts", start_date = "2013-09-09")

#### CREATE DATA FOR MACHINE LEARNING PREDICTION ####
EEM.mo <- to.monthly(MSCI.EEM[,4], indeAt = 'firstof')
EEM.ROC <- ROC(EEM.mo,1)

AUDUSD.mo <- to.monthly(AUDUSD[,1], indexAt = 'firstof')
USD.mo <- to.monthly(USD[,4], indexAt = 'firstof')

State <- cbind(ifelse(EEM.ROC[,4] > 0,"Up", "Down"))
State <- lag(State, -1)

Live <- State
State <- as.data.frame(State)
Live.df <- as.data.frame(Live)
Live.df <- na.omit(Live.df)

RawData <- as.data.frame(na.omit(cbind(AUDUSD.mo[,4], USD.mo[,4])))

LDRoCt <- ifelse(nrow(RawData) > nrow(Live.df),nrow(Live.df),nrow(RawData))

LiveData <- na.omit(cbind(tail(RawData, LDRoCt), tail(Live.df, LDRoCt)))
names(LiveData) <- c("AUDUSD", "USD","State")

USRow <- ifelse(nrow(RawData) > nrow(State), nrow(State), nrow(RawData))

USDData <- na.omit(cbind(tail(RawData, USRow), tail(State, USRow)))
names(USDData) <- c("AUDUSD", "USD","State")

#### CREATED LIST of 80% OF ROWS IN ORIGINAL DATASET TO TRAIN THE MODEL ####
validation_index <- createDataPartition(USDData$State, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- USDData[-validation_index,]
# use the remaining 80% of data to training and testing the models
USDData <- USDData[validation_index,]

#### SUMMARIZE DATASET ####
# dimensions of dataset
dim(USDData)

# list types for each attribute
sapply(USDData, class)

# list the levels for the class
levels(USDData$State)

# summarize the class distribution
percentage <- prop.table(table(USDData$State)) * 100
cbind(freq=table(USDData$State), percentage=percentage)

summary(USDData)

#### VISUALIZE DATASETS ####
# split input and output
x <- USDData[,1:2]
y <- USDData[,3]

# boxplot for each attribute on one image
par(mfrow=c(1,2))
for(i in 1:2) {
  boxplot(x[,i], main=names(USDData[-3])[i])
}

# barplot for class breakdown
plot(y)

# scatterplot matrix (requires ellipse package to be available)
featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#### EVALUATE SOME ALGORITHMS ####
# Run algorithms using 10-fold cross validation
# if method = "repeadedcv", "repeats =" can be used
# repeats can be dangerous if model is trained in "deteministic manner"
control <- trainControl(method="cv", number=10)

#### CONTROL METHODS ####
#### Rolling Window aka Forward Chainging / Walk Forward Analysis best for time series analysis ####
## Resampling Methods in Caret ##
## "boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV" ##
## "none" (only fits one model to the entire training set),
## "oob" (only for random forest, bagged trees, bagged earth, bagged flexible discriminant analysis,
## or conditional tree forest models)
## timeslice, "adaptive_cv", "adaptive_boot" or "adaptive_LGOCV"

#control <- trainControl(method = "timeslice", initialWindow = 97,  fixedWindow = TRUE, horizon = 1)

####
###Metric "RMSE" & "Rsquared" for Regression; "Accuracy" & "Kappa" for Classification
metric <- "Accuracy"

#### BUILD MODELS ####
# A) LINEAR ALGORITHMS
fit.lda <- train(State~., data=USDData, method="lda", metric=metric, trControl=control)

# B) NONLINEAR ALGORITHMS
# CART
fit.cart <- train(State~., data=USDData, method="rpart", metric=metric, trControl=control)
# kNN
fit.knn <- train(State~., data=USDData, method="knn", metric=metric, trControl=control)

# C) ADVANCED ALGORITHMS
# SVM
fit.svm <- train(State~., data=USDData, method="svmRadial", metric=metric, trControl=control)
# Random Forest
fit.rf <- train(State~., data=USDData, method="rf", metric=metric, trControl=control)

#### SELECT BEST MODEL ####
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.svm)

#### MAKE PREDICTIONS USING BEST MODEL ####
# estimate skill of the Model on the validation dataset
predictions <- predict(fit.svm, validation)
confusionMatrix(predictions, validation$State)

########## FINALIZE MODEL ##########
# Save Final Model #
final_model <- svm(State~., LiveData) ###The entire Dataset
# save the model to disk
saveRDS(final_model, "./USD_model.rds")

# load the model
super_model <- readRDS("./USD_model.rds")
print(super_model)

### Predictive Model with probabilities ###
pred.model <- svm(State~., LiveData, probability = TRUE)
predict(pred.model, LiveData[,1:2], probability = TRUE)

KNN_Final_Model <- knnreg(State~., LiveData)
predict(KNN_Final_Model, LiveData[,1:2])

# make a predictions on "new data" using the final model
final_predictions <- predict(super_model, LiveData[,1:2])
confusionMatrix(final_predictions, LiveData$State)
###################### END MACHINE LEARNING PREDICTIONS #####################

##################### BEGIN CAUSATION TESTING ON RESPONSE AND PREDICTORS #######################
# #### AUD/USD AND EEM DATA WHERE AUD/USD = Response Variable (y) and EEM = Predictor (x) ####
# AUDEEM <- na.locf((cbind(AUDUSD[,1], MSCI.EEM[,4])), na.omit = FALSE, fromLast = TRUE) #na.locf fills the NA fields with prior day's value
# PairAE <- data.frame(AUDEEM) 
# names(PairAE) <- c('AUDUSD', 'EEM')
# 
# AEdata <- cbind(PairAE[,1], PairAE[,2])
# 
# #### EEM AND AUD/USD DATA WHERE EEM = Response Variable (y) and AUD/USD = Predictor (x) ####
# EEMAUD <- na.locf((cbind(MSCI.EEM[,4], AUDUSD[,1])), na.omit = FALSE, fromLast = TRUE)
# PairEA <- data.frame(EEMAUD)
# names(PairEA) <- c('EEM', 'AUDUSD')
# 
# EAdata <- cbind(PairEA[,1], PairEA[,2])
# 
# #### EEM AND USD DATA WHERE EEM = Response Variable (y) and USD = Predictor (x) ####
# EEMUSD <- na.locf((cbind(MSCI.EEM[,4], USD[,4])), na.omit = FALSE, fromLast = TRUE)
# PairEU <- data.frame(EEMUSD) 
# names(PairEU) <- c('EEM', 'USD')
# 
# EUdata <- cbind(PairEU[,1], PairEU[,2])

# #### Perform an Causal Impact on EEM Predicting a Response for AUD ####
# pre.period <- c(1,856) #70% used for the Pre-Intervention Period
# post.period <- c(857,1225) #30% used for the post-intervention Period
# impactAE <- CausalImpact(AEdata, pre.period, post.period) # y = response variable (AUDUSD), x1 = predictor (EEM)
# plot(impactAE)

# #### Perform an Causal Impact on AUD Predicting a Response for EEM ####
# pre.period <- c(1,856) #70% used for the Pre-Intervention Period
# post.period <- c(857,1225) #30% used for the post-intervention Period
# impactEA <- CausalImpact(EAdata, pre.period, post.period) # y = response variable (EEM), x1 = predictor (AUDUSD)
# plot(impactEA)
# 
# #### Perform an Causal Impact on USD Predicting a Response for EEM ####
# upre.period <- c(1,756) #70% used for the Pre-Intervention Period
# upost.period <- c(757,1081) #30% used for the post-intervention Period
# impactEU <- CausalImpact(EUdata, upre.period, upost.period) # y = response variable (EEM), x1 = predictor (AUDUSD)
# plot(impactEU)
# 
# 
# #Run Mean Reversion Test
# Model <- lm(AUDUSD ~ EEM + 0, data = PairAE)
# beta <- coef(Model)[1]
# 
# ###Compute Spread
# sprd <- PairAE$AUDUSD - beta*PairAE$EEM
# ht <- suppressWarnings(adf.test(sprd, alternative = 'stationary', k = 1))
# 
# cor(PairEA[,1], PairEA[,2])
# cor(PairEU[,1], PairEU[,2])
# 
# #summary(impactAE, "report") # Report on impact of EEM on AUD/USD
# summary(impactEA, "report") # Report on impact of AUD/USD on EEM
# summary(impactEU, "report") # Report on impact of USD on EEM



