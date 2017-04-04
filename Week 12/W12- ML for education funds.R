
#*****************************************************
#     Week 12: Predict Federal Education Funds Allocation
#     by: Richard S.
#     Date: March 30, 2017
#******************************************************

# **************** Install required R packages ********
install.packages("rpart")
install.packages("randomForest")
install.packages("caret")
install.packages("nnet")
install.packages("corrplot")
install.packages("kernlab")
install.packages("Metrics")
install.packages("forecast")
install.packages("doMC")
install.packages("readr")

# **************** Load R packages ************
library(rpart)
library(randomForest)
library(nnet)
library(corrplot)
library(caret)
library(kernlab)
library(Metrics)
library(forecast)
library(doMC)
library(readr)

#**************** Assign clusters (Check number of cores of your computer**********
n=detectCores(all.tests = FALSE, logical = TRUE)
registerDoMC(cores = n-1)

#************** Start data processing *************
# import and transform data
# federalfunds <- read_csv("federalfunds.txt")
# nyData <- subset(federalfunds, STATE == 'NY')
# remove NA in the data set
# nyData = nyData[complete.cases(nyData),]
# ny0809Data = subset(nyData, AUDITYEAR == 2008 | AUDITYEAR == 2009)
# write.csv(ny0809Data, "ny0809Data.csv")

ny0809Data <- read_csv("ny0809Data.csv")
ny0809Data = data.frame(ny0809Data)
#****************** Create tranning and testing data set *************
set.seed(1234)
trainIndices = createDataPartition(ny0809Data$AMOUNT)
trainIndices = unlist(trainIndices)
data_train = ny0809Data[trainIndices,] 
data_test = ny0809Data[-trainIndices,]

# **************** Random Forest **********************
# Adjust the model "rfmodel" to improve accuracy
rfmodel = AMOUNT ~ AUDITYEAR + DIRECT + MAJORPROGRAM
rffit <- foreach(ntree = rep(500, 2), .combine = combine, .multicombine=TRUE, .packages = "randomForest") %dopar% randomForest(rfmodel, data=data_train,ntree=ntree,importance=TRUE)

preds_rf = predict(rffit, type="response",data_test)
sqrt(mean((preds_rf-data_test$AMOUNT)^2))
plot(data_test$AMOUNT, preds_rf)

# **************** SVM *********************************
cv_opts = trainControl(method="cv", number=10)
svmmodel = AMOUNT ~ AUDITYEAR + DIRECT + MAJORPROGRAM
svmfit = train(svmmodel, data=data_train, method="svmLinear",preProcess='range',trControl=cv_opts, tuneLength=5)

preds_svm = predict(svmfit, data_test)
sqrt(mean((preds_svm-data_test$AMOUNT)^2))
plot(data_test$AMOUNT, preds_svm)


##################################################