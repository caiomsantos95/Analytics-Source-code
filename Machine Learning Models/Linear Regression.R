# Homework 1
#packages
install.packages("tidyverse")
install.packages("skimr")
install.packages("caret")
library(tidyverse)
library(skimr)

## Question 1
ames <- read.csv ("AmesSales.csv")
ames$SalePrice <- as.numeric(ames$SalePrice)

### 1a
summary(ames$SalePrice)
hist(ames$SalePrice)

### 1b
library(Rcpp)
library(ggplot2)
library(lattice)
library(caret)
RNGkind(sample.kind = "Rounding")
set.seed(219)
idx <- createDataPartition(ames$SalePrice, p = 0.60 , list = FALSE)
view(idx)
train <- ames[idx,]
test <- ames[-idx,]
####test of data split
mean(train$SalePrice) ##178625.1

#### linear regression model
train_cor <- subset(train, select = -c(BldgType))
cor(train_cor)
#### removing collinearities - LivArea collinear with TotalRooms
train1 <- subset(train, select = -c(TotalRooms))
#### building the model 
md1 <- lm(data = train1, SalePrice ~.)
summary(md1)
md2 <- lm(data = train1,  SalePrice ~. -HalfBath)
summary(md2)
md3 <- lm(data = train1,  SalePrice ~. -HalfBath -YearSold)
summary(md3)

### linear regression with all the variables
model <- lm(data = train, SalePrice ~.)
summary(model)

### 1c - investigating the model
par(mfrow =c(2 ,2)) # create two rows and columns in the plot window
plot(model)
par(mfrow =c(1 ,1)) # reset the plot window to default settings

#### investigating the outliers 
# Outliers
outliers <- c("1451", "2114", "2115")
print(train [outliers ,])
# Data summary
summary (train)

#### Section below is interesting for analysis in the real world since it considers the impact of outliers in the mode. This is very common
### 1d - removing outliers
train2 <- ames[setdiff(idx,outliers),]
model1 <- lm(data = train2, SalePrice ~.)
summary(model1) ### use this one for comparison - with collinear variables still included 
model2 <- lm(data = train2, SalePrice ~. -TotalRooms)
summary(model2)
model3 <- lm(data = train2, SalePrice ~. -TotalRooms - FullBath)
summary(model3)
model4 <- lm(data = train2, SalePrice ~. -TotalRooms - FullBath -YearSold)
summary(model4)

### 1g - model requested by the prompt
modelg <- lm(data = train2, SalePrice ~ BldgType + YearBuilt + Fireplaces + GarageArea + PoolArea + LivArea)
summary(modelg)

##### OUT OF SAMPLE R SQUARED
### MODEL G - Using the model to make predictions with the Out-of-Sample Dataset 
pred = predict(modelg, newdata = test)
### Calculating the Sum of Square Residuals
resid = test$SalePrice - pred
SSR = sum((resid)^2)
### Calculating the baseline model
baseline = mean(test$SalePrice)
### Calculating the total of sum squares
SST = sum((test$SalePrice - baseline)^2)
### Determing the out-of-sample R-squared
OSR2 = 1 - SSR / SST
OSR2

### MODEL 1 - Using the model to make predictions with the Out-of-Sample Dataset 
pred_1 = predict(model1, newdata = test)
### Calculating the Sum of Square Residuals
resid_1 = test$SalePrice - pred_1
SSR_1 = sum((resid_1)^2)
### Calculating the baseline model
baseline = mean(test$SalePrice)
### Calculating the total of sum squares
SST = sum((test$SalePrice - baseline)^2)
### Determing the out-of-sample R-squared
OSR2_1 = 1 - SSR_1 / SST
OSR2_1

#### Question 2
library(rpart.plot)
library(rpart)

### 2a
cart <- rpart(data = train2, SalePrice ~.)
prp(cart)
summary(cart)

### The function below is very useful for being replicated 
### 2b - R squared of the model
r2_osr2 <- function(tree, trainData, testData, yvar) {
  # Like we did for logistic regression, we use the predict() function on the model, 
  # indicating the data we should predict on with the newdata parameter.
  PredictTrain = predict(tree, newdata = trainData)
  PredictTest = predict(tree, newdata = testData)
  
  # Let us compute R2 for both the training and test set
  # First we create a baseline model, which is the average of the training set reimbursements.
  ymean = mean(trainData[,yvar])
  
  # Then we can compute SSE and SST - the sum of square residuals between the
  # predictions and the truth vs the baseline and the truth
  SSETrain = sum((trainData[,yvar] - PredictTrain)^2)
  SSTTrain = sum((trainData[,yvar] - ymean)^2)
  # R2 is 1 minus the ratio of these terms
  R2 = 1 - SSETrain/SSTTrain
  print(paste0("R2=",R2))
  
  # We compute the out of sample R2 similarly, using predictTest and claimsTest
  # instead of the corresponding training sets
  # Remember that we keep the baseline *the same* as in the training set.
  # Using the testset true values would be cheating!
  SSETest = sum((testData[,yvar] - PredictTest)^2)
  SSTTest = sum((testData[,yvar] - ymean)^2)
  OSR2 = 1 - SSETest/SSTTest
  print(paste0("OSR2=",OSR2))
}
# Now we can run this function for our tree by calling r2_osr2(...)
r2_osr2(tree=cart, trainData=train2, testData=test, yvar="SalePrice")
# The R2 and OSR2 are quite similar.

### 2c - new tree
tree.model2 = rpart(SalePrice ~., data = train2, control = rpart.control(cp =0.0001))
prp(tree.model2)
barplot(tree.model2$variable.importance, cex.names = .7)

### 2d - Out of sample of new models
cart1 <- rpart(data = train2, SalePrice ~., control = rpart.control(cp =0.01))
oos_r2_cart1 <- r2_osr2(tree=cart1, trainData=train2, testData=test, yvar="SalePrice")
oos_r2_tree.model2 <- r2_osr2(tree=tree.model2, trainData=train2, testData=test, yvar="SalePrice")


