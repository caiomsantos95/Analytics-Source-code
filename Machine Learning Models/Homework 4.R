# Packages 
library(caret)
library(glmnet)l
library(leaps)
library(ggcorrplot)
library(gridExtra)
library(xgboost)
library(skimr)

# Read data
options(stringsAsFactors = FALSE)
Hitters_raw <- read.csv("Hitters.csv")

### 1a - CORRELATION MATRIX 
cor<- cor(Hitters_raw[,2:18])
ggcorrplot(cor)

#### 1c - NORMALIZING DATA AND SPLITTING TRAIN/TEST
pp <- preProcess(Hitters_raw, method=c("center", "scale"))
Hitters <- predict(pp, Hitters_raw)
# Train/test
RNGkind(sample.kind = "Rejection")
set.seed(299)
train.obs <- createDataPartition(Hitters$Salary, p = 0.70, list = FALSE)
train <- Hitters[train.obs,2:21]
test <- Hitters[-train.obs,2:21]

# Prepare the data matrices for glmnet functions
x.train=model.matrix(Salary~.,data=train)
y.train=train$Salary
x.test=model.matrix(Salary~.,data=test) 
y.test=test$Salary

mean(y.train) #0.02788154

summary(train$League)
skim(x.train)
skim(train)

### R SQUARED FUNCTION  
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

r2_osr2_glmnet <- function(tree, trainData, testData, trainY, testY, lambda) {
  # Like we did for logistic regression, we use the predict() function on the model, 
  # indicating the data we should predict on with the newdata parameter.
  PredictTrain = predict(tree, newx = trainData, s=lambda)
  PredictTest = predict(tree, newx = testData, s=lambda)
  
  # Let us compute R2 for both the training and test set
  # First we create a baseline model, which is the average of the training set reimbursements.
  ymean = mean(trainY)
  
  # Then we can compute SSE and SST - the sum of square residuals between the
  # predictions and the truth vs the baseline and the truth
  SSETrain = sum((trainY - PredictTrain)^2)
  SSTTrain = sum((trainY - ymean)^2)
  # R2 is 1 minus the ratio of these terms
  R2 = 1 - SSETrain/SSTTrain
  print(paste0("R2=",R2))
  
  # We compute the out of sample R2 similarly, using predictTest and claimsTest
  # instead of the corresponding training sets
  # Remember that we keep the baseline the same as in the training set.
  # Using the testset true values would be cheating!
  SSETest = sum((testY - PredictTest)^2)
  SSTTest = sum((testY - ymean)^2)
  OSR2 = 1 - SSETest/SSTTest
  print(paste0("OSR2=",OSR2))
}

#### 1d - Linear regression model 
lm.md <- lm(train$Salary~., data = train)
summary(lm.md)
or2_lm.md <- r2_osr2(tree=lm.md, trainData=train, testData=test, yvar="Salary")

#### 1f - RIDGE REGRESSION
all.lambdas <- c(exp(seq(15, -10, -.1)))
set.seed(300)
ridge.cv=cv.glmnet(x.train, y.train, alpha=0, lambda=all.lambdas)
plot(ridge.cv, main="Ridge regression MSE\n")
best.lambda.ridge <- ridge.cv$lambda.min 
ridge.model=glmnet(x.train,y.train,alpha=0,lambda=best.lambda.ridge)

beta.ridge <- glmnet(x = x.train, y = y.train, alpha = 0, lambda = best.lambda.ridge)$beta
sum(beta.ridge != 0)

#### 1g - LASSO REGRESSION
all.lambdas <- c(exp(seq(15, -10, -.1)))
set.seed(301)
lasso.cv=cv.glmnet(x.train, y.train, alpha=1, lambda=all.lambdas)
plot(lasso.cv, main="Ridge regression MSE\n")
best.lambda.lasso <- lasso.cv$lambda.min 
lasso.model=glmnet(x.train,y.train,alpha=1,lambda=best.lambda.lasso)

beta.lasso <- glmnet(x = x.train, y = y.train, alpha = 1, lambda = best.lambda.lasso)$beta
sum(beta.lasso != 0)

#### 1h - Coefficient plot 
coeff.matrix = as.matrix(cbind(ridge.model$beta, lasso.model$beta))
coeff.matrix
barplot(t(coeff.matrix[order(ridge.model$beta),]), 
        beside=TRUE, horiz = TRUE, las=1, cex.names=.7, 
        main = "Model coefficients")

#### 1i - R2 FOR LASSO/RIDGE
##RIDGE REGRESSION
ridge.preds_train <- predict(ridge.model, newx = x.train, s=best.lambda.ridge)
ridge.isr <- 1 - sum((ridge.preds_train - y.train)^2)/sum((mean(y.train) - y.train)^2)
ridge.preds_test <- predict(ridge.model, newx = x.test, s=best.lambda.ridge)
ridge.osr <- 1 - sum((ridge.preds_test - y.test)^2)/sum((mean(y.train) - y.test)^2)

##LASSO REGRESSION
lasso.preds_train <- predict(lasso.model, newx = x.train, s=best.lambda.lasso)
lasso.isr <- 1 - sum((lasso.preds_train - y.train)^2)/sum((mean(y.train) - y.train)^2)
lasso.preds_test <- predict(lasso.model, newx = x.test, s=best.lambda.lasso)
lasso.osr <- 1 - sum((lasso.preds_test - y.test)^2)/sum((mean(y.train) - y.test)^2)

ridge_r2 <- c(ridge.isr,ridge.osr)
lasso_r2 <- c(lasso.isr, lasso.osr)
r2_matrix <- cbind(ridge_r2, lasso_r2)
r2_matrix

#### 1j - Elastic net training
set.seed(302)
elnet.cv=train(Salary~.,train, method = "glmnet",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid=expand.grid(alpha=seq(0,1,.1), lambda=all.lambdas) )

best.elnet.params=elnet.cv$bestTune
best.elnet.params

#### 1k - Elastic net
elnet.model=glmnet(x.train,y.train,alpha=best.elnet.params$alpha,lambda=best.elnet.params$lambda)
elnet.preds <- predict(elnet.model, newx =x.train, s = best.elnet.params$lambda)
r2_osr2_glmnet(tree = elnet.model, trainData = x.train, testData = x.test, trainY = y.train, testY = y.test, lambda = best.elnet.params$lambda)

####1l - Coefficients
# Coefficient plot 2
coeff.matrix = as.matrix(cbind(lasso.model$beta, elnet.model$beta, ridge.model$beta))
sum(elnet.model$beta != 0)
barplot(t(coeff.matrix[order(ridge.model$beta),]),
        beside=TRUE, horiz = TRUE, las=1, cex.names=.7,
         main = "Model coefficients")

####1m - Best Subset
set.seed(308)
fs <- train(Salary~., train, method = "leapForward",
            trControl = trainControl(method = "cv", number = 10),
            tuneGrid = expand.grid(.nvmax=seq(1,15)))

nvars.fs = fs$bestTune$nvmax
nvars.fs
coef(fs$finalModel, nvars.fs)
summary(fs)

####1n - Best Subset model
fs.model <- lm(data = train, Salary ~. -HmRun -Runs -RBI -Years - CAtBat - CHits - CHmRun -CRuns -Assists -Errors -League -NewLeague)
summary(fs.model)
r2_osr2(fs.model, train, test, "Salary")

#### 1o - XGBOOST
set.seed(304)
xgb.cv <-  train(Salary~.,train,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=10))

best.xgb.params=xgb.cv$bestTune
best.xgb.params
xgb.model <- xgboost(data = x.train, label = y.train, 
                              nrounds = 50, max_depth = 3, eta = 0.4, gamma = 0, colsample_bytree = 0.6, min_child_weight = 1, subsample = 0.5)

#### 1p - R2
xgb.preds_train <- predict(xgb.model, newdata = x.train, n.trees=100)
xgb.isr <- 1 - sum((xgb.preds_train - y.train)^2)/sum((mean(y.train) - y.train)^2)
xgb.preds_test <- predict(xgb.model, newdata = x.test, n.trees=100)
xgb.osr <- 1 - sum((xgb.preds_test - y.test)^2)/sum((mean(y.train) - y.test)^2)

#### 1q - Variable importance
mat <- xgb.importance (model = xgb.model)
xgb.plot.importance (importance_matrix = mat) 
mat$Importance%>%sum()#This sums to 1. So the Gain column gives us the the relative importance

