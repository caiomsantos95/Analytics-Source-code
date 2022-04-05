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
