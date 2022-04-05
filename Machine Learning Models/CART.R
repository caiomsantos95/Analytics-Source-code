library(tidyverse)
library(randomForest)
library(rpart.plot)
library(caTools)
library(dplyr)
library(randomForest)
library(caret)
library(rpart)
library(rpart.plot) 

### TRAIN THE MODEL USING A TRAIN FUNCTION 
cv.trees <- train(y = train$SalePrice,
                 x = subset(train, select=-c(SalePrice)),
                 method = "rpart", 
                 trControl = trainControl(method = "cv", number = 10), 
                 tuneGrid = data.frame(.cp = seq(.00001,.0003,.000001)))

### EXTRACT RESULTS FROM THE MODEL
plot(cv.trees$results$Rsquared, cv.trees$results$cp)
cv.trees$results$cp[cv.trees$results$Rsquared >= max(cv.trees$results$Rsquared)]
cv.trees$bestTune
cv.results = cv.trees$results
cv.results

### COMMAND TO BUILD THE MODEL
### IMPORTANT PARAMETERS:
#### MINBUCKET - MINIMAL NUMBER OF OBSERVATIONS IN EACH BRANCH
#### CP - "complexity parameter" - NEW SPLIT MUST IMPROVE THE R2 OF THE MODEL IN THE MINIMUM INDICATED BY CP

cart.model <- rpart(train$SalePrice ~., data = train, control = rpart.control(cp = 0.000296)) ### THIS CP NUMBER COMES FROM cv.results above after cross validation
prp(cart.model, digits = 2, type = 2)

### CART VARIABLE IMPORTANCE - RETURNS VARIABLES THAT WERE USED THE MOST TO CREATE THE BRANCHES 
CART_importance_scores = cart.model$variable.importance
n_variables = 20 # how many variables to display?
barplot( tail( sort(CART_importance_scores), n_variables ),
         beside = TRUE,
         horiz = TRUE,
         las=1,
         main = paste("CART - top", n_variables, "importance scores"),
         cex.names =.7)
