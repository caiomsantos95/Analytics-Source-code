### RANDOM FOREST #####
### how to build a model with randomForest code
rf.md1 <- randomForest(train$SalePrice~., data=train, nodesize=25, ntree=500, mtry = 20)
rf.md1

# RF OOB
set.seed(527)
train.rf.oob <- train(y = train$SalePrice,
                      x = subset(train, select=-c(SalePrice)),
                      method="rf",
                      ntree=500, nodesize=25,
                      tuneGrid=data.frame(mtry=seq(20,30,2)),
                      trControl=trainControl(method="oob") )
train.rf.oob
plot(train.rf.oob$results$Rsquared, train.rf.oob$results$mtry)
plot(train.rf.oob$results$RMSE, train.rf.oob$results$mtry)
train.rf.oob$results$mtry[train.rf.oob$results$Rsquared >= max(train.rf.oob$results$Rsquared)]
train.rf.oob$bestTune

### Variable importance with the random forest
# RF variable importance
RF_importance_scores <- rf.md1$importance[,1]
n_variables = 20 # how many variables to display?
barplot( tail( sort(RF_importance_scores), n_variables ),
         beside = TRUE,
         horiz = TRUE,
         las=1,
         main = paste("Random Forest - top", n_variables, "importance scores"),
         cex.names =.7)

# Variable importance comparison
all_variables = names(CART_importance_scores)
# Make a blank plot by setting col=0
plot(CART_importance_scores[all_variables], RF_importance_scores[all_variables], col=0,
     main = "CART vs RF importance scores",
     xlab="CART", ylab="RF")
# Label variables on the plot
text(CART_importance_scores[all_variables], RF_importance_scores[all_variables], all_variables, cex=.55)
