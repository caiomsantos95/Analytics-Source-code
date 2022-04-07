######## LEARN HOW TO DO THE BELOW SIMULATION
# To avoid copying and pasting the commmands again and again, we can 
# write a for-loop  

metrics = data.frame(cutoff1 = 0.0, 
                     Accuracy1 = 0.6465, 
                     Error_Rate1 = 0.3544, 
                     FNR1 = 0.0, 
                     FPR1 = 1.0)

cutoffs = seq(0.195, 0.9, 0.05)

for (i in cutoffs) {
  test = mutate(test, predicted_to_fake_news = ifelse(probs > i,"Predicted_Fake_news","Predicted_Normal"))
  confusion_matrix1 = table(test$predicted_to_fake_news, test$actual_fake_news)
  print(confusion_matrix1)
  accuracy1 = (confusion_matrix1[1,1] + confusion_matrix1[2,2]/ nrow(test))
  error_rate1 = 1 - accuracy1
  FNR1 = confusion_matrix1[2,1] / (confusion_matrix1[1,1] + confusion_matrix1[2,1])
  FPR1 = confusion_matrix1[1,2] / (confusion_matrix1[1,2] + confusion_matrix1[2,2])
  metrics = rbind(metrics, c(i,accuracy1, error_rate1, FNR1, FPR1))
}

metrics = rbind(metrics, 
                c(1.0,0.646, 0.3544, 1.0, 0.0))
metrics

ggplot(data = metrics) +
  geom_line(aes(x = cutoff1, y = FNR1), color = "red") +
  geom_line(aes(x = cutoff1, y = FPR1), color = "turquoise") +
  ylab("FNR1/FPR1")
