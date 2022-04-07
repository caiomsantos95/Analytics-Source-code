library(tidyverse)
library(skimr)

### PREPARING THE DATESET
df = read_csv("fake_news.csv") 
## setting the seed
set.seed(15060)
## creating additional column for splitting data set 
## (splitter is a column created with random numbers from 0 to 1)
splitter = runif(nrow(df))
head(df)
## splitting dataset
train = filter(df, splitter < 0.7)
test = filter(df, splitter >= 0.7)
# Take a look
glimpse(train)
glimpse(test)
skim(train)
skim(test)

### BUILDING THE MODEL
mod = glm(data = train, fake_news_account ~., family="binomial")
summary(mod)
mod1 = glm(data = train, fake_news_account ~. -positive_posts_etc, family="binomial")
summary(mod1)
mod2 = glm(data = train, fake_news_account ~. -positive_posts_etc -profile_picture, family="binomial")
summary(mod2)
mod3 = glm(data = train, fake_news_account ~. -positive_posts_etc -profile_picture -state_actor_country, family="binomial")
summary(mod3)

### PREDICT THE VALUE FOR A GIVEN OBSVERVATION
new_obs = data.frame(posts_per_week = 5,
                     negative_posts_etc = 3,
                     positive_posts_etc = 2,
                     political_content = 5,
                     profile_picture = 0,
                     state_actor_country = 0)
view(new_obs)
## predicting the probability
prob_new_obs = predict(mod3, newdata=new_obs, type="response")
## 0.634
  
### TESTING THE MODEL
test$probs = predict(mod3, newdata=test, type="response")
head(test,5)

## user friendly version of the 'fake news spreader' column
test = mutate(test, actual_fake_news = ifelse(fake_news_account == 1,"Fake News Account","Normal Account"))
head(test,5)
## cutoff
cutoff = 0.5

## creating the model
test = mutate(test, prediction = ifelse(probs >= cutoff, "Predict_Fake","Predict_Normal"))
head(test,10)

### CONFUSION MATRIX
confusion_matrix = table(test$prediction, test$actual_fake_news)
confusion_matrix

### METRICS FOR CONFUSION MATRIX
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / nrow(test)
accuracy
error_rate = 1 - accuracy
error_rate

# TPR = true positives / actual positives
# Equivalently:
# TPR = true positives / (true positives + false negatives) 
TPR = confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[2,1]) 
TPR

# False Negative Rate = 1 - True Positive Rate - MISSING 
FNR = 1 - TPR
FNR

# FPR = false positives / actual negatives - FALSE ALARMS
# Equivalently:
# FPR = false positives / (false positives + true negatives) 
FPR = confusion_matrix[1,2] / (confusion_matrix[1,2] + confusion_matrix[2,2]) 
FPR

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




