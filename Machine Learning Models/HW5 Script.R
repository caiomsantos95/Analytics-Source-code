
# packages
install.packages("flexclust")
library(tidyverse)
library(flexclust)
library(ggcorrplot)
library(skimr)

# read data
data = read.csv("autoSurvey.csv")
skim(data)
male_preference <- filter(data, data$gender == 0) 
female_preference <- filter(data, data$gender == 1)
skim(male_preference)
skim(female_preference)
view(data)

##1b - correlation matrix
ggcorrplot(cor(data))

##1d HIERARCHICAL CLUSTERING
d <- dist(data)    # method = "euclidean"
class(d)

#### CREATE THE CLUSTERS FUNCTION
hclust.mod <- hclust(d, method="ward.D2")
plot(hclust.mod, labels=F, ylab="Dissimilarity", xlab = "", sub = "")

### CUT CLUSTERS - 2 CLUSTERS
h.clusters <- cutree(hclust.mod, 2)
plot(h.clusters, labels=F, ylab="Dissimilarity", xlab = "", sub = "")

### AGGREGATE
aggregate(data, by=list(h.clusters), mean)
table(h.clusters)

### CLUSTERS ANALYSIS
# cluster plots ("assignments" is the assignment of data points to clusters)
clusterMeans = sapply(split(data, h.clusters), colMeans)
heatmap(clusterMeans, scale="none", main="Unscaled scores")
heatmap(clusterMeans, scale="row", 
        main="Which cluster scores highest on each variable?")
heatmap(clusterMeans, scale="col", 
        main="Which variables score highest in each cluster?")


### CUT CLUSTERS - 3 CLUSTERS
three.clusters <- cutree(hclust.mod, 3)
plot(three.clusters, labels=F, ylab="Dissimilarity", xlab = "", sub = "")

### AGGREGATE
aggregate(data, by=list(three.clusters), mean)
table(three.clusters)

### CLUSTERS ANALYSIS
# cluster plots ("assignments" is the assignment of data points to clusters)
clusterMeans_three = sapply(split(data, three.clusters), colMeans)
heatmap(clusterMeans_three, scale="none", main="Unscaled scores")
heatmap(clusterMeans_three, scale="row", 
        main="Which cluster scores highest on each variable?")
heatmap(clusterMeans_three, scale="col", 
        main="Which variables score highest in each cluster?")


#### SCREE PLOT
hc.dissim <- data.frame(k = seq_along(hclust.mod$height),   # index: 1,2,...,length(hclust.mod$height)
                        dissimilarity = rev(hclust.mod$height)) # reverse elements
head(hc.dissim)

# Scree plot
plot(hc.dissim$k, hc.dissim$dissimilarity, type="l")
# Let's zoom on the smallest k values:
plot(hc.dissim$k, hc.dissim$dissimilarity, type="l", xlim=c(0,20))
axis(side = 1, at = 1:10)

### 5 CLUSTERS
five.clusters <- cutree(hclust.mod, 5)
plot(five.clusters, labels=F, ylab="Dissimilarity", xlab = "", sub = "")

### AGGREGATE
aggregate(data, by=list(five.clusters), mean)
table(five.clusters)

### CLUSTERS ANALYSIS
# cluster plots ("assignments" is the assignment of data points to clusters)
clusterMeans_five = sapply(split(data, five.clusters), colMeans)
heatmap(clusterMeans_five, scale="none", main="Unscaled scores")
heatmap(clusterMeans_five, scale="row", 
        main="Which cluster scores highest on each variable?")
heatmap(clusterMeans_five, scale="col", 
        main="Which variables score highest in each cluster?")

### Clustering: K-Means
set.seed(2407)
kmeans_model <- kmeans(data, iter.max=100, 5) # Change NumberOfClusters to the number of clusters you used in 1g
class(kmeans_model)
kmeans_model.size <- kmeans_model$size
kmeans_model.size

k.data <- data.frame(k = 1:100)
k.data$SS <- sapply(k.data$k, function(k) {
  kmeans(data, iter.max=100, k)$tot.withinss
})

# Plot the scree plot.
plot(k.data$k, k.data$SS, type="l", xlim=c(0,30))
kmeans_model$centers

heatmap(kmeans_model$centers, scale="none", main="Unscaled scores")
heatmap(kmeans_model$centers, scale="row", 
        main="Which cluster scores highest on each variable?")
heatmap(kmeans_model$centers, scale="col", 
        main="Which variables score highest in each cluster?")

table(five.clusters, kmeans_model$cluster)
