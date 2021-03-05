# K-means and clustering evaluation

#importing data
sdata = read.csv("sdata.txt", stringsAsFactors = TRUE)

# apply Kmeans with initial centres the spots: (-4,10),(0,0) and (4,10)
# What is the value of the cohesion metric?
X1= c(-4,0,4)
X2= c(10,0,10)
input = data.frame(X1,X2)

model = kmeans(sdata, centers = input)
cohesion = model$tot.withinss
# What is the value of the seperation metric?
seperation = model$betweenss 
# What is the value of the seperation metric?
library(cluster)
# What is the value of the silhouette metric?
model_silhouette = silhouette(model$cluster, dist(sdata))
mean_silhouette = mean(model_silhouette[, 3])


# apply KNN initial centres the spots: (-2,0),(2,0) and (0,10)
# Is clustering better or worse
X1= c(-2,2,0)
X2= c(0,0,10)
input = data.frame(X1,X2)

model = kmeans(sdata, centers = input)
seperation = model$betweenss 
model_silhouette = silhouette(model$cluster, dist(sdata))
mean_silhouette = mean(model_silhouette[, 3])
# answer --> worse
