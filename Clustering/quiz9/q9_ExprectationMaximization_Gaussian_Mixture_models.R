### Expectation Maximization - Gaussian Models Mixtures
#importing data
kmdata = read.csv("kmdata.txt", stringsAsFactors = TRUE)
target = kmdata[,3]
kmdata= kmdata[,c(1:2)]
#plotting data
plot(kmdata)
#plotting data by class
plot(kmdata, col = target)

# apply k-Means for 3 clusters to the data and plotting the clusters
model = kmeans(kmdata, centers = 3)
plot(kmdata, col = model$cluster, pch=15, main="k-means")
points(model$centers, col = 4, pch = "+", cex = 2)

# apply Gaussian Mixture Models for 3 clusters to the data and plotting the clusters

model_gmm = mvnormalmixEM(kmdata, k = 3 , epsilon = 0.01)
plot(kmdata, col = max.col(model_gmm$posterior), pch=15, main="gmm")
points(matrix(unlist(model_gmm$mu), byrow = TRUE, ncol = 2), col = 4, pch = "+", cex = 2)
# From the above plots, which model seperate data better?
# answer : Gaussian Mixture Models (GMM)
