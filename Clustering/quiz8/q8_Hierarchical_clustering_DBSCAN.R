### Quiz 8 - Hierarchical clustering and DBSCAN 
# importing data
dcdata = read.csv("dcdata.txt", stringsAsFactors = TRUE)
target = dcdata[,3]
dcdata= dcdata[,1:2]

# Apply 1)hierarchical clustering with single linkage
#       2)hierarchical clustering with 
#       3)DBSCAN with minPts=5 and for eps 0.75, 1.00, 1.25, 1.50
#       4)k-Means with 2 clusters
# Which algorithms are separate the data properly.


plot(dcdata,pch=15)
d = dist(dcdata)
hc_single = hclust(d, method = "single")
clusters_single = cutree(hc_single, k = 2)
plot(dcdata, col=clusters_single+1, pch=15, main="single hierch")
acc_h_single = Accuracy(clusters_single,target)

hc_complete = hclust(d, method = "complete")
clusters_complete= cutree(hc_complete, k = 2)
plot(dcdata, col=clusters_complete+1, pch=15, main="complete hierch")
acc_h_complete = Accuracy(clusters_complete,target)

library(dbscan)
# apply DBSCAN on our data and create our model , radius = eps , minPts = min points
model = dbscan(dcdata, eps = 0.75, minPts = 5)
plot(dcdata, col=model$cluster+1, pch=15, main="DBSCAN(eps = 0.75, minPts = 5)")
acc_DB_0_75 = Accuracy(model$cluster,target)



model = dbscan(dcdata, eps = 1, minPts = 5)
plot(dcdata, col=model$cluster+1, pch=15, main="DBSCAN(eps = 1, minPts = 5)")
acc_DB_1 = Accuracy(model$cluster,target)
model = dbscan(dcdata, eps = 1.25, minPts = 5)
plot(dcdata, col=model$cluster+1, pch=15, main="DBSCAN(eps = 1.25, minPts = 5)")
acc_DB_1_25 = Accuracy(model$cluster,target)
model = dbscan(dcdata, eps = 1.5, minPts = 5)
plot(dcdata, col=model$cluster+1, pch=15, main="DBSCAN(eps = 1.5, minPts = 5)")
acc_DB_1_5 = Accuracy(model$cluster,target)

#k-means
model = kmeans(dcdata, centers = 2)
plot(dcdata, col=model$cluster+1, pch=15, main="k-means)")
acc_kmean = Accuracy(model$cluster,target)
# Answer (from plot): Hierarchical clustering with complete linkage and k-Means

# For Hierarchical clustering (single link) with 2 clusters, find Accuracy
hc_single = hclust(d, method = "single")
clusters_single = cutree(hc_single, k = 2)
acc = Accuracy(clusters_single,target)
plot(dcdata, col=clusters_single+1, pch=15, main="single hierch")
# For Hierarchical clustering (complete link) with 2 clusters, find Accuracy
library(MLmetrics)
acc = Accuracy(clusters_complete,target)
plot(dcdata, col=clusters_complete+1, pch=15, main="single hierch")