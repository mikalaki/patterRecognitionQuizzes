## Lab 6 - Data preprocessing -PCA - ISOMAP quiz ##

#importing DATA
data(Glass, package = "mlbench")
training = Glass[c(1:50,91:146),-10]
trainingType = factor(Glass[c(1:50,91:146),10])
testing = Glass[51:90,-10]
testingType = factor(Glass[51:90, 10])


# Apply PCA to training data with center=TRUE, scale=TRUE
#and find what percentage of initial dataset's information is in PC1
pca_model <- prcomp(training, center = TRUE, scale = TRUE)


eigenvalues = pca_model$sdev^2
perc1 = eigenvalues[1]/sum(eigenvalues);

# What is the percentage of information loss, if only the first 4 principals are hold.
perc2 =(sum(eigenvalues) - sum(eigenvalues[c(1:4)])) /sum(eigenvalues)

#apply KNN with k=3 (without PCA) and find the Accuracy's value.
library(class)
library(e1071)
library(MLmetrics)
predy = knn(training, testing, trainingType, k = 3, prob = TRUE)
acc = Accuracy(predy, testingType)
acc

# Find the Recall's metric value for the previous KNN (take class 2 as positive)
rec = Recall(testingType, predy, positive = 2)

#Apply PCA (center= TRUE and scale=TRUE), train a kNN(k=3), which is the optimal
#number of the Principal Components to hold?
pca_model <- prcomp(training, center = TRUE, scale = TRUE)
eigenvalues = pca_model$sdev^2
accuracies = c(1:length(eigenvalues))
for (i in c(1:length(eigenvalues)))
{
  data_pc <- as.data.frame(predict(pca_model, training)[, 1:i])
  test_pc <- as.data.frame(predict(pca_model, testing)[, 1:i])
  predy = knn(data_pc, test_pc, trainingType, k = 3, prob = TRUE)
  accu = Accuracy(predy, testingType)
  
  accuracies[i]= accu
  
}
plot(accuracies
     )

# From the plot optimas PCs number is 6
