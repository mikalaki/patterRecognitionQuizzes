# lab 5 kNN and SVM classification problem

# 1. kNN 
# class is the library for the knn, so I load it
library(class)
# constructing out dataset
X1 = c(2, 2, -2, -2, 1, 1, -1, -1)
X2 = c(2, -2, -2, 2, 1, -1, -1, 1)
Y = c(1, 1, 1, 1, 2, 2, 2, 2)

alldata = data.frame(X1, X2, Y)

# ploting data with different colors and symbols for each class
plot(alldata[ ,c(1,2)], col = alldata$Y , pch=c("o","+")[alldata$Y])

legend("center",c("Class 1","Class 2"),pch=c("o","+"),
       col = c("black","red") )

# Given initial dataset, classify A = (4,5) for k=1 neighbor
A = c(4,5)
resultA = knn(alldata[ ,c(1,2)], A, alldata[ ,3], k = 1, prob = TRUE)
#knn(X_train, X_test, Y_train, k = 1, prob = TRUE)


# for k=3 neighbors, what is the probability for B = (1.8,4) to be classified as "2"
B = c(1.8,4)
resultB = knn(alldata[ ,c(1,2)], A, alldata[ ,3], k = 3, prob = TRUE)

# 2. SVM
library(e1071)
#from the initial dataset construct a SVM model, with RDF (radial)
#kernel and gamma =1 
model_svm = svm(Y ~ ., alldata, kernel = "radial", type = "C-classification",gamma =1 )

#Accuracy of the model on the training set.
# getting the predictions for the training test
svm_pred = predict(model_svm, alldata[ ,c(1,2)])

#finding accuracy
library(MLmetrics) #Accuracy is part of MLmetrics library
svm_acc = Accuracy(svm_pred,alldata$Y) 

# construct a SVM model, with RDF (radial) kernel and gamma =1000000
svm_model_B =  svm(Y ~ ., alldata, kernel = "radial", type = "C-classification",gamma =1e6 )

# with the new SVM model classify C = (-2,-1.9) 
C = data.frame(X1 = c(-2),X2 = c(-1.9))
C_pred = predict(svm_model_B, C )

X1_grid = seq(min(alldata$X1) , max(alldata$X1), by = 0.05)
X2_grid = seq(min(alldata$X2) , max(alldata$X2), by = 0.05)

# plotting the hyper-plane () we get with the last SVM (svm_model_B)
mygrid = expand.grid(X1_grid, X2_grid)
colnames(mygrid) = colnames(alldata)[1:2]
pred_grid = predict(svm_model_B, mygrid);

Y = matrix( pred_grid,length(X1_grid),length(X2_grid))
contour(X1_grid, X2_grid, Y, add = TRUE, levels = 1.5, labels = "gamma = 1000000", col = "blue")
