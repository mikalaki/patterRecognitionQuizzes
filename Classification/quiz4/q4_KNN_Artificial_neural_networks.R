# Lab 4 homework solutions (KNN - aNN) problems
# mikalaki

# importing our data for the KNN problem
X1 = c(-2.0, -2.0, -1.8, -1.4, -1.2, -1.2,
       1.3, 1.3, 2.0, 2.0, -0.9, -0.5, 
       -0.2, 0.0, 0.0, 0.3, 0.4, 0.5, 
       0.8,1.0)
X2 = c(-2.0, 1.0, -1.0, 2.0, 1.2, 1.0, 
       -1.0, 2.0, 0.0, -2.0, 0.0, -1.0,
       1.5, 0.0, -0.5, 1.0 ,0.0, -1.5, 1.5,
       0.0)
Y=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

alldata= data.frame(X1,X2,Y)

# KNN
#importing proper library
library(class)

# plotting properly
plot(alldata[,c("X1","X2")], col = alldata$Y, pch = c("o","+")[alldata$Y])
X_train = alldata[,c("X1","X2")];
Y_train =  alldata$Y


#knn(X_train, c(0.7, 0.4), Xtest, k = 1, prob = TRUE)

#for K =3 neighbours classifie (1.5, -0.5) 
Xtest1 = c(1.5, -0.5)
result1KNN = knn(X_train, Xtest1, Y_train, k = 3, prob = TRUE);

#for K =5 neighbours classifie (-1, 1)
Xtest2 = c(-1, 1)
result2KNN = knn(X_train, Xtest2, Y_train, k = 5, prob = TRUE);

# ANN
#importing proper library 
library(neuralnet)


# importing our data for the ANN (artificial Neural Network) problem
X1_NN = c(2, 2, -2, -2, 1, 1, -1, -1)
X2_NN = c(2, -2 , -2, 2, 1, -1, -1, 1)

Y_NN= c(1, 1, 1, 1, 2, 2, 2, 2)

alldataANN = data.frame(X1_NN,X2_NN,Y_NN)

# Creating our NN with 1 hidden layer of 2 neurons
model <- neuralnet(Y_NN ~ X1_NN + X2_NN, data = alldataANN,
                   hidden = c(2), threshold = 0.01)

X_training = alldataANN[,c(1,2)]
Y_training = alldataANN$Y_NN

Y_est_training = compute(model, X_training)$net.result

# calculating its mean training error
mean_training_error = mean(abs(Y_est_training - Y_training))
