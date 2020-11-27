# Quiz 3 - Naive Bayesian Classifier & Classifier Evaluation.

# proper library for Naive Bayesian Classifier.
library(e1071);

# proper libraries for classifiers' evaluation
library(MLmetrics);
library(ROCR);

# importing our data
Class = c(1, 1, 0, 0, 1, 1, 0, 0, 1, 0)
P_M1 = c(0.73, 0.69, 0.44, 0.55, 0.67, 0.47, 0.08, 0.15, 0.45, 0.35)
P_M2 = c(0.61, 0.03, 0.68, 0.31, 0.45, 0.09, 0.38, 0.05, 0.01, 0.04)
data = data.frame(Class, P_M1, P_M2)

#thresshold
t = 0.5;

# getting TPR FPR
pred_obj_M1 = prediction(data$P_M1, data$Class , label.ordering = c("0", "1"))
pred_obj_M2 = prediction(data$P_M2, data$Class , label.ordering = c("0", "1"))

# getting ROC curve
ROC_M1 <- performance(pred_obj_M1, "tpr","fpr")
ROC_M2 <- performance(pred_obj_M2, "tpr","fpr")


# getting our data in proper form
cutoffs <- data.frame(cut=ROCcurve@alpha.values[[1]], fpr=ROCcurve@x.values[[1]], tpr=ROCcurve@y.values[[1]])
cutoffval = cutoffs[which.max(which(cutoffs$cut-0.0002 > 0.5)), 1]
tpr = cutoffs[which.max(which(cutoffs$cut-0.0002 > 0.5)), 3]

# ploting ROC curve

plot(ROC_M1, col = "blue")
abline(0,1, col = "grey")
plot(ROC_M2, col = "red", add = TRUE)
legend(1,96,legend=c("M1","M2"),
       col=c("blue", "red"), lty=1:2, cex=0.8);

# F- measure for M2
#actual predictions of model 2

pred2 = data$P_M2;
pred2[which(pred2>=t)]= 1;
pred2[which(pred2<t)]= 0;
f_m2 =F1_Score(data$Class, pred2,positive = 1)

# area under curve for model 2
AUC_M2 = performance(pred_obj_M2, "auc")
AUC_M2 = AUC_M2@y.values[[1]]
