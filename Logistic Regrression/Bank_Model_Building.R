Bank = read.csv("E://Data Science//Assignments//Logistic Regression//bank-full.csv",sep = ";")

View(Bank)
attach(Bank)


# Logistic Regression 
str(Bank)
logit<-glm(y~factor(age)+job+marital+education+default+factor(balance)+housing+loan+contact+factor(day)+month+factor(duration)+factor(pdays)+factor(campaign)+factor(previous)+poutcome,data =Bank)

# Confusion matrix table 
prob <- predict(logit,type=c("response"),Bank)
prob
confusion<-table(prob>0.5,Bank$y)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,Bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
#  More area under the ROC Curve better is the logistic regression model obtained