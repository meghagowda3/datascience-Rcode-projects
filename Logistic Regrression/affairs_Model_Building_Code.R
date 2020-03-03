
View(AFFAIRS)
attach(AFFAIRS)


# Logistic Regression 
str(AFFAIRS)
logit<-glm(factor(gender)~affairs+factor(children)+age+yearsmarried+religiousness+education+occupation+rating,family=binomial,data =AFFAIRS)
summary(logit)

logit1<-glm(factor(gender)~affairs+factor(children)+age+yearsmarried+education+occupation,family=binomial,data =AFFAIRS)
summary(logit1)

exp(coef(logit1))
table(AFFAIRS$gender)

# Confusion matrix table 
prob <- predict(logit1,type=c("response"),AFFAIRS)
prob
confusion<-table(prob>0.5,AFFAIRS$gender)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,AFFAIRS$gender)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
#  More area under the ROC Curve better is the logistic regression model obtained