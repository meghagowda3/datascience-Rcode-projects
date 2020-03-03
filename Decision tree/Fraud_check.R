library(readr)
Fraud_check <- read_csv("E:/Data Science/Assignments/Decision Trees/Fraud_check.csv")
View(Fraud_check)
library(caret)
library(C50)
str(Fraud_check)
Marital_Status<-as.factor(Fraud_check$Marital.Status)
Fraud_check<-Fraud_check[,-2]
View(Fraud_check)
Fraud_check<- cbind(Fraud_check,Marital_Status)
str(Fraud_check)
View(Fraud_check)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(Fraud_check$Marital_Status,p=.75,list=F)
training <- Fraud_check[inTraininglocal,]
View(training)
testing <- Fraud_check[-inTraininglocal,]
table(Fraud_check$Marital_Status)
#model building
model <- C5.0(training$Marital_Status~.,data = training,trails = 40)
# Generating the model summary
summary(model)
plot(model)
pred <- predict.C5.0(model,testing[,-6])
table(pred)
a <- table(testing$Marital_Status,pred)
a
sum(diag(a)/sum(a))
plot(model)

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(Fraud_check$Marital_Status,p=.85,list=F)
  training1<-Fraud_check[inTraininglocal,]
  testing<-Fraud_check[-inTraininglocal,]
  
  fittree<-C5.0(training1$Marital_Status~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-6])
  a<-table(testing$Marital_Status,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
}

acc
summary(acc)
