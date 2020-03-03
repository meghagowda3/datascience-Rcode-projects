library(readr)
Company_Data <- read_csv("E:/Data Science/Assignments/Decision Trees/Company_Data.csv")
View(Company_Data)
library(caret)
library(C50)
str(Company_Data)
sales<-as.factor(Company_Data$Sales)
Company_Data<-Company_Data[,-1]
View(Company_Data)
Company_Data<- cbind(Company_Data,sales)
str(Company_Data)
View(Company_Data)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(Company_Data$sales,p=.50,list=F)
training <- Company_Data[inTraininglocal,]
View(training)
testing <- Company_Data[-inTraininglocal,]
table(testing$sales)
#model building
model <- C5.0(training$sales~.,data = training,trails = 40)
# Generating the model summary
summary(model)
plot(model)
pred <- predict.C5.0(model,testing[,-11])
table(pred)
a <- table(testing$sales,pred)
a
sum(diag(a)/sum(a))
plot(model)

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(Company_Data$sales,p=.50,list=F)
  training1<-Company_Data[inTraininglocal,]
  testing<-Company_Data[-inTraininglocal,]
  
  fittree<-C5.0(training1$sales~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-11])
  a<-table(testing$sales,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

acc
summary(acc)
