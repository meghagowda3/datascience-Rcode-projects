

library(readr)
SalaryData_Train <- read_csv("E:/Data Science/Assignments/Naive_Bayes/SalaryData_Train.csv")
View(SalaryData_Train)
str(SalaryData_Train)
set.seed(135)
train<-order(runif(135))
t1<-train
t2<-train
test<--train
training<-SalaryData_Train[train,]
testing<-SalaryData_Train[test,]
table(training[,5])
library(e1071)
model_salary<-naiveBayes(training$Salary~.,data = training[,-5])
pred_salary<-predict(model_salary,testing[,-5])
mean(pred_salary==testing[,5])
table(pred_salary)
table(testing[,5])

boxplot(SalaryData_Train$age)
boxplot(SalaryData_Train$capitalloss)
summary(SalaryData_Train$Salary)
