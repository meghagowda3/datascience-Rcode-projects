library(readr)
SalaryData_Test <- read_csv("E:/Data Science/Assignments/Naive_Bayes/SalaryData_Test.csv")
View(SalaryData_Test)
str(SalaryData_Test)
set.seed(135)
train<-order(runif(135))
t1<-train
t2<-train
test<--train
training<-SalaryData_Test[train,]
testing<-SalaryData_Test[test,]
table(training[,3])
library(e1071)
model_Salary<-naiveBayes(training$Salary~.,data = training[,-3])
pred_Salary<-predict(model_Salary,testing[,-3])
mean(pred_Salary==testing[,3])
table(pred_Salary)
table(testing[,3])

boxplot(SalaryData_Test$age)
boxplot(SalaryData_Test$capitalloss)
summary(SalaryData_Test$Salary)
