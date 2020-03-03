library(readr)
SalaryData <- read_csv("E:/Data Science/Assignments/Support Vector Machines/SalaryData_Train.csv")
View(SalaryData)
str(SalaryData)

# data is randomly arranged 
SalaryData_train<-SalaryData[1:16000,]
SalaryData_test<-SalaryData[16001:20000,]
View(SalaryData)
str(SalaryData)
# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 


# kvsm() function uses gaussian RBF kernel 

# Building model 

library(kernlab)
library(caret)
model1<-ksvm(SalaryData ~.,data = SalaryData_train,kernel = "vanilladot")


# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(SalaryData ~.,data = SalaryData_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=SalaryData_test)
mean(pred_rfdot==SalaryData$Salary) 

# kernel = vanilladot
model_vanilla<-ksvm(SalaryData ~.,data = SalaryData_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=SalaryData_test)
mean(pred_vanilla==SalaryData$Salary) 


# kernal = besseldot
model_besseldot<-ksvm(SalaryData ~.,data = SalaryData_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=SalaryData_test)
mean(pred_bessel==SalaryData$Salary)

# kernel = polydot

model_poly<-ksvm(SalaryData ~.,data = SalaryData_train,kernel = "polydot")
pred_poly<-predict(model_poly,nnewdata=SalaryData_test)
mean(pred_poly==SalaryData$Salary)
