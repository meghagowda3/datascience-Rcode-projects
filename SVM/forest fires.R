library(readr)
forestfires <- read_csv("E:/Data Science/Assignments/Support Vector Machines/forestfires.csv")

forestfires<-forestfires[,-1]
View(forestfires)
sizecategory<-as.factor(forestfires$size_category)
forestfires<-forestfires[,-29]
View(forestfires)
forestfires<-cbind(forestfires,sizecategory)

# data is randomly arranged 
forestfires_train<-forestfires[1:300,]
forestfires_test<-forestfires[301:517,]
str(forestfires)


# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 


# kvsm() function uses gaussian RBF kernel 

# Building model 

library(kernlab)
library(caret)
model1<-ksvm(forestfires ~.,data = forestfires_train,kernel = "vanilladot")


# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(forestfires ~.,data = forestfires_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=forestfires_test)
mean(pred_rfdot==forestfires$sizecategory) 

# kernel = vanilladot
model_vanilla<-ksvm(forestfires ~.,data = forestfires_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=forestfires_test)
mean(pred_vanilla==forestfires$sizecategory) 


# kernal = besseldot
model_besseldot<-ksvm(forestfires ~.,data = forestfires_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=forestfires_test)
mean(pred_bessel==forestfires$sizecategory)

# kernel = polydot

model_poly<-ksvm(forestfires ~.,data = forestfires_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata=forestfires_test)
mean(pred_poly==forestfires$sizecategory)
