# Read the dataset
library(readr)
Zoo <- read_csv("E:/Data Science/Assignments/KNN/Zoo/Zoo.csv")
View(Zoo)
#First colum in dataset is id which is not required so we will be taking out
Zoo <- Zoo[-1]
View(Zoo)

table(Zoo$type)

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(Zoo$type))*100,1)
summary(Zoo)
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
#norm(c(1,2,3,4,5))
#norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
Zoo_n <- as.data.frame(lapply(Zoo[1:17], norm))
View(Zoo_n)

#create training and test datasets
Zoo_train <- Zoo_n[1:70,]
Zoo_test <- Zoo_n[71:101,]

#Get labels for training and test datasets

Zoo_train_labels <- Zoo[1:70,17]
Zoo_test_labels <- Zoo[71:101,17]

# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_Zoo_pred <- knn(train=Zoo_train,test=Zoo_test,cl=Zoo_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_Zoo_pred==Zoo_train_labels))
  test_wbcd_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_Zoo_pred==Zoo_test_labels))
}


# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#Below is the final method 
Zoo_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=21)

