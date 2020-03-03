library(readr)
library(randomForest)
Fraud_check <- read_csv("E:/Data Science/Assignments/Random Forests/Fraud_check.csv")
View(Fraud_check)
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

# Building a random forest model on training data 
fit.forest <- randomForest(Marital_Status~.,data=training,na.action=na.roughfix,importance=TRUE)
fit.forest$ntree

# Training accuracy 
mean(training$Marital_Status==predict(fit.forest,training)) # 100% accuracy 

# Predicting test data 
pred_test <- predict(fit.forest,newdata=testing)
mean(pred_test==testing$Marital_Status) 
library(gmodels)
# Cross table 
rf_perf<-CrossTable(testing$Marital_Status, pred_test,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))

