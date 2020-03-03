library(readr)
library(randomForest)
Company_Data <- read_csv("E:/Data Science/Assignments/Random Forests/Company_Data.csv")
View(Company_Data)
str(Company_Data)
sales<-as.numeric(Company_Data$Sales)
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

# Building a random forest model on training data 
fit.forest <- randomForest(sales~.,data=training,na.action=na.roughfix,importance=TRUE)
fit.forest$ntree

# Training accuracy 
mean(training$Sales==predict(fit.forest,training)) # 100% accuracy 

# Predicting test data 
pred_test <- predict(fit.forest,newdata=testing)
mean(pred_test==testing$Sales) 
library(gmodels)
# Cross table 
rf_perf<-CrossTable(testing$Sales, pred_test,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))

