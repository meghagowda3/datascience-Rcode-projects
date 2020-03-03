setwd("E://Data Science//Assignments//Neural Networks")
library(readr)
Startups <- read_csv("50_Startups.csv")
View(Startups)
str(Startups)
attach(Startups)
State<-as.numeric(Startups$State)
Startups<-Startups[,-4]
Startups<-cbind(Startups,State)
str(Startups)
#normal_concrete<-scale(concrete)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups,FUN=normalize))
summary(Startups_norm$Profit)
#summary(normal_concrete)
summary(Startups$Profit)
Startups_train<-Startups_norm[1:35,]
Startups_test<-Startups_norm[35:50,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)

# Building model
Startups_model <- neuralnet(Profit~ R.D.Spend+Administration+Marketing.Spend,data = Startups_train)
str(Startups_model)
plot(Startups_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(Startups_model,Startups_test[1:4])
predicted_strength <- model_results$net.result
predicted_strength
model_results$neurons
cor(predicted_strength,Startups_test$Profit)
plot(predicted_strength,Startups_test$Profit)
model_5<-neuralnet(Profit~ R.D.Spend+Administration+Marketing.Spend,data= Startups_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,Startups_test[1:4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,Startups_test$Profit)
plot(pred_strn_5,Startups_test$Profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased

