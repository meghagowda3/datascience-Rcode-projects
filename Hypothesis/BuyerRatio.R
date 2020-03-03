
library(readxl)
library(xlsx) 
library(openxlsx)
library(WriteXLS)
library(readr)
BuyerRatio <- read_csv("E:/Data Science/Assignments/Hypothesis Testing/BuyerRatio.csv")
View(BuyerRatio)
stacked_cof<-stack(BuyerRatio)
attach(stacked_cof)
View(stacked_cof)
table(stacked_cof$ind,stacked_cof$values)
chisq.test(table(stacked_cof$ind,stacked_cof$values))

# p-value = 0.297> 0.05 accept null hypothesis


