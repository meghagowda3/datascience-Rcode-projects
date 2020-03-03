
library(readxl)
library(xlsx) 
library(openxlsx)
library(WriteXLS)
library(readr)
cof <- read_csv("E:/Data Science/Assignments/Hypothesis Testing/Costomer+OrderForm.csv")
View(cof)
# Customer order form 
# Unstacked data 

cof<-read_excel(file.choose()) # customer_order(unstacked).xlsx
View(cof) # countries are in their own columns; so we need to stack the data 
stacked_cof<-stack(cof)
attach(stacked_cof)
View(stacked_cof)
table(stacked_cof$ind,stacked_cof$values)
chisq.test(table(stacked_cof$ind,stacked_cof$values))


