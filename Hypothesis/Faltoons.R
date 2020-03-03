setwd("E:\\Excelr Data\\R Codes\\Hyothesis Testing")

##### Normality Test##################

library(readxl)
library(xlsx) 
library(openxlsx)
library(WriteXLS)
library(readr)
Faltoons <- read_csv("E:/Data Science/Assignments/Hypothesis Testing/Faltoons.csv")
View(Faltoons)


attach(Faltoons)
table1 <- table(Weekdays,Weekend)
table1
?prop.test
prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "less")


chisq.test(table(Weekdays,Weekend))




