

##### Normality Test##################

library(readxl)
library(xlsx) 
library(openxlsx)
library(WriteXLS)
library(readr)
LabTAT <- read_csv("E:/Data Science/Assignments/Hypothesis Testing/LabTAT.csv")
View(LabTAT)
attach(LabTAT)

#############Normality test###############

shapiro.test(`Laboratory 1`)
# p-value = 0.5508 >0.05 so p high null fly => It follows normal distribution

shapiro.test(`Laboratory 2`)
# p-value = 0.8637 >0.05 so p high null fly => It follows normal distribution

shapiro.test(`Laboratory 3`)
# p-value = 0.4205 >0.05 so p high null fly => It follows normal distribution

shapiro.test(`Laboratory 4`)
# p-value = 0.6619 >0.05 so p high null fly => It follows normal distribution

#############Anova (Contract_Renewal Data )##########

Stacked_Data <- stack(LabTAT)
View(Stacked_Data)
attach(Stacked_Data)
var.test(ind,values)
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value = 2e-16 < 0.05 accept Alternate hypothesis 
# All Proportions all equal 
