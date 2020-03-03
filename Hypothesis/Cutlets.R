

##### Normality Test##################

library(readxl)
library(xlsx) 
library(openxlsx)
library(WriteXLS)
library(readr)
Cutlets <- read_csv("E:/Data Science/Assignments/Hypothesis Testing/Cutlets.csv")
View(Cutlets)

######## Promotion.xlsx data ###################

attach(Cutlets)

#############Normality test###############

shapiro.test(Cutlets$`Unit A`)
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Cutlets$`Unit B`)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############

var.test(Cutlets$`Unit A`,Cutlets$`Unit B`)#variance test
# p-value = 0.3136 > 0.05 so p high null fly => Equal variances

############2 sample T Test ##################

t.test(Cutlets$`Unit A`,Cutlets$`Unit B`,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.4723 > 0.05 accept Null Hypothesis 

t.test(Cutlets$`Unit A`,Cutlets$`Unit B`,alternative = "greater",var.equal = T)

# alternative = "greater means true difference is greater than 0

# p-value = 0.2361 > 0.05 => accept Null Hypothesis
# There is no significant difference in the diameter of the cutlet between two units

