library(readr)
emp_data <- read_csv("E:/Data Science/Assignments/Linear_Regression/emp_data.csv")
View(emp_data)

# Exploratory data analysis
summary(emp_data)

#Scatter plot
plot(emp_data$Salary_hike, emp_data$Churn_out_rate)  # plot(X,Y)

?plot

attach(emp_data)


#Correlation Coefficient (r)
cor(Salary_hike,Churn_out_rate)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Churn_out_rate ~ Salary_hike) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=Churn_out_rate))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(wc_at,aes(Waist,AT))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(Salary_hike); y = Churn_out_rate

plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)

reg_log <- lm(Churn_out_rate ~ log(Salary_hike))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp_data))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = Salary_hikei and y = log(Churn_out_rate)

plot(Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))

reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error = emp_data$Churn_out_rate - at
error

sqrt(sum(error^2)/nrow(emp_data))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(Salary_hike, Churn_out_rate)
plot(Salary_hike*Salary_hike, Churn_out_rate)

cor(Salary_hike*Salary_hike, Churn_out_rate)

plot(Salary_hike*Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))
cor(Salary_hike*Salary_hike, log(Churn_out_rate))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = emp_data$Churn_out_rate - expy

sqrt(sum(err^2)/nrow(emp_data))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = emp_data, aes(x = Salary_hike + I(Salary_hike^2), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike+I(Salary_hike^2), y=logpol))
##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Churn_out_rate)~ Salary_hike + I(Salary_hike*Salary_hike) + I(Salary_hike*Salary_hike*Salary_hike))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = emp_data, aes(x = Salary_hike + I(Salary_hike^2) + I(Salary_hike^3), y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike+I(Salary_hike^2)+I(Salary_hike^3), y=expy3))
#