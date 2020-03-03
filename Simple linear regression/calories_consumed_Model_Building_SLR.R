# Load wc_at.csv dataset
library(readr)
calories_consumed <- read_csv("E:/Data Science/Assignments/Linear_Regression/calories_consumed.csv")
View(calories_consumed)

# Exploratory data analysis
summary(calories_consumed)

#Scatter plot
plot(calories_consumed$`Calories Consumed`, calories_consumed$`Weight gained (grams)`)  # plot(X,Y)

?plot

attach(calories_consumed)


#Correlation Coefficient (r)
cor(`Calories Consumed`,`Weight gained (grams)`)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(`Weight gained (grams)`~ `Calories Consumed`) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)
pred
reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(calories_consumed))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = calories_consumed, aes(x = `Calories Consumed`, y = `Weight gained (grams)`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=`Calories Consumed`, y=pred))

?ggplot2


# Logrithamic Model

# x = log(calories Consumed); y = Weight gained (grams)

plot(log(`Calories Consumed`), `Weight gained (grams)`)
cor(log(`Calories Consumed`), `Weight gained (grams)`)

reg_log <- lm(`Weight gained (grams)`~ log(`Calories Consumed`))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(calories_consumed))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = calories Consumed and y = log(Weight gained (grams))

plot(`Calories Consumed`, log(`Weight gained (grams)`))

cor(`Calories Consumed`, log(`Weight gained (grams)`))

reg_exp <- lm(log(`Weight gained (grams)`) ~ `Calories Consumed`)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error = calories_consumed$`Weight gained (grams)` - at
error

sqrt(sum(error^2)/nrow(calories_consumed))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(`Calories Consumed`, `Weight gained (grams)`)
plot(`Calories Consumed`*`Calories Consumed`, `Weight gained (grams)`)

cor(`Weight gained (grams)`,`Calories Consumed`*`Calories Consumed`)

plot(`Calories Consumed`*`Calories Consumed`, log(`Weight gained (grams)`))

cor(`Calories Consumed`, log(`Weight gained (grams)`))
cor(`Calories Consumed`*`Calories Consumed`, log(`Weight gained (grams)`))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(`Weight gained (grams)`) ~ `Calories Consumed` + I(`Calories Consumed`*`Calories Consumed`))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = calories_consumed$`Weight gained (grams)` - expy

sqrt(sum(err^2)/nrow(calories_consumed))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = calories_consumed, aes(x = `Calories Consumed` + I(`Calories Consumed`^2), y = log(`Weight gained (grams)`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=`Calories Consumed`+I(`Calories Consumed`^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(`Weight gained (grams)`)~`Calories Consumed` + I(`Calories Consumed`*`Calories Consumed`) + I(`Calories Consumed`*`Calories Consumed`*`Calories Consumed`))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = calories_consumed, aes(x = `Calories Consumed` + I(`Calories Consumed`^2) + I(`Calories Consumed`^3), y = `Weight gained (grams)`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=`Calories Consumed`+I(`Calories Consumed`^2)+I(`Calories Consumed`^3), y=expy3))

################################
