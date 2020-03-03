View(Computer_Data)
attach(Computer_Data)


summary(Computer_Data)
CD<-as.factor(Computer_Data$cd)
MULTI<- as.factor(Computer_Data$multi)
PREMIUM<-as.factor(Computer_Data$premium)
Computer_Data_New<- Computer_Data [,-c(7,8,9)]
View(Computer_Data_New)
Computer_Data_New<-cbind(Computer_Data_New,CD,MULTI,PREMIUM)
View(Computer_Data_New)
str(Computer_Data_New)

# 7. Find the correlation b/n Output & Input -Scatter plot
pairs(Computer_Data_New)
plot(Computer_Data_New)
# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Computer_Data_New)
install.packages("psych")
library(psych)
pairs.panels(Computer_Data_New)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Computer_Data_New))


# The Linear Model of interest
model.Computer <- lm(price~speed+hd+ram+screen+ads+trend,data=Computer_Data_New)
summary(model.Computer)

# Prediction based on only speed 
model.ComputerS<-lm(price~speed)
summary(model.ComputerS) # Speed became significant

# Prediction based on only HD
model.ComputerH<-lm(price~hd)
summary(model.ComputerH) # hd became significant

# Prediction based on ads 
model.ComputerA<-lm(price~ads)
summary(model.ComputerA) # ads became Insignificant

# Prediction based on screen
model.ComputerS<-lm(price~screen)
summary(model.ComputerS)

# Prediction based on RAM
model.ComputerR<-lm(price~ram)
summary(model.ComputerR)

# Prediction based on trend
model.ComputerT<-lm(price~trend)
summary(model.Computer)


### Scatter plot matrix along with Correlation Coefficients

library(psych)
pairs.panels(Computer_Data_New)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.Computer)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.Computer,id.n=3) # index plots for infuence measures
influencePlot(model.Computer,id.n=3) # A user friendly representation of the above

# Regression after deleting the  observation, which is influential observation
model.Computer11<-lm(price~speed+hd+ram+screen+ads+trend,data=Computer_Data_New[-c(1701,1441,3784,4478),])
summary(model.Computer11)

## Variance Inflation factor to check collinearity b/n variables 
vif(model.Computer)
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.Computer,id.n=2,id.cex=0.7)

## VIF and AV plot has given us an indication to delete "wt" variable

## Final model
finalmodel<-lm(price~speed+hd+ram+screen+ads+trend)
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model.Computer,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 

