View(ToyotaCorolla)

Corolla<-ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)

str(Corolla)

attach(Corolla)

# 7. Find the correlation b/n Output & Input -Scatter plot
pairs(Corolla)
plot(Corolla)
# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Corolla)
install.packages("psych")
#psychometric package
library(psych)
pairs.panels(Corolla)


### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Corolla))


# The Linear Model of interest
model.Corolla <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla)
summary(model.Corolla)

# Prediction based on only Age_08_04 
model.CorollaA<-lm(Price~Age_08_04)
summary(model.CorollaA) # Age_08_04 became significant

# Prediction based on only KM
model.CorollaK<-lm(Price~KM)
summary(model.CorollaK) # KM became significant

# Prediction based on ads 
model.CorollaH<-lm(Price~HP)
summary(model.CorollaH) # HP became Insignificant

# Prediction based on cc
model.Corollac<-lm(Price~cc)
summary(model.Corollac)

# Prediction based on Doors
model.CorollaD<-lm(Price~Doors)
summary(model.CorollaD)# Doors became Insignificant

# Prediction based on Gears
model.CorollaG<-lm(Price~Gears)
summary(model.CorollaG)# Gears became Insignificant

# Prediction based on Gears
model.CorollaG<-lm(Price~Gears)
summary(model.CorollaG)# Gears became Insignificant

# Prediction based on Quarterly_Tax
model.CorollaQ<-lm(Price~Quarterly_Tax)
summary(model.CorollaQ)# Quarterly_Tax became Insignificant


# Prediction based on Weight
model.CorollaW<-lm(Price~Weight)
summary(model.CorollaW)# Quarterly_Tax became Insignificant


### Scatter plot matrix along with Correlation Coefficients

library(psych)
pairs.panels(Corolla)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.Corolla)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.Corolla,id.n=3) # index plots for infuence measures
influencePlot(model.Corolla,id.n=3) # A user friendly representation of the above

# Regression after deleting the  observation, which is influential observation
model.Corolla11<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla[-c(81.222,961),])
summary(model.Corolla11)

## Variance Inflation factor to check collinearity b/n variables 
vif(model.Corolla)
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.Corolla,id.n=2,id.cex=0.7)

## VIF and AV plot has given us an indication to delete "wt" variable

## Final model
finalmodel<-lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight)
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model.Corolla,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 
