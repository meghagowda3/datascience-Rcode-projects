
View(Startups)
attach(Startups)

summary(Startups)

# 7. Find the correlation b/n Output (Profit) & (MarketingSpend,Administration,RandD Spend)-Scatter plot
pairs(Startups)
plot(Startups)
# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Startups)
install.packages("psych")
library(psych)
pairs.panels(Startups)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Startups))


# The Linear Model of interest
model.startups<- lm(Profit ~ `R&D Spend`+`Marketing Spend`+Administration+state,data=Startups)
summary(model.startups)

# Prediction based on only R&D Spend
model.startups1<-lm(Profit~`R&D Spend`)
summary(model.startups1) # `R&D Spend` became significant

# Prediction based on only Administration
model.startups2<-lm(Profit~Administration)
summary(model.startups2) # Administration became significant

# Prediction based on 
model.startups3<-lm(Profit~`R&D Spend`+Administration)
summary(model.startups3) # Both became Insignificant

# So there exists a collinearity problem b/n volume and weight
### Scatter plot matrix along with Correlation Coefficients

library(psych)
pairs.panels(Startups)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.startups)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.startups,id.n=3) # index plots for infuence measures
influencePlot(model.startups,id.n=3) # A user friendly representation of the above

# Regression after deleting the 50th observation, which is influential observation
model.startups11<-lm(Profit ~ `R&D Spend`+`Marketing Spend`+Administration+state,data=Startups[-50,])
summary(model.startups11)

# Regression after deleting the 50th and 46th Observations
model.startups22<-lm(Profit ~ `R&D Spend`+`Marketing Spend`+Administration+state,data=Startups[-c(50,46),])
summary(model.startups22)

## Variance Inflation factor to check collinearity b/n variables 
vif(model.startups)
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.startups,id.n=2,id.cex=0.7)

## VIF and AV plot has given us an indication to delete "state" variable

## Final model
finalmodel<-lm(Profit ~`R&D Spend`+`Marketing Spend`+Administration)
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model.startups,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 
