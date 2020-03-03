# Whenever we have data containing item names, then load that data using 
# read.transactions(file="path",format="basket",sep=",")
# use this to form association rules 
library(arules)
library(arulesViz)

groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)

groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
inspect(groceries_rules[1:10])
library(arulesViz)
plot(groceries_rules)
-----------------------------------------------------------------------------------------------------------------------------------------------------
  
library(arules)
library(arulesViz)

groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)

groceries_rules<-apriori(groceries,parameter = list(support = 0.001,confidence = 0.01,minlen=4))
inspect(groceries_rules[1:10])
library(arulesViz)
plot(groceries_rules)
--------------------------------------------------------------------------------------------------------------------------------------