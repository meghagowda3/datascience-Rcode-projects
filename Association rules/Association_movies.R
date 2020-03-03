library(arules)
library(arulesViz)
movies<-read.csv(file.choose())
View(movies)
class(movies)
movies_new<-movies[,-(1:5)]
View(movies_new)
movies_trans<-as(as.matrix(movies_new),"transactions")
inspect(movies_trans[1:10])
# If we inspect book_trans
# we should get transactions of items i.e.
# As we have 10 rows ..so we should get 10 transactions 
# Each row represents one transaction
# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm 
rules<-apriori(as.matrix(movies_new),parameter = list(support=0.002,confidence=0.7))

inspect(rules[1:10])
# Apriori algorithm 
plot(rules)


-----------------------------------------------------------------------------------------------------------
library(arules)
library(arulesViz)
movies<-read.csv(file.choose())
View(movies)
class(movies)
movies_new<-movies[,-(1:5)]
View(movies_new)
movies_trans<-as(as.matrix(movies_new),"transactions")
inspect(movies_trans[1:10])

rules<-apriori(as.matrix(movies_new),parameter = list(support=0.01,confidence=0.9))

inspect(rules[1:10])
# Apriori algorithm 
plot(rules)
