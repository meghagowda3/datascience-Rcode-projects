library(arules)
library(arulesViz)
book<-read.csv(file.choose())
View(book)
class(book)
book_trans<-as(as.matrix(book),"transactions")
inspect(book_trans[1:100])

rules<-apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.7)) #when support support=0.002,confidence=0.7

inspect(rules[1:100])
# Apriori algorithm 
plot(rules)

------------------------------------------------------------------------------------------------------------
library(arules)
library(arulesViz)
book<-read.csv(file.choose())
View(book)
class(book)
book_trans<-as(as.matrix(book),"transactions")
inspect(book_trans[1:100])

rules<-apriori(as.matrix(book),parameter = list(support=0.005,confidence=0.75)) #when support support=0.002,confidence=0.7

inspect(rules[1:100])
# Apriori algorithm 
plot(rules)

------------------------------------------------------------------------------------------------------------------library(arules)
library(arules)
library(arulesViz)
book<-read.csv(file.choose())
View(book)
class(book)
book_trans<-as(as.matrix(book),"transactions")
inspect(book_trans[1:100])

rules<-apriori(as.matrix(book),parameter = list(support=0.01,confidence=0.80)) #when support support=0.01,confidence=0.80

inspect(rules[1:100])
# Apriori algorithm 
plot(rules)
