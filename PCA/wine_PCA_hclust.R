library(readr)
wine <- read_csv("E:/Data Science/Assignments/wine.csv")
View(wine)

normalized_data<-scale(wine[,2:14]) #excluding the X1 column before normalizing
View(normalized_data)

d <- dist(normalized_data, method = "euclidean") # distance matrix
d
# Model Building
fit <- hclust(d, method="complete")
fit$labels
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters
class(groups)
rect.hclust(fit, k=5, border="red")


membership<-as.matrix(groups)
table(membership)

final <- data.frame(wine, membership)
#final <- cbind(mydata, membership)
View(final)


#explore setcolorder for repositioning the columns in R
# Also install the package "data.table"
install.packages("data.table")
library(data.table)
setcolorder(final,c("membership"))
View(final)


write.csv(final, file="winehclust.csv")
setwd("E://Data Science//Assignments")
getwd()
