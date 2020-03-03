library(readr)
crime_data <- read_csv("E:/Data Science/Assignments/Clustering/crime_data.csv")
View(crime_data)


normalized_data<-scale(crime_data[,2:5]) #excluding the X1 column before normalizing
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

final <- data.frame(crime_data, membership)
#final <- cbind(mydata, membership)
View(final)


#explore setcolorder for repositioning the columns in R
# Also install the package "data.table"
install.packages("data.table")
library(data.table)
setcolorder(final,c("membership"))
View(final)


write.csv(final, file="final11.csv")
setwd("E:/Data Science/Assignments/Clustering")
getwd()
