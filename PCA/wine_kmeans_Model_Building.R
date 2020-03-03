library(readr)
wine <- read_csv("wine.csv")
View(wine)
normalized_data<-scale(wine[,2:14])
View(normalized_data)

#elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:14) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:14, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")

# model Building
fit <- kmeans(normalized_data, 6) # 6 cluster solution
str(fit)
table(fit$cluster)
final2<- data.frame(wine, fit$cluster) # append cluster membership
View(final2)
library(data.table)
setcolorder(final2, neworder = c("fit.cluster"))
View(final2)
aggregate(wine[,2:14], by=list(fit$cluster), FUN=mean)
fit$size

km <- kmeans(final2,6) #kmeans clustering
str(km)
km$ifault

install.packages("animation")
library(animation)

km <- kmeans.ani(final2,6)
km$size
