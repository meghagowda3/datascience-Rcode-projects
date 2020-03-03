library(readr)
wine <- read_csv("wine.csv")
View(wine)

data <- wine[-1]
View(data) 
attach(data)
cor(data)
# Model Building#
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# Normalizing the data 
norm_clus<-scale(data) # Scale function is used to normalize data

dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
dist1
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,data) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1

write.csv(final1,file="wine.csv",row.names = F,col.names = F)
getwd()
