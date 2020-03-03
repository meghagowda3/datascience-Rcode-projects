
#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
library("recommenderlab")
library(caTools)

books_data <- read.csv("E://Data Science//Assignments//Recommendation system//books.csv")
View(books_data)
class(books_data)
#metadata about the variable
str(books_data)


#rating distribution
hist(books_data$ratings...3.)

#the datatype should be realRatingMatrix inorder to build recommendation engine
books_data_matrix <- as(books_data, 'realRatingMatrix')

#Popularity based 

books_recomm_model1 <- Recommender(books_data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(books_recomm_model1, books_data_matrix[413:414], n=10)
as(recommended_items1, "list")


## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

books_recomm_model2 <- Recommender(books_data_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(books_recomm_model2, books_data_matrix[413:414], n=10)
as(recommended_items2, "list")
