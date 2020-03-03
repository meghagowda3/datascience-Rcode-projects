salary_data<-read.csv(file.choose(),stringsAsFactors = F)
class(salary_data)
str(salary_data)
salary_data$Salary<-factor(salary_data$Salary)
str(salary_data)
table(salary_data$Salary)

library(tm)

# Prepare corpuse for the text data 
salary_corpous<-Corpus(VectorSource(salary_data$Salary))

# Cleaning data (removing unwanted symbols)
corpus_clean<-tm_map(salary_corpous,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
# Do not run the plainTextDocument
# corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
as.character(corpus_clean)
# create a document-term sparse matrix
#corpus_clean<-Corpus(VectorSource(corpus_clean))
salary_dtm <- DocumentTermMatrix(corpus_clean) 
class(salary_dtm)
# if code at 25 shows any error run the code at line 24 first and proceed
as.character(salary_dtm)
# creating training and test datasets
salary_raw_train <- salary_data[1:25000, ]
salary_raw_test  <- salary_data[25001:30161, ]

salary_dtm_train <- salary_dtm[1:25000, ]
salary_dtm_test  <- salary_dtm[25001:30161, ]

salary_corpus_train <- salary_clean[1:25000]
salary_corpus_test  <- salary_clean[25001:30161]

# check that the proportion of spam is similar
prop.table(table(salary_raw_train$Salary))
prop.table(table(salary_raw_test$Salary))

# indicator features for frequent words
salary_dict<-findFreqTerms(salary_dtm_train, 5)

salary_train <- DocumentTermMatrix(salary_corpus_train, list(dictionary = salary_dict))
salary_test  <- DocumentTermMatrix(salary_corpus_test, list(dictionary = salary_dict))
salary_dict
inspect(salary_corpus_train[1:100])
list(salary_dict[1:100])
# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
salary_train <- apply(salary_train, MARGIN = 2, convert_counts)
salary_test  <- apply(salary_test, MARGIN = 2, convert_counts)
View(salary_train)
View(salary_test)
##  Training a model on the data ----
library(e1071)
salary_classifier <- naiveBayes(salary_train, salary_raw_train$Salary)
salary_classifier

##  Evaluating model performance ----
sms_test_pred <- predict(salary_classifier, salary_test)

library(gmodels)
CrossTable(salary_test_pred, salary_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

