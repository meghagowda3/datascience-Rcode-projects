install.packages("twitteR")
library("twitteR")
install.packages("ROAuth")
library("ROAuth")
cred <- OAuthFactory$new(consumerKey='43hHJkZa4snEM8vbikYur2C0V', # Consumer Key (API Key)
                         consumerSecret='T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("43hHJkZa4snEM8vbikYur2C0V", # Consumer Key (API Key)
                    "T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM", #Consumer Secret (API Secret)
                    "529590041-AJNiB9OMFcaZuCqzoN99fH4pyfUN8uJ9EXDBwY5B",  # Access Token
                    "frz56qjS1TXT1jmBcIWEmXGFLXQu1X3gUswyvMEdCMOfj")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('narendramodi', n = 1000)

TweetsDF <- twListToDF(Tweets)
write.csv(TweetsDF, "Tweets.csv")

getwd()
