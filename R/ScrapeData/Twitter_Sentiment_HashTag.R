if (!require(lubridate)){install.packages("lubridate")}
if (!require(coinmarketcapr)){install.packages("coinmarketcapr")}
if (!require(twitteR)){install.packages("twitteR")}
if (!require(dplyr)){install.packages("dplyr")}
if (!require(plyr)){install.packages("plyr")}
if (!require(stringr)){install.packages("stringr")}
if (!require(ROAuth)){install.packages("ROAuth")}
if (!require(base64enc)){install.packages("base64enc")}
if (!require(openssl)){install.packages("openssl")}
if (!require(httpuv)){install.packages("httpuv")}
if (!require(tm)){install.packages("tm")}
if (!require(syuzhet)){install.packages("syuzhet")}

consumerKey <- 'enteryourkey'
consumerSecret <- 'enteryoursecret'
accesstoken <- 'enteryourtoken'
accesssecret <- 'enteryouraccesssecret'

library(lubridate)
library(coinmarketcapr)
library(plyr)
library(dplyr)
library(stringr)
library(ROAuth)
library(httr)
library(twitteR)
library(openssl)
library(httpuv)
library(base64enc)
library(tm)
library(syuzhet)

Unaccent <- function(x) {
  x=tolower(x)
  x = gsub("@\\w+", "", x) 
  x = gsub("[[:punct:]]", " ", x)
  x = gsub("[ |\t]{2,}", " ", x) 
  x = gsub("^ ", " ", x) 
  x = gsub("http\\w+", " ", x) 
  x=tolower(x)
  x=gsub('_',' ',x,fixed=T)
  x
}

getTweets <- function (searchHashtag)
{
  searchHashtag <- paste(searchHashtag, "-filter:retweets", sep="")
  setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesssecret)
  tweetslist <- searchTwitter(searchHashtag, since=str(Sys.Date() -1), until = str(Sys.Date()), n = 500)#
  tweetsdf1Hr <- twListToDF(tweetslist)
  timeminus1hr <- Sys.time() - lubridate::hours(1)
  tweetsdf1Hr$NewDateTime <- strptime(tweetsdf1Hr$created, '%Y-%m-%d %H:%M:%S') 
  tweetsdffiltered <- with(tweetsdf1Hr, subset(tweetsdf1Hr, tweetsdf1Hr$NewDateTime > as.POSIXct(timeminus1hr)))
  return(tweetsdffiltered)
  
}

tweetsdf1Hr <- data.frame()
top100CryptoCC <- get_marketcap_ticker_all()
top5CC <- head(top100CryptoCC, 5)
dfCCSentiments1hr <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("CryptoCurrency", "PositiveSentimentValue", "Pctchange1Day"))

for(i in 1:5)
{
  CCHashCode = paste0("#",top5CC$symbol[i],sep="")
  tweetsdf1Hr <- getTweets(CCHashCode)
  
  tweetsdf1Hr$text=Unaccent(iconv(tweetsdf1Hr$text,from="UTF-8",to="ASCII//TRANSLIT"))
  tweetsdf1Hr$text=stripWhitespace(tweetsdf1Hr$text)
  words <- as.vector(tweetsdf1Hr$text)
  sent.value <- get_sentiment(words)
  category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
  
  SenimentsCount <- table(category_senti)
  positiveSentiment <- SenimentsCount[3] - SenimentsCount[1]
  dfCCSentiments1hr[nrow(dfCCSentiments) + 1,] = list(top5CC$name[i],positiveSentiment,top5CC$percent_change_1h[i])#
}
View(dfCCSentiments1hr)
