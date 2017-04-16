getwd()
setwd("C:/Users/Bindu/SentiTwitterR")
getwd()

library(twitteR)
library(plyr)
library(ROAuth)
library(stringr)
library(ggplot2)
library(ggplot2)
library(RCurl)
library(wordcloud)
library(base64enc)
library(RColorBrewer)
library(e1071)


consumer_key <- "o528f9AJqrCuTdnY9MvdR2wRf"
consumer_secret <- "N45TcVtbqTDjahtpzXQmUHMVltehxDvfjhCW8A5zHb1KXGa2pK"
access_token <- "851486451598491650-BCanmtomQ48Olkm6QFBXM1rt4SK9Etg"
access_token_secret <- "V949w4FmfyEwBQYBxVhVK4DIVmWegFQsdc7qdyE92QsVf"

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)

cred = OAuthFactory$new(consumerKey=consumer_key, 
                         consumerSecret=consumer_secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")

tweets=searchTwitter("#modi",n=1000)


