modi_text = sapply(modi.tweets, function(x) x$getText()) #sapply returns a vector 
df <- do.call("rbind", lapply(modi.tweets, as.data.frame)) #lapply returns a list
modi_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
str(modi_text) #gives the summary/internal structure of an R object

library(tm) #tm: text mining
modi_corpus <- Corpus(VectorSource(modi_text)) #corpus is a collection of text documents
modi_corpus
inspect(modi_corpus[1])

#clean text
library(wordcloud)
modi_clean <- tm_map(modi_corpus, removePunctuation)
modi_clean <- tm_map(modi_clean, removeWords, stopwords("english"))
modi_clean <- tm_map(modi_clean, removeNumbers)
modi_clean <- tm_map(modi_clean, stripWhitespace)
wordcloud(modi_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(3.5,1))
