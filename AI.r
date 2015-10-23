setwd("C:/Users/crobertson/Dropbox/nsercCREATE/Scraping/tweets/outputs/AITweets")
x <- read.csv("test2_clean.csv")
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
library(plyr)
library(ggplot2)
library(XML)
library(tm)
library(wordcloud)
library(RColorBrewer)

x$D <- as.POSIXct(strptime(as.character(x$created_at), "%a %b %d %H:%M:%S %z %Y"))
x$day <- as.Date(x$D)
x$textC <- removeURL(x$text)
x <- x[!duplicated(x$textC), ]
daily <- ddply(x, .(day), summarise, numTweets = length(day)) #summarize by mean

png("FULL_dailyTweetsAI.png", width=1280,height=800)
  ggplot(daily, aes(day, numTweets)) + geom_line() + xlab("") + ylab("Daily AI-related Tweets")
dev.off()

recent <- subset(daily, day >= max(daily$day)-14)

png("Recent_dailyTweetsAI.png", width=1280,height=800)
  ggplot(recent, aes(day, numTweets)) + geom_line() + xlab("") + ylab("Daily AI-related Tweets - last 14 days")
dev.off()




recent14 <- subset(x, day >= max(x$day)-14)


ap.corpus <- Corpus(DataframeSource(data.frame(recent14$text)))
ap.corpus <- tm_map(ap.corpus, content_transformer(tolower))
ap.corpus <- tm_map(ap.corpus, removeWords, stopwords("english"))
ap.corpus <- tm_map(ap.corpus, removeWords, c("bird", "flu", "avian", "influenza", "poultry"))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, PlainTextDocument)
ap.corpus <- tm_map(ap.corpus, stemDocument)

ap.corpus <- Corpus(VectorSource(ap.corpus))
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
head(ap.d)
pal2 <- brewer.pal(8,"Dark2")
png("wordCloud_14day.png", width=8,height=6, units="in", res=300)
    wordcloud(ap.d$word,ap.d$freq, scale=c(6,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()


