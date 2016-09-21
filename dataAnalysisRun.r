# Colin Robertson
# AI-Tweets Project
# 
# Winter 2016
#***********************************************************************************
#library(twitteR)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(SnowballC)

setwd("X")
options(stringsAsFactors = FALSE)
x <- read.csv("../../../../nsercCREATE/Scraping/tweets/test2.csv")
tweets <- xA1$text 

# Here we pre-process the data in some standard ways. I'll post-define each step
tweets <- iconv(tweets, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters
tweets <- tolower(tweets)  # Make everything consistently lower case
tweets <- gsub("rt", " ", tweets)  # Remove the "RT" (retweet) so duplicates are duplicates
tweets <- gsub("@\\w+", " ", tweets)  # Remove user names (all proper names if you're wise!)
tweets <- gsub("http.+ |http.+$", " ", tweets)  # Remove links
tweets <- gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets <- gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets <- gsub("amp", " ", tweets)  # "&" is "&amp" in HTML, so after punctuation removed ...
tweets <- gsub("^ ", "", tweets)  # Leading blanks
tweets <- gsub(" $", "", tweets)  # Lagging blanks
tweets <- gsub(" +", " ", tweets) # General spaces (should just do all whitespaces no?)
tweets <- unique(tweets)  # Now get rid of duplicates!


# Convert to tm corpus and use its API for some additional fun
corpus <- Corpus(VectorSource(tweets))  # Create corpus object
# Remove English stop words. This could be greatly expanded! # Don't forget the mc.cores thing
corpus <- tm_map(corpus, removeWords, stopwords("en"), mc.cores=1)  
# Remove numbers. This could have been done earlier, of course.
corpus <- tm_map(corpus, removeNumbers, mc.cores=1)
# Stem the words. Google if you don't understand
corpus <- tm_map(corpus, stemDocument, mc.cores=1)
# Remove the stems associated with our search terms!
corpus <- tm_map(corpus, removeWords, c("bird", "flu", "avian", "influenza", "poultry"), mc.cores=1)

#pal <- brewer.pal(8, "Dark2")
#wordcloud(corpus, min.freq=2, max.words = 150, random.order = TRUE, col = pal)

# Now for Topic Modeling

# Get the lengths and make sure we only create a DTM for tweets with
# some actual content
doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])
# model <- LDA(dtm, 10)  # Go ahead and test a simple model if you want


# Now for some topics
SEED = sample(1:1000000, 1)  # Pick a random seed for replication


harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}
k = 15 #31
burnin = 1000
iter = 1000
keep = 50
maxK = 34*4 #k is 31 for AI1, and 15 for AI2 (i think for 2)

# generate numerous topic models with different numbers of topics
sequ <- seq(2, maxK, 1) # in this case a sequence of numbers from 1 to 50, by ones.
fitted_many <- lapply(sequ, function(k) LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))
# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
# inspect
dx <- data.frame(sequ, hm_many)
png("Figxx_TopicModelsA1.png", width=12, height=8, units="in", res=300)
   ggplot(dx, aes(x=sequ, y=hm_many)) + geom_line() + theme_bw() + labs(x='Number of Topics', y = 'Log Likelihood')
dev.off()
# compute optimum number of topics
sequ[which.max(hm_many)]
##
k = sequ[which.max(hm_many)]
Gibbs     = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100,    iter = 1000))
terms(Gibbs, 20)

top <- topics(Gibbs)
top.df <- as.data.frame(top)
top.df$tx <- tweets[which(!doc.lengths == 0)]

png("Figxx_TopicModelsA2_xx.png", width=12, height=8, units="in", res=300)
par(mfrow=c(4,4))
for(i in 1:15) {
  corpus <- Corpus(VectorSource(top.df$tx[which(top.df$top == i)]))  # Create corpus object
  corpus <- tm_map(corpus, removeWords, stopwords("en"), mc.cores=1)  
  corpus <- tm_map(corpus, removeWords, c("bird", "flu", "avian", "influenza", "poultry"), mc.cores=1)
  pal <- brewer.pal(8, "Dark2")
  wordcloud(corpus, min.freq=2, max.words = 150, random.order = TRUE, col = pal)
}
dev.off()

sapply(models[2:4], slot, "alpha")
sapply(models, function(x) mean(apply(posterior(x)$topics,1, function(z) - sum(z * log(z)))))

par(mfrow=c(1,4))
hist(sapply(models[1], slot, "gamma"), xlab="P(x)", main="CTM")
hist(sapply(models[2], slot, "gamma"), xlab="P(x)", main="VEM")
hist(sapply(models[3], slot, "gamma"), xlab="P(x)", main="VEM_Fixed")
hist(sapply(models[4], slot, "gamma"), xlab="P(x)", main="Gibbs")

########################################################
#section 1 - scan statistic or tscount model 
########################################################
library(plyr)
library(ggplot2)
library(zoo)

x$D <- as.POSIXct(strptime(as.character(x$created_at), "%a %b %d %H:%M:%S %z %Y"))
x$day <- as.Date(x$D)
#x$textC <- removeURL(x$text)
#x <- x[!duplicated(x$textC), ]
daily <- ddply(x, .(day), summarise, numTweets = length(day)) #summarize by mean
#figure 1****************************************************************************************************
daily$dt=strptime(daily$day, "%Y-%m-%d")
temp.zoo<-zoo(daily$numTweets,daily$dt) #Make zoo object of data
#Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
m.av<-rollmean(temp.zoo, 7,fill = list(NA, NULL, NA))
daily$numTweets7=coredata(m.av) #Add calculated moving averages to existing data frame
m.av<-rollmean(temp.zoo, 3,fill = list(NA, NULL, NA)) #Add calculated moving averages to existing data frame
daily$numTweets3=coredata(m.av) 
#Add additional line for moving average in red
require(grid)
png("Fig1_timeSeries.png", width=12, height=8, units="in", res=300)
   ggplot(daily, aes(dt)) + geom_line(aes(y=numTweets, colour="Daily"), size=1) + stat_smooth(aes(y=numTweets)) + geom_line(aes(y=numTweets3, colour="3 Day Moving \nAverage"), size=1) + geom_line(aes(y=numTweets7, color="7 Day Moving \nAverage"), size=1) + xlab("Day") + ylab("Daily AI-related Tweets") + scale_colour_manual("Lines", values=c("Daily"="black", "3 Day Moving \nAverage"="red", "7 Day Moving \nAverage"="grey")) + theme_bw() + theme(legend.title = element_blank(), legend.key.size = unit(1.5, "cm")) 
dev.off()
png("Fig1_timeSeries_noSmooth.png", width=12, height=8, units="in", res=300)
ggplot(daily, aes(dt)) + geom_line(aes(y=numTweets, colour="Daily"), size=1) + geom_line(aes(y=numTweets3, colour="3 Day Moving \nAverage"), size=1) + geom_line(aes(y=numTweets7, color="7 Day Moving \nAverage"), size=1) + xlab("Day") + ylab("Daily AI-related Tweets") + scale_colour_manual("Lines", values=c("Daily"="black", "3 Day Moving \nAverage"="red", "7 Day Moving \nAverage"="grey")) + theme_bw() + theme(legend.title = element_blank(), legend.key.size = unit(1.5, "cm")) 
dev.off()
png("Fig1x_timeSeries_noSmooth_OIE.png", width=12, height=8, units="in", res=300)
ggplot(daily, aes(dt)) + geom_line(aes(y=numTweets, colour="Daily"), size=1) + geom_line(aes(y=numTweets3, colour="3 Day Moving \nAverage"), size=1) + geom_line(aes(y=numTweets7, color="7 Day Moving \nAverage"), size=1) + geom_point(data=oieS, aes(x=D, y=rep(2000, 171))) + xlab("Day") + ylab("Daily AI-related Tweets") + scale_colour_manual("Lines", values=c("Daily"="black", "3 Day Moving \nAverage"="red", "7 Day Moving \nAverage"="grey")) + theme_bw() + theme(legend.title = element_blank(), legend.key.size = unit(1.5, "cm")) 
dev.off()
#figure 1****************************************************************************************************
oie <- read.csv("OIEOutbreaks.csv")
library(stringr)
oie$Yr <- as.numeric(paste('20', str_sub(oie$Day, -2), sep=""))
oie$Mo <- as.numeric(str_sub(str_extract(oie$Day, "/.*/"), 2, -2))
oie$D <- as.numeric(str_sub(oie$Day, 1, str_locate(oie$Day, "/")-1))[1:514] #hack
oie$day <- paste(oie$D, oie$Mo, oie$Yr, sep="/")
oie$D <- strptime(as.character(oie$day), "%d/%m/%Y")
oie$Day <- as.Date(oie$D)
oieS <- subset(oie, Mo >= 10 & !Virus.Type == "" |  Yr == 2016 & !Virus.Type == "" )
oieS$ReportType[which(str_trim(oieS$ReportType) == "Folow-up")] <- "Follow-up" #spelling mistake
oieS$Mo <- months(oieS$D)
oieM <- ddply(oieS[,1:9], c("Mo", "Virus.Type"), summarise, numReports = length(Link)) #summarize by mean
oieM$Month <- factor(oieM$Mo, levels=c("October", "November", "December", "January", "February", "March"))
#png("Fig2x_timeSeries_OIE.png", width=12, height=8, units="in", res=300)
#  ggplot(oieS, aes(D)) + geom_point(aes(y=factor(trim(ReportType)), colour=factor(Virus.Type)), size=5) + theme_bw() + labs(x="Date of Report", y="Report Type") + scale_colour_grey(name = "Virus Type") 
#dev.off()
png("Fig2x_timeReports_OIE.png", width=12, height=8, units="in", res=300)
  ggplot(oieM, aes(x = Month, y = numReports, fill=Virus.Type)) + geom_bar(stat = "identity") + theme_bw() + labs(y = "Number of OIE Reports") + guides(fill=guide_legend(title="Virus Type"))
dev.off()
x$MM <- months(x$day)
oiM <- ddply(oieS[,1:9], c("Mo"), summarise, numReports = length(Link)) #summarize by mean
TwM <- ddply(x, c("MM"), summarise, numReports = length(day)) #summarize by mean

options(scipen = 999)
library(tscount)
err_ts = ts(as.vector(daily$numTweets), frequency = 1)
err_fit = tsglm(err_ts, model = list(past_obs = 1:5, past_mean = NULL, external = FALSE), distr = 'nbinom')
err_fitP = tsglm(err_ts, model = list(past_obs = 1:5, past_mean = NULL, external = FALSE), distr = 'poisson')

pit(err_fit)
scoring(err_fit)
#check for autocorrelation structure
png("Figxx_acf_TW.png", width=8, height=5, units="in", res=300)
  acf(err_ts, xlab="Lag (Days)", main="")
dev.off()
#fit the negative binomial model and plot observed vs expected
png("Figxx_ts_TW.png", width=8, height=5, units="in", res=300)
  ts.plot(err_ts, gpars=list(col=c('black'), lwd=c(2)), ylab='AI-Related Tweets', xlab='Day')
  abline(h=mean(err_ts) + (2 * (sd(err_ts)/sqrt(147))), col="blue", lwd=2)
  points(which(err_ts > (mean(err_ts) + (2 * (sd(err_ts)/sqrt(147))))), as.numeric(err_ts[which(err_ts > (mean(err_ts) + (2 * (sd(err_ts)/sqrt(147)))))]), pch=20)
dev.off()
#use model estimates to find alarms using the cusum algorithm
library(surveillance)
disProgObj <- create.disProg(week=1:147, observed = daily$numTweets, state=rep(0,147))
find.kh(ARLa = 365, ARLr = 7, sided = "one", method = "BFGS", verbose=FALSE)
res <- algo.cusum(disProgObj, control = list(range = 1:147, trans="pearsonNegBin", m=fitted(err_fit), alpha = 0.433, k=0.56937, h=3.668))
png("Figxx_cusum_OIE.png", width=8, height=5, units="in", res=300)
  plot(res, xlab="Day", xaxis.years=FALSE, ylab='AI-Related Tweets', main='')
dev.off()

png("Figxx_cusum.png", width=8, height=5, units="in", res=300)
  ts.plot(err_ts, res$upperbound, gpars=list(col=c('black', 'blue'), lwd=c(2,2)), ylab='AI-Related Tweets', xlab='Day')
  points(which(res$alarm == 1), as.numeric(err_ts[which(res$alarm == 1)]), pch=20)
dev.off()


#simple alarms
xA1 <- x[x$day %in% daily[which(err_ts > (mean(err_ts) + (2 * (sd(err_ts)/sqrt(147))))), 1], ]

#model alarms
#daily[which(res$alarm==1), ]
xA2 <- x[x$day %in% daily[which(res$alarm==1), 1], ]


#########^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^########################################
#########^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^########################################
#########  Comparing OIE and Tweets                  ########################################
#########                                            ########################################
#########^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^########################################
#########^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^########################################

oieF <- read.csv("OIEOutbreaks_final2.csv") #read.csv("OIEOutbreaks_final.csv")
oieF$Yr <- as.numeric(paste('20', str_sub(oieF$ReportDay, -2), sep=""))
oieF$Mo <- as.numeric(str_sub(str_extract(oieF$ReportDay, "/.*/"), 2, -2))
oieF$D <- as.numeric(str_sub(oieF$ReportDay, 1, str_locate(oieF$ReportDay, "/")-1))[1:71] #hack
oieF$newDate <- paste(oieF$D, oieF$Mo, oieF$Yr, sep="/")
oieF$newDate2 <- strptime(as.character(oieF$newDate), "%d/%m/%Y")
oieF$Day <- as.Date(oieF$newDate2)

oieF$Month <- months(oieF$Day)

oieFs <- subset(oieF, Mo >= 10  |  Yr == 2016  )
oieFs$startDate <- strptime(as.character(oieFs$Start.of.Outbreak), "%d/%m/%Y")
oieFs$SDay <- as.Date(oieFs$startDate)


ggplot(oieFs, aes(startDate, reorder(Location, as.POSIXct(startDate)), colour = Location)) + geom_errorbarh(aes(xmin = startDate,xmax = newDate2, height = 0.2)) + scale_colour_discrete(guide = FALSE) + labs(x="Start of Outbreak to the Report Date", y = "Location of Outbreak")

dailyA1 <- ddply(xA1, .(day), summarise, numTweets = length(day)) #summarize by mean
dailyA2 <- ddply(xA2, .(day), summarise, numTweets = length(day)) #summarize by mean

png("Figxx_AI_offical2.png", width=9, height=6, units="in", res=300)
  ggplot(oieFs, aes(SDay, reorder(Location, as.POSIXct(SDay)), colour = Location)) + geom_vline(data=dailyA1,aes(xintercept=as.numeric(day)), colour='green') + geom_vline(data=dailyA2,aes(xintercept=as.numeric(day)), colour='blue')  + geom_errorbarh(aes(xmin = SDay,xmax = Day, height = 0.2)) + scale_colour_discrete(guide = FALSE) + labs(x="Start of Outbreak to the Report Date", y = "Location of Outbreak")  + scale_x_date()
dev.off()


write.csv(xA2, "XA2.csv")

xA1$tweets <- iconv(xA1$text, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters
xA1$tweets <- tolower(xA1$tweets)  # Make everything consistently lower case
xA1$tweets <- gsub("rt", " ", xA1$tweets)  # Remove the "RT" (retweet) so duplicates are duplicates
xA1$tweets <- gsub("@\\w+", " ", xA1$tweets)  # Remove user names (all proper names if you're wise!)
xA1$tweets <- gsub("http.+ |http.+$", " ", xA1$tweets)  # Remove links
xA1$tweets <- gsub("[[:punct:]]", " ", xA1$tweets)  # Remove punctuation
xA1$tweets <- gsub("[ |\t]{2,}", " ", xA1$tweets)  # Remove tabs
xA1$tweets <- gsub("amp", " ", xA1$tweets)  # "&" is "&amp" in HTML, so after punctuation removed ...
xA1$tweets <- gsub("^ ", "", xA1$tweets)  # Leading blanks
xA1$tweets <- gsub(" $", "", xA1$tweets)  # Lagging blanks
xA1$tweets <- gsub(" +", " ", xA1$tweets) # General spaces (should just do all whitespaces no?)
write.csv(xA1[!duplicated(xA1$tweets),], "XA1.csv")
xA1.d <- xA1[!duplicated(xA1$tweets),]

locations <- data.frame(countA1 = sort(table(xA1$location), TRUE)[1:25])
locations$LocationA1 <- row.names(locations)
locations2 <- data.frame(countA2 = sort(table(xA2$location), TRUE)[1:25])
locations2$Location <- row.names(locations2)
locations$countA2 <- locations2$countA2
locations$LocationA2 <- locations2$Location
rm(locations2)
write.csv(locations, 'locations.csv')
dailyA1$Method = 'Static'
dailyA2$Method = 'Dynamic'
dailyA <- rbind(dailyA1, dailyA2)
png("Figxx_OIE_TW2.png", width=10, height=8, units="in", res=300)
  ggplot(dailyA, aes(x = day, y = numTweets)) + geom_point(aes(colour=Method), size=3) + geom_vline(data=oieFs,aes(xintercept=as.numeric(SDay)), colour='green') +  scale_x_date(limits = as.Date(c('2015-10-1','2016-04-01'))) + geom_text(data = oieFs, aes(x=SDay, y=rep(1500, 35), label=Location), angle=90) + labs(x = 'Date', y='AI-Related Tweets ')
dev.off()


xA2$tweets <- iconv(xA2$text, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters
xA2$tweets <- tolower(xA2$tweets)  # Make everything consistently lower case
xA2$tweets <- gsub("rt", " ", xA2$tweets)  # Remove the "RT" (retweet) so duplicates are duplicates
xA2$tweets <- gsub("@\\w+", " ", xA2$tweets)  # Remove user names (all proper names if you're wise!)
xA2$tweets <- gsub("http.+ |http.+$", " ", xA2$tweets)  # Remove links
xA2$tweets <- gsub("[[:punct:]]", " ", xA2$tweets)  # Remove punctuation
xA2$tweets <- gsub("[ |\t]{2,}", " ", xA2$tweets)  # Remove tabs
xA2$tweets <- gsub("amp", " ", xA2$tweets)  # "&" is "&amp" in HTML, so after punctuation removed ...
xA2$tweets <- gsub("^ ", "", xA2$tweets)  # Leading blanks
xA2$tweets <- gsub(" $", "", xA2$tweets)  # Lagging blanks
xA2$tweets <- gsub(" +", " ", xA2$tweets) # General spaces (should just do all whitespaces no?)
write.csv(xA1[!duplicated(xA2$tweets),], "XA2.csv")
xA2.d <- xA2[!duplicated(xA2$tweets),]

xA2.d.1 <- subset(xA2.d, day == '2015-10-26')
write.csv(xA2.d.1, 'dynclust1.csv')




#paper revisions
library(stringr)
x <- read.csv("../../../../nsercCREATE/Scraping/tweets/test2.csv")
x$ct1 <- str_count(x$text, ignore.case(c('bird flu')))
x$ct2 <- str_count(x$text, ignore.case(c('avian influenza')))
x$ct3 <- str_count(x$text, ignore.case(c('avian flu')))
x$ct4 <- str_count(x$text, ignore.case(c('poultry disease')))
x$ct <- x$ct1 + x$ct2 + x$ct3 + x$ct4

