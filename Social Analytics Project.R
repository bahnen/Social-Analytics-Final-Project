#################################################################################
#################################################################################
#################################################################################
###REDDIT API###
#install and load reddit api package
install.packages("RedditExtractoR")
library(RedditExtractoR)

#search for reddit threat url containing certain keywords
urlscc <- find_thread_urls(keywords = "supply chain crisis", period = "month")
urlsc <- find_thread_urls(keywords = "supply chain", period = "month")
urlspc <- find_thread_urls(keywords = "supply crisis", period = "month")
urlshc <- find_thread_urls(keywords = "shipping crisis", period = "month")

#retrieve comments found in all threads - takes a while to run.
contents1 <- get_thread_content(urlscc$url)
contents2 <- get_thread_content(urlsc$url)
contents3 <- get_thread_content(urlspc$url)
contents4 <- get_thread_content(urlshc$url)


#the functions above return as a list data type. these will subset the comments from everything else
title1 <- ((contents1[["threads"]]$title))
title2 <- ((contents2[["threads"]]$title))
title3 <- ((contents3[["threads"]]$title))
title4 <- ((contents4[["threads"]]$title))

comments1 <- ((contents1[["comments"]]$comment))
comments2 <- ((contents2[["comments"]]$comment))
comments3 <- ((contents3[["comments"]]$comment))
comments4 <- ((contents4[["comments"]]$comment))

author1 <- ((contents1[["threads"]]$author))
author2 <- ((contents2[["threads"]]$author))
author3 <- ((contents3[["threads"]]$author))
author4 <- ((contents4[["threads"]]$author))

date1 <- ((contents1[["threads"]]$date))
date2 <- ((contents2[["threads"]]$date))
date3 <- ((contents3[["threads"]]$date))
date4 <- ((contents4[["threads"]]$date))

upvote1 <- ((contents1[["threads"]]$upvotes))
upvote2 <- ((contents2[["threads"]]$upvotes))
upvote3 <- ((contents3[["threads"]]$upvotes))
upvote4 <- ((contents4[["threads"]]$upvotes))

downvote1 <- ((contents1[["comments"]]$downvotes))
downvote2 <- ((contents2[["comments"]]$downvotes))
downvote3 <- ((contents3[["comments"]]$downvotes))
downvote4 <- ((contents4[["comments"]]$downvotes))


#combine all rows of comments together. this is the final data frame
comments_combined <- as.data.frame(c(comments1, comments2, comments3, comments4))
author_combined <- as.data.frame(c(author1, author2, author3, author4))
date_combined <- as.data.frame(c(date1, date2, date3, date4))
upvote_combined <- as.data.frame(c(upvote1, upvote2, upvote3, upvote4))
title_combined <- as.data.frame(c(title1, title2, title3, title4))

final_df <- cbind(title_combined, author_combined, date_combined, upvote_combined)


#write to csv
write.csv(final_df, "post_df.csv")

#################################################################################
#################################################################################
#################################################################################
###REDDIT POST ANALYSIS###
library(tm)
library(SnowballC)
library(RColorBrewer)
library(syuzhet)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(wordcloud)
library(Rcpp)
library(printr)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(data.table)
library(openxlsx)
library(writexl)
#update.packages()

# Read Data In and manipulate data
data = read.csv("post_df.csv", header = T)
df = read.csv("post_df.csv", header = T)
data$Date = as.Date(data$Date, format = "%Y-%m-%d")
data$day = day(data$Date)
x <- data

#put data into corpus and DTM for analysis
DataCorp = VCorpus(VectorSource(x$Title))
dt = DocumentTermMatrix(DataCorp)

#Find terms
CorpTerms = colSums(as.matrix(dt))
CorpTermsSorted = sort(CorpTerms, decreasing = T)
Top100 = head(CorpTermsSorted, 100)
View(as.matrix(Top100))

#top 5 terms and plot
top5 <- Top100[c(3,4,5,10,13)]

barplot(top5, main="Top 5 Most Frequent Words in Reddit Titles",
        xlab="Frequent Words", ylab = "Frequency",names.arg = c('supply', 'chain', 'crisis', 'presale', 'cap'), width = 2, col = 'steelblue')



# Polarity of Reddit posts --- Completed
RedditPolarity = data.frame(Post = data$Title, Account = data$User, data$Date, syuzhet = get_sentiment(as.character(data$Title), method = "syuzhet")) 
RedditPolarity$Polarity = ifelse(RedditPolarity$syuzhet < 0, 'Negative', ifelse(RedditPolarity$syuzhet == 0, 'Neutral', ifelse(RedditPolarity$syuzhet > 0, 'Positive', 'Negative')))


#positive vs negative vs neutral post sentiment
library(plyr)
f <- ddply(RedditPolarity, .(Polarity), nrow)
F <- f[order(f$V1, decreasing = TRUE),]

barplot(F$V1, main="Reddit Post Sentiment: Titles Only",
        xlab="# of instances", ylab = "Sentiment",names.arg = c('Negative', 'Positive', 'Neutral'), width = 2, col = 'steelblue')




#Sentiment analysis
RedditEmotion = data.frame(data$Title, data$day,get_nrc_sentiment(as.character(data$Title)))
colnames(RedditEmotion) = c('Comment', 'Day', 'Anger', 'Anticipation', 'Disgust', 'Fear', 'Joy', 'Sadness', 'Surprise', 'Trust', 'Negative', 'Positive')
head(RedditEmotion)

for (i in 1:nrow(RedditEmotion))
{ 
  RedditEmotion$Emotion[i]=ifelse((table(as.numeric(RedditEmotion[i,3:12]))
                                   [names(table(as.numeric(RedditEmotion[i, 3:12])))==max(RedditEmotion[i, 3:12])]) ==1,
                                  'FindEmotion', '')
  
  for (column in 3:12)
  {
    RedditEmotion$Emotion[i]=ifelse(RedditEmotion$Emotion[i]=='FindEmotion',
                                    ifelse(RedditEmotion[i, column]==max(RedditEmotion[i, 3:12]),
                                           colnames(RedditEmotion[column]),RedditEmotion$Emotion[i]), RedditEmotion$Emotion[i])
  }
}

RedditCompareEmo=c(anger=paste(subset(RedditEmotion, Emotion =='Anger')$Comment,
                               sept='\n', collapse = ' '), paste(subset(RedditEmotion, Emotion=='Disgust')$Comment,
                                                                 sept='\n', collapse = ' ') )
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Fear')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Sadness')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Surprise')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Joy')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Anticipation')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Trust')$Comment,
                                           sept='\n', collapse = ' '))

#pre-processing function
preprocessing = function(Corp) {
  StripString=content_transformer(function(x, pattern) gsub(pattern, ' ', x))
  SearchReplace=content_transformer(function(x, pattern1, pattern2) gsub(pattern1, pattern2, x))
  latin2Ascii=content_transformer(function(x) iconv(x, 'latin1', 'ASCII', sub=' '))
  
  Corp=tm_map(Corp, latin2Ascii)  
  Corp=tm_map(Corp, content_transformer(tolower))
  Corp=tm_map(Corp, removeWords, stopwords("english"))
  Corp=tm_map(Corp, removePunctuation)
  Corp=tm_map(Corp, removeNumbers)
  Corp=tm_map(Corp, stripWhitespace)
  
  Corp=tm_map(Corp, StripString, 'http[[:alnum:]]*')
  Corp=tm_map(Corp, StripString, '[\r\n]')
  Corp=tm_map(Corp, StripString, '[\t]')
  
  Corp=tm_map(Corp, stemDocument)
  return(Corp)
}

#construct TDM
RedditCompareCorp = Corpus(VectorSource(RedditCompareEmo))
RedditCompareCorp = preprocessing(RedditCompareCorp)
RedditCompareTDM = TermDocumentMatrix(RedditCompareCorp)
RedditCompareTDM = removeSparseTerms(RedditCompareTDM, 0.99)
colnames(RedditCompareTDM) = c('Anger', 'Disgust', 'Fear', 'Sadness', 'Surprise', 'Joy', 'Anticipation', 'Trust')
RedditCompareTDM = as.matrix(RedditCompareTDM)
cloud = comparison.cloud(RedditCompareTDM, max.words = 2000, vfont=c('serif', 'plain'), random.order = F, rot.per = .25, scale = c(2.5, .5), title.size = 1.5)


# Daily for loop
emotion_df <- RedditEmotion

comments_per_day <- data.frame()
day_emotion_df <- data.frame()



# % of total posts
emotions <- c('Anger', 'Anticipation', 'Disgust', 'Fear', 'Joy', 'Sadness', 'Surprise', 'Trust', 'Negative', 'Positive', 'Neutral')
for(day in 1:30){
  day_subset <- subset(emotion_df, emotion_df$Day == day)
  daily_total <- nrow(day_subset)
  comments_per_day <- rbind(comments_per_day, cbind(day, daily_total))
  
  day_output <- data.frame('day' = day)
  for(emotion in emotions){
    total <- nrow(subset(day_subset, day_subset$Emotion == emotion))
    percentage <- round(total/daily_total,2)
    day_output <- cbind(day_output, percentage)
    mat <- names(day_output) == 'percentage'
    names(day_output)[mat] <- emotion
  }
  day_emotion_df <- rbind(day_emotion_df, day_output)
}

#subsetting data frame 
posts_per_day <- comments_per_day[31:60,]



#Comments per Day
ggplot(posts_per_day, aes(x=day, y = daily_total)) + 
  #geom_bar(aes(y = daily_total), color = "darkred") +
  geom_bar(position = "stack", stat = "identity", fill = 'steelblue') +
  ggtitle("Daily Reddit Posts") +
  xlab("Day") + ylab("Total Posts") 

#Sentiment by Day Plot
ggplot(day_emotion_df, aes(x=day)) + 
  #geom_line(aes(y = Positive), color = "darkred") +
  #geom_line(aes(y = Negative), color = "steelblue") +
  geom_line(aes(y = Fear), color = "green") +
  geom_line(aes(y = Anger), color = "blue") +
  geom_line(aes(y = Sadness), color = "red") +
  geom_line(aes(y = Disgust), color = "orange") +
  ggtitle("Daily Reddit Posts with Fear, Anger, Sadness, and Disgust") +
  xlab("Day") + ylab("Total Posts")



#################################################################################
#################################################################################
#################################################################################
###REDDIT COMMENT ANALYSIS###
library(tm)
library(SnowballC)
library(RColorBrewer)
library(syuzhet)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(wordcloud)
library(Rcpp)
library(printr)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(data.table)
library(openxlsx)
library(writexl)
#update.packages()

# Read Data In and manipulate data
#df = read.csv("reddit_df.csv", header = T)
df <- read_xlsx("reddit_df.xlsx", sheet = "reddit_df")
data = df
data$date = as.Date(data$date, format = "%m-%d-%Y")
data$day = day(data$date)

# NOT WORKING

#x = read.csv("reddit_df.csv", header = T)
x <- data

DataCorp = VCorpus(VectorSource(x$comment))

dt = DocumentTermMatrix(DataCorp)

CorpTerms = colSums(as.matrix(dt))
CorpTermsSorted = sort(CorpTerms, decreasing = T)
Top100 = head(CorpTermsSorted, 100)
View(as.matrix(Top100))

# Polarity of Reddit posts --- Completed
#data = read.csv("reddit_df.csv", header = T)
#data$date = as.Date(data$date, format = "%Y-%m-%d")
#data$day = day(data$date)
RedditPolarity = data.frame(Post = data$comment, Account = data$author, data$day, syuzhet = get_sentiment(as.character(data$comment), method = "syuzhet")) 
RedditPolarity$Polarity = ifelse(RedditPolarity$syuzhet < 0, 'Negative', ifelse(RedditPolarity$syuzhet == 0, 'Neutral', ifelse(RedditPolarity$syuzhet > 0, 'Positive', 'Negative')))

# Top 10 Accounts in mean Polarity --- Completed
top10 = RedditPolarity %>%
  group_by(Account) %>%
  summarize(MeanSentiment = mean(syuzhet))
top10 = top10[order(-top10$MeanSentiment),]
head(top10, 10)

# Bottom 10 Accounts in mean polarity --- Completed
bot10 = RedditPolarity %>%
  group_by(Account) %>%
  summarize(MeanSentiment = mean(syuzhet))
bot10 = bot10[order(bot10$MeanSentiment),]
head(bot10, 10)

#Word cloud of emotion --- Completed
#RedditEmotion_1 <- RedditEmotion
data_1 <- data
data <- subset(data, data$day >= 11 & data$day <= 20)
RedditEmotion = data.frame(data$comment, data$day , get_nrc_sentiment(as.character(data$comment)))
colnames(RedditEmotion) = c('Comment', 'Day', 'Anger', 'Anticipation', 'Disgust', 'Fear', 'Joy', 'Sadness', 'Surprise', 'Trust', 'Negative', 'Positive')
head(RedditEmotion)

for (i in 1:nrow(RedditEmotion))
{ 
  RedditEmotion$Emotion[i]=ifelse((table(as.numeric(RedditEmotion[i,3:12]))
                                   [names(table(as.numeric(RedditEmotion[i, 3:12])))==max(RedditEmotion[i, 3:12])]) ==1,
                                  'FindEmotion', '')
  
  for (column in 3:12)
  {
    RedditEmotion$Emotion[i]=ifelse(RedditEmotion$Emotion[i]=='FindEmotion',
                                    ifelse(RedditEmotion[i, column]==max(RedditEmotion[i, 3:12]),
                                           colnames(RedditEmotion[column]),RedditEmotion$Emotion[i]), RedditEmotion$Emotion[i])
  }
}

RedditCompareEmo=c(anger=paste(subset(RedditEmotion, Emotion =='Anger')$Comment,
                               sept='\n', collapse = ' '), paste(subset(RedditEmotion, Emotion=='Disgust')$Comment,
                                                                 sept='\n', collapse = ' ') )
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Fear')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Sadness')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Surprise')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Joy')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Anticipation')$Comment,
                                           sept='\n', collapse = ' '))
RedditCompareEmo=c(RedditCompareEmo, paste(subset(RedditEmotion, Emotion =='Trust')$Comment,
                                           sept='\n', collapse = ' '))

preprocessing = function(Corp) {
  StripString=content_transformer(function(x, pattern) gsub(pattern, ' ', x))
  SearchReplace=content_transformer(function(x, pattern1, pattern2) gsub(pattern1, pattern2, x))
  latin2Ascii=content_transformer(function(x) iconv(x, 'latin1', 'ASCII', sub=' '))
  
  Corp=tm_map(Corp, latin2Ascii)  
  Corp=tm_map(Corp, content_transformer(tolower))
  Corp=tm_map(Corp, removeWords, stopwords("english"))
  Corp=tm_map(Corp, removePunctuation)
  Corp=tm_map(Corp, removeNumbers)
  Corp=tm_map(Corp, stripWhitespace)
  
  Corp=tm_map(Corp, StripString, 'http[[:alnum:]]*')
  Corp=tm_map(Corp, StripString, '[\r\n]')
  Corp=tm_map(Corp, StripString, '[\t]')
  
  Corp=tm_map(Corp, stemDocument)
  return(Corp)
}

RedditCompareCorp = Corpus(VectorSource(RedditCompareEmo))
RedditCompareCorp = preprocessing(RedditCompareCorp)
RedditCompareTDM = TermDocumentMatrix(RedditCompareCorp)
RedditCompareTDM = removeSparseTerms(RedditCompareTDM, 0.99)
colnames(RedditCompareTDM) = c('Anger', 'Disgust', 'Fear', 'Sadness', 'Surprise', 'Joy', 'Anticipation', 'Trust')
RedditCompareTDM = as.matrix(RedditCompareTDM)
cloud = comparison.cloud(RedditCompareTDM, max.words = 2000, vfont=c('serif', 'plain'), random.order = F, rot.per = .25, scale = c(2.5, .5), title.size = 1.5)


# Daily for loop-------------------------------------------------------------------------
emotion_df <- RedditEmotion
emotion_df$Week <- cut(emotion_df$Day, breaks = c(-Inf, 7, 14, 21, Inf), labels = c(1:4))
emotion_df$Final_Emotion <- " "
for(i in 1:nrow(emotion_df)){
  print(i)
  if(emotion_df[i,]$Emotion == ""){
    positive <- sum(emotion_df[i,]$Anticipation, emotion_df[i,]$Joy, emotion_df[i,]$Surprise, emotion_df[i,]$Trust)
    negative <- sum(emotion_df[i,]$Anger, emotion_df[i,]$Disgust, emotion_df[i,]$Fear, emotion_df[i,]$Sadness)
    if(positive > negative){
      value <- "Positive"
    } else if (negative > positive){
      value <- "Negative"
    } else if (negative == positive){
      value <- "Neutral"
    }
  } else {
    value <- emotion_df[i,]$Emotion
  }
  
  emotion_df[i,]$Final_Emotion <- value
}

emotion_df <- emotion_df[!duplicated(emotion_df$Comment),]

comments_per_day <- data.frame()
day_emotion_df <- data.frame()
week_emotion_df <- data.frame()
bar_chart_df <- data.frame()

emotions <- c('Anger', 'Anticipation', 'Disgust', 'Fear', 'Joy', 'Sadness', 'Surprise', 'Trust', 'Negative', 'Positive', 'Neutral')
for(day in 1:30){
  day_subset <- subset(emotion_df, emotion_df$Day == day)
  daily_total <- nrow(day_subset)
  comments_per_day <- rbind(comments_per_day, cbind(day, daily_total))
  
  day_output <- data.frame('day' = day)
  for(emotion in emotions){
    total <- nrow(subset(day_subset, day_subset$Final_Emotion == emotion))
    percentage <- round(total/daily_total,2)
    day_output <- cbind(day_output, percentage)
    mat <- names(day_output) == 'percentage'
    names(day_output)[mat] <- emotion
  }
  day_emotion_df <- rbind(day_emotion_df, day_output)
}

for(week in 1:4){
  week_subset <- subset(emotion_df, emotion_df$Week == week)
  week_output <- data.frame('week' = week)
  for(emotion in emotions){
    total <- nrow(subset(week_subset, week_subset$Emotion == emotion))
    week_output <- cbind(week_output, total)
    mat <- names(week_output) == 'total'
    names(week_output)[mat] <- emotion
    
    bar_chart_df <- rbind(bar_chart_df, cbind(week, emotion, total))
  }
  week_emotion_df <- rbind(week_emotion_df, week_output)
}

#Comments per Day
ggplot(comments_per_day, aes(x=day)) + 
  geom_line(aes(y = daily_total), color = "darkred") +
  ggtitle("Daily Reddit Posts") +
  xlab("Day") + ylab("Total Comments")

#Sentiment by Day Plot
ggplot(day_emotion_df, aes(x=day)) + 
  geom_line(aes(y = Positive), color = "darkred") +
  geom_line(aes(y = Negative), color = "steelblue") +
  geom_line(aes(y = Neutral), color = "green") +
  # geom_line(aes(y = Fear), color = "green") +
  # geom_line(aes(y = Anger), color = "blue") +
  # geom_line(aes(y = Sadness), color = "red") +
  # geom_line(aes(y = Disgust), color = "orange") +
  # ggtitle("Daily Reddit Posts with Fear, Anger, Sadness, and Disgust") +
  ggtitle("Daily Positive, Negative & Neutral Sentiment") +
  xlab("Day") + ylab("Percentage of Comments")

#Sentiment by Week Plot
ggplot(week_emotion_df, aes(x=week)) + 
  geom_line(aes(y = Positive), color = "darkred") +
  geom_line(aes(y = Negative), color = "steelblue") +
  geom_line(aes(y = Fear), color = "green") +
  ggtitle("Weekly Positive vs Negative Sentiment") +
  xlab("Week") + ylab("Total Comments")

#Weekly Emotion Bar
ggplot(bar_chart_df, aes(fill = emotion, x = week, y = total)) + geom_bar(position = "stack", stat = "identity") +
  ggtitle("Weekly Distribution of Emotion") +  xlab("Week") + ylab("Total Comments")

# Comment Word Cloud
png("Wordcloud_Reddit.png", width = 10, height = 8, units = 'in', res = 300)
wordcloud(data$comment, min.freq = 100, max.words = 100, vfont = c('serif', 'plain'), random.order = FALSE, scale = c(6,0,5), rot.per = 0, colors = brewer.pal(8, 'Dark2')[c(2,4,8)])
dev.off()

non_null_total <- nrow(subset(emotion_df, is.null(emotion_df$Emotion)))


#################################################################################
#################################################################################
#################################################################################
###REDDIT USER ANALYSIS###
library(tm)
library(SnowballC)
library(RColorBrewer)
library(syuzhet)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(wordcloud)
library(Rcpp)
library(printr)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(data.table)
library(openxlsx)
library(writexl)
#update.packages()

# Read Data In and manipulate data
df <- read.csv("reddit_df.csv")
data = df
data$date = as.Date(data$c.date1..date2..date3..date4., format = "%m-%d-%Y")
data$day = day(data$date)

# Top 10 posts by upvotes
top10posts = data %>%
  group_by(c.comments1..comments2..comments3..comments4.) %>%
  summarize(TotalUpVotes = sum(c.upvote1..upvote2..upvote3..upvote4.))
top10posts = top10posts[order(-top10posts$TotalUpVotes),]
head(top10posts, 10)

# Top 10 accounts by upvotes
top10accts = data %>%
  group_by(c.author1..author2..author3..author4.) %>%
  summarize(TotalUpVotes = sum(c.upvote1..upvote2..upvote3..upvote4.))
top10accts = top10accts[order(-top10accts$TotalUpVotes),]
head(top10accts, 10)

# Bottom 10 posts by downvotes
bot10posts = data %>%
  group_by(c.comments1..comments2..comments3..comments4.) %>%
  summarize(TotalDownVotes = sum(c.upvote1..upvote2..upvote3..upvote4.))sonic
bot10posts = bot10posts[order(bot10posts$TotalDownVotes),]
head(bot10posts, 10)

# Bottom 10 accounts by downvotes
bot10accts = data %>%
  group_by(c.author1..author2..author3..author4.) %>%
  summarize(TotalDownVote = sum(c.upvote1..upvote2..upvote3..upvote4.))
bot10accts = bot10accts[order(bot10accts$TotalDownVote),]
head(bot10accts, 10)








