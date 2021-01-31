library(rtweet)
library(tidytext)
library(ROAuth)
library(readr)
library(ggplot2)
library(dplyr)
library("quanteda")
library("quanteda.corpora")
library("quanteda.dictionaries")
library("quanteda.textmodels")
library("readtext")
library("spacyr")
library("caret")
library("e1071")
library("dplyr")
library("lubridate")
library("readtext")
library(wordcloud)
library(RColorBrewer)

#authorization code
my_oauth <- rtweet::create_token(app = your_app, consumer_key = your_key,
                                 consumer_secret = your_secret, access_token=your_access_token,
                                 access_secret = your_access_secret)

#scrape the tweets
biden_tweets <- rtweet::get_timeline(c("joebiden"), n = 3200, parse=T, token=my_oauth)
rtweet::write_as_csv(biden_tweets, "biden_tweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

#create corpus
bidenTweets<-read.csv("biden_tweets.csv")
dim(bidenTweets)
bidenTexts <- readtext::readtext("biden_tweets.csv", text_field = "text")
head(bidenTexts)
dim(bidenTexts)


#most frequently retweeted tweets 
head(bidenTexts[order(-bidenTexts$retweet_count), 'text' ],5)

#most favourite made tweets
head(bidenTexts[order(-bidenTexts$favorite_count), 'text'], 5)

#create the corpus
bidenCorpus<-quanteda::corpus(bidenTexts)
dfmbiden <- dfm(bidenCorpus, remove = c(stopwords("english")),
                remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE,tolower=T)


#plot the word cloud
set.seed(111)
textplot_wordcloud(dfmbiden,min_size = 1, max_size = 4, max_words = 150, color = RColorBrewer::brewer.pal(8, "Dark2"))

#define dictionary of topics and example words
dict <- dictionary(list(capitol_attack = c("attack", "responsibility", "capitol", "shame", "storming"),
        cabinet = c("cabinet", "minister", "advisory"),
        education = c("school", "education"),
        employment = c("job", "right", "wage","employment"),
        winning = c("victory", "win", "thank", "america","great", "back", "proud"),
        economy = c("poverty","crisis", "society", "job*", "rent", "relief" ),
        war = c("war", "soldier*", "nuclear"),
        crime = c("crime*", "murder", "killer", "violence"),
        corona = c("covid-19", "corona", "pandemic", "virus", "mask", "vaccine"),
        trump = c("trump", "donald","president", "law" )))

#fit seededlda model 
library(seededlda)
slda <- textmodel_seededlda(dfmbiden, dict, residual = TRUE)
topwords_biden<-as.data.frame(seededlda::terms(slda, 20))
head(topics(slda), 20)

#store topics thus identified into the tweets data frame
bidenTweets$topic <- seededlda::topics(slda)
View(bidenTweets)

#tabulate frequencies of each topic in your corpus:
topics_table<-ftable(bidenTweets$topic)
View(topics_table)
topicsprop_table<-as.data.frame(prop.table(topics_table))
View(topicsprop_table)

#plot topic wise frequencies
ggplot(data=topicsprop_table, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity") +
  labs (x= "Topics", y = "Topic %")+
  labs(title = "Topic proportions - Biden") +
  theme(axis.text.x = element_text(face="bold", 
                                   size=10, angle=45,hjust = 1)) 
