

#### Load packages ######

library(twitteR)
library(tidyverse)
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(reshape2)
library(radarchart)
library(RWeka)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(devtools)
devtools::install_github("lchiffon/wordcloud2")

library(wordcloud2)

# Set working directory 
setwd('C:\\Users\\Ben\\Desktop\\UNH Classes\\Data 902\\')


#### Uncomment to scrape Twitter and Save Data #####

# consumer_key <- "####"
# consumer_secret <-"####"
# access_token <- "####"
# access_secret <- "####"
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
# tw = searchTwitter('#tesla -RT', n = 10000, lang = 'en')
# d = twListToDF(tw)
# 
# # Take only the text field from the scraped Twitter results
# teslatweet <- iconv(d$text, from = "UTF-8", to = "ASCII", sub = "")
# 
# # Write scraped data to CSV to save from scraping multiple times
# write.csv(teslatweet,'teslatweet.csv',row.names=FALSE)

##### End Scraping and Saving Data ################




# If saved, import csv to load data
fromCSV <- read.csv(file = 'teslatweet.csv', header = FALSE, skip =1)
fromCSV <- as.matrix(fromCSV)
teslatweet <- as.vector(fromCSV)
rm(fromCSV)
### End Loading Data from CSV ####


Vsource <- VCorpus(VectorSource(teslatweet))

# Text transformations
cleanCorpus <- function(corpus){
  
  corpus_tmp <- tm_map(corpus_tmp, str_replace_all, "model3", "model 3")
  corpus_tmp <- tm_map(corpus_tmp, removePunctuation)
  corpus_tmp <- tm_map(corpus_tmp, stripWhitespace)
  corpus_tmp <- tm_map(corpus_tmp, removeNumbers)
  
  corpus_tmp <- tm_map(corpus_tmp, content_transformer(tolower))
  
  #corpus_tmp <- VCorpus(VectorSource(corpus_tmp))
  v_stopwords <- c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt", 
                                           "tesla","teslas","car","cars",
                                           "tsla"))
  corpus_tmp <- tm_map(corpus_tmp, removeWords, v_stopwords)
  
  return(corpus_tmp)
  
}



# Created another cleaning function for bigrams and trigrams because when using VCorpus, tm_map(corpus_tmp, PlainTextDocument)
# throws the following error: Error in UseMethod("tm_map", x) : no applicable method for 'tm_map' applied to an object of class "character"

cleanCorpus2 <- function(corpus){
  
  corpus_tmp <- tm_map(corpus, removePunctuation)
  corpus_tmp <- tm_map(corpus_tmp, stripWhitespace)
  corpus_tmp <- tm_map(corpus_tmp, removeNumbers)
  corpus_tmp <- tm_map(corpus_tmp, content_transformer(tolower))
  corpus_tmp <- tm_map(corpus_tmp, PlainTextDocument)
  #corpus_tmp <- tm_map(corpus_tmp, str_replace_all, "model3", "model 3")
  v_stopwords <- c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt", 
                                           "tesla","teslas","car","cars",
                                           "tsla"))
  corpus_tmp <- tm_map(corpus_tmp, removeWords, v_stopwords)
  
  return(corpus_tmp)
  
}



# Most frequent terms 
frequentTerms <- function(text){
  
  s_cor <- Corpus(VectorSource(text))
  s_cor_cl <- cleanCorpus(s_cor)
  s_tdm <- TermDocumentMatrix(s_cor_cl)
  s_tdm <- removeSparseTerms(s_tdm, 0.999)
  m <- as.matrix(s_tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  #dm <- top_n(data.frame(word=names(word_freqs), freq=word_freqs), 200)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}

# Define bigram tokenizer 
tokenizer  <- function(x){
  
  NGramTokenizer(x, Weka_control(min=2, max=2))
  
}

# Most frequent bigrams 
frequentBigrams <- function(text){
  
  s_cor <- VCorpus(VectorSource(text))
  s_cor_cl <- cleanCorpus2(s_cor)
  s_tdm <- TermDocumentMatrix(s_cor_cl, control=list(tokenize=tokenizer))
  s_tdm <- removeSparseTerms(s_tdm, 0.999)
  m <- as.matrix(s_tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}



# Define trigram tokenizer 
tokenizer3  <- function(x){
  
  NGramTokenizer(x, Weka_control(min=3, max=3))
  
}



# Most frequent bigrams 
frequentTrigrams <- function(text){
  
  s_cor <- VCorpus(VectorSource(text))
  s_cor_cl <- cleanCorpus2(s_cor)
  s_tdm <- TermDocumentMatrix(s_cor_cl, control=list(tokenize=tokenizer3))
  s_tdm <- removeSparseTerms(s_tdm, 0.999)
  m <- as.matrix(s_tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}

##### Additional Analysis from original tweet data #####

# # How many tweets?
# length(teslatweet)
# 
# # How many different sources?
# length(levels(d$screenName))
# 
# 
# # Top 20 Usernames
# top.sources <- as.data.frame(sort(table(d$screenName), decreasing=TRUE))[1:20,]
# 
# # Visualization 
# ggplot(data=top.sources, aes(x=Var1, y=Freq)) +
#   geom_bar(stat="identity", fill="#56B4E9", colour="black") +
#   theme(axis.text.x=element_text(angle=45, hjust=1)) +
#   labs(x="screenName", y="Number of Tweets")

########################################################
########################################################


# # Unigram wordcloud Tesla logo
# wordcloud2(frequentTerms(teslatweet), size=2, color = "red", ellipticity=4,
#            figPath="C:/Users/Ben/Desktop/UNH Classes/Data 902/tlogo2.jpg")


# # Unigram wordcloud Model 3
# wordcloud2(frequentTerms(teslatweet), size=1.6, color = "black", ellipticity=1,
#            figPath="C:/Users/Ben/Desktop/UNH Classes/Data 902/mod3A.png")


# unigram wordcloud elon
wordcloud2(frequentTerms(teslatweet), size=2, color = "black", ellipticity=1,
           figPath="C:/Users/Ben/Desktop/UNH Classes/Data 902/elon.png")


# unigram wordcloud Model X
wordcloud2(frequentTerms(teslatweet), size=2, color = "black", ellipticity=1,
           figPath="C:/Users/Ben/Desktop/UNH Classes/Data 902/modx2.png")


# # Bigram wordcloud Tesla logo
# wordcloud2(frequentBigrams(teslatweet), size=1.2, color = "black", ellipticity=1,
#            figPath="C:/Users/Ben/Desktop/UNH Classes/Data 902/tlogo.jpg")

# Bigram wordcloud Model 3
wordcloud2(frequentBigrams(teslatweet), size=0.85, color = "black", ellipticity=1,
           minRotation = -pi/3, maxRotation = pi/3,
           figPath="C:/Users/Ben/Desktop/UNH Classes/Data 902/mod3A.png")

# Trigram wordcloud car outline
wordcloud2(frequentTrigrams(teslatweet), size=0.2, color = "black", ellipticity=1,
           minRotation = -pi/2.25, maxRotation = pi/2.25,
           figPath="C:/Users/Ben/Desktop/UNH Classes/Data 902/tlogo.jpg")


# # Trigram wordcloud
# wordcloud2(frequentTrigrams(teslatweet), size=0.15, color = "red", ellipticity=1,
#            minRotation = -pi/2, maxRotation = pi/2,
#            figPath="C:/Users/Ben/Desktop/UNH Classes/Data 902/tlogo.jpg")


# wordcloud2(frequentBigrams(teslatweet), size=1.6, color = "red", ellipticity=1,
#            figPath="C:/Users/Ben/Desktop/UNH Classes/Data 902/tlogo.jpg")



clean_corpus <- function(cleaned_corpus){
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, str_replace_all, "model3", "model 3")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, 
                           c(stopwords("english"), 
                             c("thats","weve","hes","theres","ive","im",
                               "will","can","cant","dont","youve","us",
                               "youre","youll","theyre","whats","didnt",
                               "tesla","teslas","car","cars","tsla")))
  return(cleaned_corpus)
}

review_corpus <- Corpus(VectorSource(teslatweet))
cleaned_review_corpus <- clean_corpus(review_corpus)


########## Create TF-IDF Matrix #############

tfidf_tdm <- TermDocumentMatrix(cleaned_review_corpus,control=list(weighting=weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)

# Term Frequency
term_frequency <- rowSums(tfidf_tdm_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)


# Create a wordcloud for the values in word_freqs
# I was getting warnings saying the words didn't fit so a workaround was to start a new device.
# The new device allowed me to render the wordcloud without ommitting major chunks

dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,random.order = FALSE, max.words=300,colors=brewer.pal(8, "Paired"), scale=c(3,.5))

#############################################
########### End of TF-IDF ###################





########### Sentiment Analysis ##############

tidy_mytext <- tidy(TermDocumentMatrix(cleaned_review_corpus))
bing_lex <- get_sentiments("bing")
mytext_bing <- inner_join(tidy_mytext, bing_lex, by = c("term" = "word"))
mytext_bing$sentiment_n <- ifelse(mytext_bing$sentiment=="negative", -1, 1)
mytext_bing$sentiment_score <- mytext_bing$count*mytext_bing$sentiment_n

sentiment_summary <- mytext_bing %>%
  group_by(document) %>%
  summarize(tweet_sentiment = sum(sentiment_score)) %>%
  arrange(desc(tweet_sentiment))

# Plot Histogram
hist(sentiment_summary$tweet_sentiment, col = "blue4", xlab = "Positivity Score", 
     main = "#Tesla Sentiment on Twitter")





########## Emotions #############

nrc_lex <- get_sentiments("nrc")
table(nrc_lex$sentiment)

loughran_lex <- get_sentiments("loughran")
table(loughran_lex$sentiment)



### Radar Chart with emotional analysis using NRC ####
library(radarchart)
nrc_lex <- get_sentiments("nrc")
mytext_nrc <- inner_join(tidy_mytext, nrc_lex, by = c("term" = "word"))
mytext_nrc_noposneg <- mytext_nrc[!(mytext_nrc$sentiment %in% c("positive","negative")),]
emotion_summary <- mytext_nrc_noposneg %>%
  group_by(document,sentiment) %>%
  summarize(tweet_sentiment = sum(count)) %>%
  arrange(desc(tweet_sentiment))

emotion_overall_summary <- mytext_nrc_noposneg %>%
  group_by(sentiment) %>%
  summarize(tweet_sentiment = sum(count)) %>%
  arrange(desc(tweet_sentiment))

chartJSRadar(emotion_overall_summary, height = 700)



##### Scrape #ford tweets for comparison cloud #####



#### Uncomment to scrape Twitter and Save Data #####
# 
# consumer_key <- "####"
# consumer_secret <-"####"
# access_token <- "####"
# access_secret <- "####"
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
# tw = searchTwitter('#ford -RT', n = 10000, lang = 'en')
# d = twListToDF(tw)
# 
# # Take only the text field from the scraped Twitter results
# fordtweet <- iconv(d$text, from = "UTF-8", to = "ASCII", sub = "")
# 
# # Write scraped data to CSV to save from scraping multiple times
# write.csv(fordtweet,'fordtweet.csv',row.names=FALSE)

##### End Scraping and Saving Data ################




# If saved, import Ford csv to load data
fromCSV <- read.csv(file = 'fordtweet.csv', header = FALSE, skip =1)
fromCSV <- as.matrix(fromCSV)
fordtweet <- as.vector(fromCSV)
rm(fromCSV)
### End Loading Data from Ford CSV ####

clean_text = function(x)
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

#### Get cleaned versions of both datasets for comparison

cleanTesla <- clean_text(teslatweet)
cleanFord <- clean_text(fordtweet)

# Join texts into a vector
tsla = paste(cleanTesla, collapse=" ")
ford = paste(cleanFord, collapse=" ")
# put everything in a single vector
all = c(tsla, ford)

# Remove stop-words
all = removeWords(all,
                  c(stopwords("english"), "ford", "tesla", "tmobile", "metropcs", 
                    "thats","weve","hes","theres","ive","im","will","can","cant",
                    "dont","youve","us","youre","youll","theyre","whats","didnt", 
                    "tesla","teslas","car","cars","tsla"))

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

# add column names
colnames(tdm) = c("Tesla", "Ford")

# comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c( "red", "blue"),
                 title.size=1.5, max.words=500)

