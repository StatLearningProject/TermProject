install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tm)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tidyverse)
library(readxl)
library(tidytext)

#Visualizations on Train Engineered DataSet
train_engineered <- read_excel("C:\\Users\\George\\Documents\\Rutgers\\Statistical Learning\\Final Project\\train_engineered.xlsx")
train_engineered %>% 
  group_by(Sentiment) %>% 
  summarize(averageLength = mean(lengthOfPhrase, na.rm=TRUE))

train_engineered %>% 
  ggplot(aes(Sentiment))+
  geom_bar() + ggtitle("Distribution of Sentiment Data")

#Pull out words for each sentiment level - For use in unigram wordcloud function
negative_raw <- train_engineered %>% filter(Sentiment == 0) %>% select(Phrase) %>% na.omit()
somewhat_negative_raw <- train_engineered %>% filter(Sentiment == 1) %>% select(Phrase) %>% na.omit()
neutral_raw <- train_engineered %>% filter(Sentiment == 2) %>% select(Phrase) %>% na.omit()
somewhat_positive_raw <- train_engineered %>% filter(Sentiment == 3) %>% select(Phrase) %>% na.omit()
positive_raw <- train_engineered %>% filter(Sentiment == 4) %>% select(Phrase) %>% na.omit()

#Some custom stop word lists for extra filtering
custom_stop <- tibble(word = c('"is','.",',",",".",'."','"the','"a','.',"'s","n't","--","...", "'","`",
                               "``","''","-rrb-",":","-lrb-", "'re", "-", "'ve","'ll", ";","?","d","'m",
                               "ca","wo","ca n't", "wo n't", "88 minute",'"most','film"','"', '"it', '"to', 
                               '"this', '"that', '"its', 'story"', "is",'"of', '"are', '"one', '..."', '",', '"as',
                               "an", ',",', '"is', '...",', 'and"', "lrb", "rrb"))
movie_words <- c("movie", "characters", "film", "minutes","comedy", "drama", "script","makes", "minute")

#Wordclouds
unigram_wordcloud(negative_raw)
unigram_wordcloud(somewhat_negative_raw)
unigram_wordcloud(neutral_raw)
unigram_wordcloud(somewhat_positive_raw)
unigram_wordcloud(positive_raw)

bigram_wordcloud(negative_raw)
bigram_wordcloud(somewhat_negative_raw)
bigram_wordcloud(neutral_raw)
bigram_wordcloud(somewhat_positive_raw)
bigram_wordcloud(positive_raw)


#Functions to make unigram and bigram word clouds for each sentiment level
unigram_wordcloud <- function(data){
  
  #Split data into lowercase list of words. Make into tibble and remove stop words and some custom stop words
  unigram <- data %>%
    unnest_tokens(word, Phrase)
  unigram <- unigram %>% 
    anti_join(stop_words) %>% 
    anti_join(custom_stop)
  
  #Create freq table
  unigram <- unigram %>% 
    count(word) %>% 
    arrange(desc(n))
  unigram <- subset(unigram, !(word %in% movie_words))
  
  #Only take top 300 words
  unigram <- head(unigram, n =300)
  

  wordcloud2(unigram, size = .60)
}

bigram_wordcloud <- function(data){
  bigram <- data %>% 
    unnest_tokens(ngram, Phrase, token = "ngrams", n = 2)
  
  #Separate into one column for each word and remove stop words
  bigram <- bigram %>%
    separate(ngram, c("word1", "word2"), sep = " ")
  bigram <- bigram %>% 
    filter(!word1 %in% stop_words$word) %>%
    filter(!word1 %in% movie_words) %>% 
    filter(!word1 %in% custom_stop) %>% 
    filter(!word2 %in% stop_words$word) %>%
    filter(!word2 %in% movie_words) %>% 
    filter(!word2 %in% custom_stop)
  
  #Make bi-grams one column again
  bigram <- bigram %>%
    unite("bigram", c("word1", "word2"), sep = " ")
  
  #Some more manual filtering
  bigram <- bigram %>%
    anti_join(custom_stop, by = c("bigram" = "word"))
  
  #Create freq table
  bigram <- bigram %>% 
    count(bigram) %>% 
    arrange(desc(n))
  
  #Only take first 300 words
  bigram <- head(bigram, n =200)
  #Create Wordcloud
  wordcloud2(bigram, size = .20)
}
