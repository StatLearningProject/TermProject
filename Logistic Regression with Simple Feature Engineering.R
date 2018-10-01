library(tidyverse)
library(tidytext)
library(tm)
library(nnet)
old_train <- read_tsv("train.tsv")

# Code to filter out bad sentences and filter out stop words. Takes a minute to compile
train = data_frame()
for(i in seq(1, dim(old_train)[1])){
  Phrase = old_train$Phrase[i]
  if(grepl("\\.$", Phrase)){
    if(grepl("^[[:upper:]]", Phrase)){
      Phrase = removeWords(Phrase, stopwords("en"))
      train = rbind(train, cbind(old_train[i,1:2], Phrase, old_train[i,4]))
    }
  }
}

#Feature Engineering Section

#Going to create # of pos words, # of neutral words, # of neg words features using AFINN dictionary.
#AFINN ranks words from negative to positive on -5 to 5 scale.
#Will map words [-5,-2] as negative [-1,1] as neutral, [2,5] as positive
  
afinn <- get_sentiments(lexicon = "afinn")

afinn.negative <- afinn %>% filter(score <= -2) %>% pull(word)
afinn.neutral <- afinn %>% filter(score > -2 & score <= 1) %>% pull(word)
afinn.positive <- afinn %>% filter(score >=2) %>% pull(word)

#Function to extract # of pos, neutral, and negative words from a phrase
get_sum_of_words <- function(phrase, dictionary){
  sum_of_words <- sum(str_detect(phrase, dictionary))
  sum_of_words
}

#Create columns of neutral, negative, and positive sum of words to add to train dataset
nrows = dim(train)[1]
neutral <- rep(0, nrows)
negative <- rep(0, nrows)
positive <- rep(0, nrows)


for(i in seq(1,nrows)){
  neutral[i] <- get_sum_of_words(train$Phrase[i], afinn.neutral)
  negative[i] <- get_sum_of_words(train$Phrase[i], afinn.negative)
  positive[i] <- get_sum_of_words(train$Phrase[i], afinn.positive)
  if(i %% 1000 == 0){
    print(i)
  }
}

neutral <-cbind(neutral)
negative <- cbind(negative)
positive <- cbind(positive)

#Create final dataset with engineered variables
train_engineered <- train %>% 
  mutate(lengthOfPhrase = as.integer(str_count(Phrase,'\\w+')))
train_engineered <- cbind(train_engineered, neutral,negative,positive)
train_engineered <- train_engineered %>% select(PhraseId:Phrase, lengthOfPhrase:positive, Sentiment)

#Run Logistic Regression
train_set <- train_engineered %>% select(lengthOfPhrase:Sentiment)
train_set$Sentiment <- as.factor(train_set$Sentiment)
log.train <- multinom(Sentiment ~., data = train_set)
preds <- predict(log.train,train_set)

mean(train_set$Sentiment == preds, na.rm = TRUE)
#52% accuracy on full training set. Most likely overfit. Will surely be much worse on test set. SVM might boost accuracy, but don't want to run right now.

#K-Folds to estimate Test Error
n.train <- nrow(train_set)
nfolds <- 5
set.seed(123)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))

#Vector to hold validation errors
logistic.errors <- rep(NA,5)

for(i in seq(nfolds)){
  
  #Logistic Regression
  log.train <- multinom(Sentiment ~., data = train_set[-s[[i]],])
  preds <- predict(log.train, train_set[s[[i]],])
  cv.error <- mean(preds != train_set[s[[i]],]$Sentiment, na.rm = TRUE)
  logistic.errors[i] <- cv.error
}
logistic.errors
mean(logistic.errors)
#47% mean error rate. So average 53% accuracy with K-Folds. So accuracy actually unharmed.