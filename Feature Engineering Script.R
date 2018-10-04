library(tidytext)
#Feature Engineering Scripts

train <- read_tsv("C:\\Users\\George\\Documents\\Rutgers\\Statistical Learning\\Final Project\\train.tsv")

test <-read_tsv("C:\\Users\\George\\Documents\\Rutgers\\Statistical Learning\\Final Project\\test.tsv")

#Feature Engineering for Train Data 
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
neutral <- rep(0, 156060)
negative <- rep(0, 156060)
positive <- rep(0, 156060)
for(i in seq(1,156060)){
  neutral[i] <- get_sum_of_words(train$Phrase[i], afinn.neutral)
  negative[i] <- get_sum_of_words(train$Phrase[i], afinn.negative)
  positive[i] <- get_sum_of_words(train$Phrase[i], afinn.positive)
}

neutral <-cbind(neutral)
negative <- cbind(negative)
positive <- cbind(positive)

#Create final dataset with engineered variables
train_engineered <- train %>% 
  mutate(lengthOfPhrase = as.integer(str_count(Phrase,'\\w+')))
train_engineered <- cbind(train_engineered, neutral,negative,positive)
train_engineered <- train_engineered %>% select(PhraseId:Phrase, lengthOfPhrase:positive, Sentiment)

#write_excel_csv(train_engineered,"C:\\Users\\George\\Documents\\Rutgers\\Statistical Learning\\Final Project\\train_engineered")



#Feature Engineering for Test Data Set
neutral <- rep(0, 66292)
negative <- rep(0, 66292)
positive <- rep(0, 66292)

for(i in seq(1,66292)){
  neutral[i] <- get_sum_of_words(test$Phrase[i], afinn.neutral)
  negative[i] <- get_sum_of_words(test$Phrase[i], afinn.negative)
  positive[i] <- get_sum_of_words(test$Phrase[i], afinn.positive)
}

neutral <-cbind(neutral)
negative <- cbind(negative)
positive <- cbind(positive)

#Create final dataset with engineered variables
test_engineered <- test %>% 
  mutate(lengthOfPhrase = as.integer(str_count(Phrase,'\\w+')))
test_engineered <- cbind(test_engineered, neutral,negative,positive)
test_set <- test_engineered %>% select(lengthOfPhrase:positive)

#write_excel_csv(test_engineered,"C:\\Users\\George\\Documents\\Rutgers\\Statistical Learning\\Final Project\\test_engineered.csv")
