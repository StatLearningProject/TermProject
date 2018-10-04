library(randomForest)
library(readxl)
library(tidyverse)

#Random Forest with Basic Feature Engineered Dataset

train_engineered <- read_excel("C:\\Users\\George\\Documents\\Rutgers\\Statistical Learning\\Final Project\\train_engineered.xlsx")
train_set <- train_engineered %>% select(lengthOfPhrase:Sentiment)
train_set$Sentiment <- as.factor(train_set$Sentiment)

#K-Folds to Estimate Test Error
n.train <- nrow(train_set)
nfolds <- 5
set.seed(123)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))

forest.errors <- rep(NA,5)

for(i in seq(nfolds)){
  
  forest.fit <- randomForest(Sentiment~., data = train_set[-s[[i]],], mtry = 2, ntrees = 500, na.action = na.exclude)
  forest.preds <- predict(forest.fit, train_set[s[[i]],], type = "class")
  forest.cv.error <- mean(forest.preds != train_set[s[[i]],]$Sentiment, na.rm = TRUE)
  forest.errors[i] <- forest.cv.error
}

mean(forest.errors) #53% accuracy rate with random forest. No improvement over logistic regression

#Now we check error rate with the test set. first we create a model with the full training set.

forest.fit <- randomForest(Sentiment~., data = train_set, mtry = 2, ntrees = 500, na.action = na.exclude)
test.preds <- predict(forest.fit, test_set, type = "class") #53% accuracy rate on Kaggle
