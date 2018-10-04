library(e1071)
library(readxl)
library(tidyverse)

#SVM with Simple Feature Engineering

train_engineered <- read_excel("C:\\Users\\George\\Documents\\Rutgers\\Statistical Learning\\Final Project\\train_engineered.xlsx")
train_set <- train_engineered %>% select(lengthOfPhrase:Sentiment)
train_set$Sentiment <- as.factor(train_set$Sentiment)

#Cross Validation with Linear Kernel
set.seed(1)
svm.tune <- tune(svm, Sentiment~., data = train_set, kernel = "linear",
                 ranges = list(cost=c(.001,.01,.1,1,5,10,100)))
