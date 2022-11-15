install.packages("e1071")
install.packages("RTextTools")

library(e1071)
library(RTextTools)

library('tidytext')
library(plyr)
library(readr)
library(dplyr)
library(caret)
db<-read.csv('train.csv')



db<-db[,c(3,5)]
colnames(db)<-c('Msg','Tag')
db$Tag<-factor(db$Tag)


db$Tag<-factor(db$Tag)
# creating our corpus


# Set the text to lowercase
text <- tolower(db$Msg)
# Remove mentions, urls, emojis, numbers, punctuations, etc.
text <- gsub("@\\w+", "", text)
text <- gsub("https?://.+", "", text)
text <- gsub("\\d+\\w*\\d*", "", text)
text <- gsub("#\\w+", "", text)
text <- gsub("[^\x01-\x7F]", "", text)
text <- gsub("[[:punct:]]", " ", text)
# Remove spaces and newlines
text <- gsub("\n", " ", text)
text <- gsub("^\\s+", "", text)
text <- gsub("\\s+$", "", text)
text <- gsub("[ |\t]+", " ", text)

db["fix_text"] <- text

db<-db[,c(2,3)]

View(db)
  
# Creating train and test portions 
porcentaje<-0.7
set.seed(123)
corte <- sample(nrow(text_dtm),nrow(text_dtm)*porcentaje)

train <- text_dtm[corte,] # 70% for training
test <- text_dtm[-corte, ] # 30% for testing
train_type <- db[corte, ]$Tag
test_type <- db[-corte, ]$Tag



