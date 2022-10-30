#Import libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer) 
library(e1071)         #For Naive Bayes
library(caret)         #For the Confusion Matrix

library(gmodels) #provides CrossTable() function for comparison

db<-read.csv('train.csv')

data<-subset(db,select=c('discourse_effectiveness','discourse_text'))
