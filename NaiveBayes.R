#Import libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer) 
library(e1071)         #For Naive Bayes
library(caret)         #For the Confusion Matrix

library(gmodels) #provides CrossTable() function for comparison

db<-read.csv('train.csv')

db<-db[,c(3,5)]
colnames(db)<-c('Msg','Tag')

db$Tag<-factor(db$Tag)
# creating our corpus
text_corpus <- VCorpus(VectorSource(db$Msg))

# Viewing the content of more than one texts using lapply() function
lapply(text_corpus[1:5], as.character) 

cleanCorpus <- tm_map(text_corpus, content_transformer(tolower)) # lowercase all texts
cleanCorpus <- tm_map(cleanCorpus, removeNumbers) # remove all numbers
cleanCorpus <- tm_map(cleanCorpus, removeWords, stopwords('english')) # remove all common words such as to, but and etc.
cleanCorpus <- tm_map(cleanCorpus, removePunctuation) # remove all punctuation
cleanCorpus <- tm_map(cleanCorpus, stripWhitespace) # remove all whitespace

text_dtm <- DocumentTermMatrix(cleanCorpus)
inspect(text_dtm)

# Creating train and test portions 
porcentaje<-0.7
set.seed(123)
corte <- sample(nrow(text_dtm),nrow(text_dtm)*porcentaje)

train <- text_dtm[corte,] # 70% for training
test <- text_dtm[-corte, ] # 30% for testing
train_type <- db[corte, ]$Tag
test_type <- db[-corte, ]$Tag


#training portion
tbl_train <- prop.table(table(train_type))

#testing portion
tbl_test <- prop.table(table(test_type))

spamText <- subset(db, Tag == "Adequate") 
wordcloud(spamText$Msg, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2") )

hamText <- subset(db, Tag =="Ineffective") # selecting ham texts
wordcloud(hamText$Msg, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2"))

freq_words <- findFreqTerms(train, 5) 
str(freq_words)

# Selecting only the frequent words from the train and test datasets
freq_words_train <- train[ , freq_words]
freq_words_test <- test[ , freq_words]


# creating a function for conversion
convert <- function(x) {x <- ifelse(x > 0, "y", "n")} 
train <- apply(freq_words_train, MARGIN = 2, convert)
test <- apply(freq_words_test, MARGIN = 2, convert)
str(train) # verifying the conversion

# Creating a Naive Bayes classifier
sms_classifier <- naiveBayes(train, train_type)
# Making prediction & evaluation with the classifier
test_prediction <- predict(sms_classifier, test)

CrossTable(test_prediction, test_type, 
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

#Modelo 2
sms_classifier_improved <- naiveBayes(train, train_type, laplace = 1)
test_prediction_improved <- predict(sms_classifier_improved, test)

CrossTable(test_prediction_improved, test_type, 
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

