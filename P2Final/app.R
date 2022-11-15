#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(openxlsx)
library(tidyverse)
library(ggplot2)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- fluidPage(
    

    # Application title
    titlePanel("Proyecto 2: Prediccion de argumentos efectivos"),
    
    textInput('texto','Texto a predecir',' '),
    textOutput("text"),
    verbatimTextOutput('value'),
    
    box(
      title = "Naive Bayes",
      textOutput("text_naive"),
      
    )

    # Sidebar with a slider input for number of bins 
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$text <- renderText('Texto introducido')
  output$value <- renderText({ input$texto })
  
  output$text_naive<-renderText({
    # library(tm)
    # library(SnowballC)
    # library(wordcloud)
    # library(RColorBrewer) 
    # library(e1071)         #For Naive Bayes
    # library(caret)         #For the Confusion Matrix
    # 
    # library(gmodels) #provides CrossTable() function for comparison
    # 
    # db<-read.csv('train.csv')
    # 
    # db<-db[,c(3,5)]
    # colnames(db)<-c('Msg','Tag')
    # 
    # db$Tag<-factor(db$Tag)
    # # creating our corpus
    # text_corpus <- VCorpus(VectorSource(db$Msg))
    # 
    # # Viewing the content of more than one texts using lapply() function
    # lapply(text_corpus[1:5], as.character) 
    # 
    # cleanCorpus <- tm_map(text_corpus, content_transformer(tolower)) # lowercase all texts
    # cleanCorpus <- tm_map(cleanCorpus, removeNumbers) # remove all numbers
    # cleanCorpus <- tm_map(cleanCorpus, removeWords, stopwords('english')) # remove all common words such as to, but and etc.
    # cleanCorpus <- tm_map(cleanCorpus, removePunctuation) # remove all punctuation
    # cleanCorpus <- tm_map(cleanCorpus, stripWhitespace) # remove all whitespace
    # 
    # text_dtm <- DocumentTermMatrix(cleanCorpus)
    # inspect(text_dtm)
    # 
    # # Creating train and test portions 
    # porcentaje<-0.7
    # set.seed(123)
    # corte <- sample(nrow(text_dtm),nrow(text_dtm)*porcentaje)
    # 
    # train <- text_dtm[corte,] # 70% for training
    # test <- text_dtm[-corte, ] # 30% for testing
    # train_type <- db[corte, ]$Tag
    # test_type <- db[-corte, ]$Tag
    # 
    # 
    # #training portion
    # tbl_train <- prop.table(table(train_type))
    # 
    # #testing portion
    # tbl_test <- prop.table(table(test_type))
    # 
    # spamText <- subset(db, Tag == "Adequate") 
    # wordcloud(spamText$Msg, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2") )
    # 
    # hamText <- subset(db, Tag =="Ineffective") # selecting ham texts
    # wordcloud(hamText$Msg, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2"))
    # 
    # freq_words <- findFreqTerms(train, 5) 
    # 
    # 
    # # Selecting only the frequent words from the train and test datasets
    # freq_words_train <- train[ , freq_words]
    # freq_words_test <- test[ , freq_words]
    # 
    # 
    # # creating a function for conversion
    # convert <- function(x) {x <- ifelse(x > 0, "y", "n")} 
    # train <- apply(freq_words_train, MARGIN = 2, convert)
    # test <- apply(freq_words_test, MARGIN = 2, convert)
    # 
    # 
    # 
    # 
    # # Creating a Naive Bayes classifier
    # sms_classifier <- naiveBayes(train, train_type)
    predict(sms_classifier, input$texto)
    
    
  }
    
    
    
    
  )
  


}

# Run the application 
shinyApp(ui = ui, server = server)
