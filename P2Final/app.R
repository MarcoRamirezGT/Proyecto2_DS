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
      
    ),
    
    box(
      title = "Random Forest",
      textOutput("text_forest"),
      
    )

    # Sidebar with a slider input for number of bins 
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$text <- renderText('Texto introducido')
  output$value <- renderText({ input$texto })
  
  output$text_naive<-renderText({
   
    predict(sms_classifier, input$texto)
    
    
  }
    
    
    
    
  )
  


}

# Run the application 
shinyApp(ui = ui, server = server)
