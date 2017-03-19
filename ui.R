library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Next Word Predictin"),
  
  fluidRow(
    column(12,
           br(),
           h4("This application predicts the next word in a phrase as you type it."),
           br(),
           br()
    )
  ),
  
  fluidRow(
    column(6,
           textInput("input_str", 
                     label = "Enter text:", 
                     value = " "
           )             
    )    
  ),
  
  fluidRow(
    column(12,
           br(),
           br(),
           h4("Predicted next word:", style = "color:blue"), 
           verbatimTextOutput("nextword")            
    )
  )
))
