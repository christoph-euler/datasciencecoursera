library(shiny)

source("D:/Users/ceuler/Documents/2 DataScience/coursera/course_10/model_v01.R")

shinyServer(
  function(input, output) {
    output$nextword <- renderText({
      get_next_word(input$input_str)
    })
  }
)