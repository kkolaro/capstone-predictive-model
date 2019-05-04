#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# Initalization

library(shiny)

source("capstone_model_final1.R")

shinyServer(function(input, output) {
  
   
  
  ok <- ready()  
  output$ready <- renderText({ 
    paste("YOU CAN", ok)
  })
    
  
   
    
    
    ans <- reactive(wordprediction(input$text))
    output$word <- renderText(ans())


})
