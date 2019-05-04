#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predicting a next word"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       
       textInput("text", h3("Text input"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      img(src = "rstudio.png", height = 72, width = 92),
      h3("Wait....."),
      
      textOutput("ready"),
      h3("The next word is:"),
      textOutput("word")
      
    )
  )
))
