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
      titlePanel("Next Word Prediction"),
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
            sidebarPanel(),
            mainPanel(
                  textInput("txtID",
                            "Enter text for which to predict the next word",
                            "Enter your text"),
                  submitButton("Predict"),
                  h4("And the predicted words are..."),
                  verbatimTextOutput("oWords")
            )
      )
)
)
