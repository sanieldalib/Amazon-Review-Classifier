#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)

# Define UI for application that draws a histogram
ui <- fluidPage(
  h1("Amazon Fake Review Predictor"),
  h5("By Daniel Salib | OIDD 245 Data Project 2"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Paste any Amazon Product ID here."),
      textInput("product", label = "Product ID"),
      actionButton('urlButton', 'Go'),
      helpText( a("Read my writeup here!",href="https://sites.google.com/view/dsalib-oidd245-dp2/home")
      )
    ),
    
    mainPanel(
      textOutput("status"),
      h2('Plots:'),
      plotOutput("density"),
      plotOutput("wordcloud")
    )
  )
)

