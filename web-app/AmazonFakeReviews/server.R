library(shiny)
library(dplyr)
library(stringr)
library(rvest)
library(purrr)
library(keras)
library(ade4)
library(ggplot2)
library(rsconnect)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringdist)
library(tm)
source("helpers.R")
categories <- read.csv('data/categories.csv')
nn.model <- load_model_hdf5('data/fakereview.hd5')
terms <- read.csv('data/terms.csv')

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$writeupButton,{
      session$sendCustomMessage("mymessage", "mymessage")
  })
  
  observeEvent(input$urlButton, {
    req(input$product)
    productid <- input$product
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Scraping Reviews...", value = .25)
    reviews.scraped <- scrape_100_reviews(productid)
    progress$inc(.75, message = "Runnning reviews through classifier...")
    results <- runModel(reviews.scraped)
    output$status <- renderText({
      fake.count <- sum(results$y >=.5) / length(results$y) * 100
      
      if(fake.count > 0){
        paste("My model classified ", fake.count, "% of the sampled reviews as fake. Be careful when purchasing this product.", sep = "")
      } else {
        paste("No fake reviews detected. The reviews sampled for this product appear to be real and legitimate")
      }
    })
    
    output$density <- renderPlot({
      getDensityPlot(results)
    })
    
    output$wordcloud <- renderPlot({
      getWordCloud(reviews.scraped)
    })
    
  })
}


