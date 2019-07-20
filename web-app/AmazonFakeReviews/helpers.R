# inspired by https://justrthings.com/2019/03/03/web-scraping-amazon-reviews-march-2019/

categories <- read.csv('data/categories.csv')
nn.model <- load_model_hdf5('data/fakereview.hd5')
terms <- read.csv('data/terms.csv')

scrape_100_reviews <- function(prod_code) {
  # Install / Load relevant packages
  
  PRODUCT_CATEGORY <- getProductCategory(prod_code)
  
  RATING <- numeric()
  texts <- character()
  VERIFIED_PURCHASE <- numeric()
  
  for(counter in 1:10) {
    # obtain HTML of URL
    doc <- read_html(paste0(paste0("http://www.amazon.com/product-reviews/", paste0(prod_code, "?pageNumber=")), counter))
    # Parse relevant elements from HTML
    reviews <- doc %>%
      html_nodes("#cm_cr-review_list .celwidget")
    
    # drop last node as it is empty
    reviews <- reviews[-length(reviews)]

    for(review in reviews) {
      rating <- review %>%
        html_nodes(".review-rating") %>%
        html_text() %>%
        str_extract("\\d") %>%
        as.numeric() 
      
      RATING <- c(RATING, rating)
      
      text <- review %>%
        html_nodes(".review-text") %>%
        html_text() %>% as.character()
      
      texts <- c(texts, text)
      
      verified <- review %>%
        html_node(".a-color-state") %>%
        html_text()
      
      verified <- verified == "Verified Purchase"
      
      VERIFIED_PURCHASE <- c(VERIFIED_PURCHASE, verified)
    }
  }
  scraped.df <- data.frame(PRODUCT_CATEGORY, RATING, texts, VERIFIED_PURCHASE, stringsAsFactors = FALSE)
  return(scraped.df)
}

getProductCategory <- function(prod_code) {
  category <- read_html(paste0("https://www.amazon.com/dp/", prod_code)) %>%
    html_node(".a-size-small li:nth-child(1)") %>% html_text(trim = TRUE)
  
  category <- strsplit(category, " ")[[1]][1]
  matches <- stringdist(tolower(category), tolower(categories$Categories), method='lv')
  category <- as.character(categories$Categories[which.min(matches)])
  
  return(category)
}

runModel <- function(reviews) {
  review.corpus <- VCorpus(VectorSource(reviews$texts)) %>% tm_map(removeWords, stopwords()) %>% tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>% tm_map(content_transformer(tolower)) %>% tm_map(stemDocument, lazy = TRUE)
  
  dtm <- DocumentTermMatrix(review.corpus, control = list(dictionary = terms$terms))
  tdm <- TermDocumentMatrix(review.corpus)
  
  reviews.merged <- data.frame(reviews, as.matrix(dtm))
  reviews.merged$texts <- NULL
  
  for(cat in categories$Categories) {
    name <- paste0("PRODUCT_CATEGORY.", cat)
    if (cat == reviews.merged$PRODUCT_CATEGORY[1]) {
      reviews.merged[[name]] <- 1
    } else {
      reviews.merged[[name]] <- 0
    }
  }
  
  reviews.merged$PRODUCT_CATEGORY <- NULL
  predictions <- data.frame(y = predict(nn.model, as.matrix(reviews.merged)))
  pred.df <- data.frame(predictions, reviews.merged)
  return(pred.df)
}

getDensityPlot <- function(results) {
  g <- ggplot(results, aes(y)) + geom_density(aes(fill=factor(RATING)), alpha=0.8) + 
    labs(title="Probability of Fake Review by Review Rating", 
         x="Probability of Fake Review",
         fill="Rating")
  return(g)
}

getWordCloud <- function(reviews) {
  review.corpus <- VCorpus(VectorSource(reviews$texts)) %>% tm_map(removeWords, stopwords()) %>% tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>% tm_map(content_transformer(tolower))
  
  tdm <- TermDocumentMatrix(review.corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  wordloud <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  return(wordloud)
}



# yo <- scrape_100_reviews("B01BV1XB2K")
# getWordCloud(yo)
# results <- runModel(yo)
# getDensityPlot(results)
# sum(results$y < .5)
# 
# wordcloud::wordcloud()
