library(data.table)   # rbindlist
library(dplyr)        # pipes %>% and table manipulation commands
library(magrittr)     # pipes %>%
library(rvest)        # read_html
library(RSelenium)    # webscraping with javascript

setwd("C:/Chanel/NYU/Messy Data and Machine Learning, Section 001 Resources/project/project/")

url <- "https://www.goodreads.com/book/show/34114362-what-happened"
book.title <- "What Happened"
output.filename <- "Review_What Happened.csv"

remDr$close()
rm(rD)
gc()

#Open firefox browser
rD <- rsDriver(browser="firefox",verbose = FALSE)
rD
remDr <- rD$client
#Navigate to the book website
remDr$navigate(url) 
remDr$getTitle() #"What Happened by Hillary Rodham Clintond"

#Create dataframe to store book review information
df <- data.frame(booktitle = character(),
                 reviewer = character(),
                 rating = character(),
                 review = character(), 
                 stringsAsFactors = F)

# Webscraping of book reviews for What Happened
# Use a for loop to go through the 100 pages of comments on the website 
for(t in 1:171){
  
  #Identify location of reviews and extract reviews
  reviews <- remDr$findElements("css selector", "#bookReviews .stacked")
  reviews_html <- lapply(reviews, function(x){x$getElementAttribute("outerHTML")[[1]]})
  reviews_list <- lapply(reviews_html, function(x){read_html(x) %>% html_text()} )
  reviews_text <- unlist(reviews_list)
  
  # Clean the reviews with regularization expressions
  reviews_text2 <- gsub("[^A-Za-z\\-]|\\.+"," ",reviews_text) # Only keep characters that are letters, dash or periods
  reviews_clean <- gsub("\n|[ \t]+"," ",reviews_text2)  # Removing extra spaces and line end characters
  
  n <- floor(length(reviews)/2)
  reviews_df <- data.frame(booktitle = character(n), 
                           reviewer = character(n), 
                           rating = character(n), 
                           review = character(n), 
                           stringsAsFactors = F)
  
  # Fill in the data frame with information extracted
  for(j in 1:n){
    reviews_df$booktitle[j] <- book.title
    
    #Extracting name of the reviewer
    reviewer.sep <- regexpr(" rated it | marked it | added it ", reviews_clean[2*j-1])
    reviews_df$reviewer[j] <- substr(reviews_clean[2*j-1], 5, reviewer.sep-1)
    
    #Extracting ratings
    rating.end <- regexpr("·| Shelves| Recommend| review of another edition", reviews_clean[2*j-1])
    if (rating.end==-1){rating.end=nchar(reviews_clean[2*j-1])}
    reviews_df$rating[j] <- substr(reviews_clean[2*j-1], reviewer.sep+10, rating.end-1)
    
    #Removing the beginning of each review that was repeated on the html file
    short.str <- substr(reviews_clean[2*j], 1, 50)
    review.start <- unlist(gregexpr(short.str, reviews_clean[2*j]))[2]
    if (is.na(review.start)){review.start <- 1}
    review.end <- regexpr("\\.+more|Blog", reviews_clean[2*j])
    if (review.end==-1){review.end <- nchar(reviews_clean[2*j])}
    reviews_df$review[j] <- substr(reviews_clean[2*j], review.start, review.end-1)
  }
  
  review_lst <- list(df, reviews_df)
  df <- rbindlist(review_lst)
  
  NextPageButton <- remDr$findElement("css selector", ".next_page")
  NextPageButton$clickElement()
  Sys.sleep(3)
}   
#end of loop

write.csv(df, "Review_What Happened.csv")