### GR_EDA_SentimentAnalysis.R

### Setup and data preparation

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(magrittr)
library(textcat)
library(tidytext)
library(RTextTools)

setwd("C:/Chanel/NYU/Messy Data and Machine Learning, Section 001 Resources/project/project/What Happened")

review.data <- read.csv("Review_What Happened.csv", stringsAsFactors = FALSE)
review.data <- data.frame(review.data) #5130 obs
review.data <- review.data[,-1] 

### Data Cleaning ###

# Keep only reviews written in english
review.data$language <- as.factor(textcat(review.data$review))
ind1<-which(review.data$language == "english")
review.data <- review.data[ind1,] #4792 obs

# Keep only standard ratings that correspond to a 1 to 5 star ratings
unique(review.data$rating)
#Ratings such as "as to-read", "ey is currently reading it" and "as gave-up-on" are unrelevant to current analysis
ind2<-which(review.data$rating %in% c('did not like it',
                                      'it was ok',
                                      'liked it',
                                      'really liked it',
                                      'it was amazing'))
review.data <- review.data[ind2,] #4460 obs
sum(nchar(review.data$review) >= 30) #All reviews left have lengths of over 30 characters

#Recode ratings into numerical format of 1-5
review.data$rating[review.data$rating == 'did not like it'] <- 1
review.data$rating[review.data$rating == 'it was ok'      ] <- 2
review.data$rating[review.data$rating == 'liked it'       ] <- 3
review.data$rating[review.data$rating == 'really liked it'] <- 4
review.data$rating[review.data$rating == 'it was amazing' ] <- 5
review.data$rating <- as.integer(review.data$rating)

# Remove language and reviewer columns which will not be used anymore
review.data<-review.data[,-2]
review.data<-review.data[,-4]
#Add an id column
review.data$id <- 1:nrow(review.data)

### Exploratory Data Analysis ### 

# Draw bar chart of distribution of ratings
barplot(table(as.factor(review.data$rating)),
        ylim = c(0,2100), xlab="ratings",ylab="count",
        main = "Distribution of ratings",yaxt="n",col="#CCCCFF")
axis(2,at=seq(0,2100,300),labels=seq(0,2100,300))

# Or, using gglot:
tab<-as.data.frame(table(as.factor(review.data$rating)))
tab<-dplyr::rename(tab,"rating"="Var1","frequency"="Freq")
ggplot(tab,aes(x=rating,y=frequency,fill=rating))+ 
  geom_bar(stat="identity")+
  labs(y="count")

# Add a length column
review.data$length <- nchar(review.data$review)
# Use histgram to display distribution of lengths of reviews
hist(review.data$length, 
     ylim = c(0,2000), xlab="length",ylab="frquency",
     main = "Distribution of lengths of reviews",col="#CCCCFF")
sum(review.data$length>8000)#172 reviews contain more than 8000 characters, resulting in a skewed histogram

#Remove the 172 reviews since they might cause bias to the weight of certain reviews
review.data <- review.data[which(review.data$length <=8000),] #4288 obs left
hist(review.data$length, 
     ylim = c(0,1200),xlab="length",ylab="frequency",
     main = "Distribution of lengths of reviews",col="#CCCCFF")

# Or, using gglot:
ggplot(review.data, aes(length,fill=cut(length,30))) +
  geom_histogram()+
  labs(y="frequency")

# Use box plot to display review length by rating
with(review.data, boxplot(length~rating, xlab="rating",ylab="length",
                   main = "Review length by rating"))

# Or, using gglot:
ggplot(review.data, aes(rating, length, group = rating,fill=cut(rating,5))) +
  geom_boxplot()+
  scale_fill_discrete(h = c(100, 300))

### Use tidytext package to conduct sentiment analysis ###
View(sentiments)
# Subset the AFINN lexicon from the sentiment lexicon
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn.score = score)
head(AFINN)

# Subset the Bing lexicon from the sentiment lexicon
Bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(word, bing.sentiment = sentiment)
head(Bing)

# tokenize the review.data to contain one word per row and combine the data with the sentiment score lexicon 
review_words <- review.data %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words) %>% 
  select(-c(booktitle, length)) %>%
  left_join(AFINN, by = "word") %>%
  left_join(Bing, by = "word")

# Group review_words by id and rating to calculate average sentiment score, which reflects the overall sentiment of the review contents
average_sentiment_score <- review_words %>%
  group_by(id, rating) %>%
  summarize(average_score = mean(afinn.score, na.rm = TRUE))

# Merge average sentiment score with the review dataset by id
average_sentiment_score <- average_sentiment_score %>%
  select(-rating) %>%
  data.frame()
review_clean <- review.data%>%
  left_join(average_sentiment_score, by = "id")

# Calculate median sentiment score
median_sentiment_score <- review_words %>%
  group_by(id, rating) %>%
  summarize(median_score=median(afinn.score, na.rm = TRUE))

# Merge median sentiment score with the review_clean dataset by id 
median_sentiment_score <- median_sentiment_score %>%
  select(-rating) %>%
  data.frame()
review_clean <- review_clean %>%
  left_join(median_sentiment_score, by = "id")

# For each review, grouped by id and rating, count the number of negative words according to AFINN lexicon
count_afinn_negative <- review_words %>%
  filter(afinn.score < 0) %>%
  group_by(id, rating) %>%
  summarize(count_afinn_negative = n())

# Merge the result with the review dataset
count_afinn_negative <- count_afinn_negative %>%
  select(-rating) %>%
  data.frame()
review_clean <- review_clean %>%
  left_join(count_afinn_negative, by = "id")
review_clean$pro_afinn_negative<-100*review_clean$count_afinn_negative/review_clean$length

  # For each review, grouped by id and rating, count the number of positive words according to AFINN lexicon
count_afinn_positive <- review_words %>%
  filter(afinn.score > 0) %>%
  group_by(id, rating) %>%
  summarize(count_afinn_positive = n())

# Merge the result with the review dataset
count_afinn_positive <- count_afinn_positive %>%
  select(-rating) %>%
  data.frame()
review_clean <- review_clean %>%
  left_join(count_afinn_positive, by = "id")
review_clean$pro_afinn_positive<-100*review_clean$count_afinn_positive/review_clean$length

# For each review, grouped by id and rating, count the number of negative words according to BING lexicon
count_bing_negative <- review_words %>%
  filter(bing.sentiment == "negative") %>%
  group_by(id, rating) %>%
  summarize(count_bing_negative = n())

# Merge the result with the review dataset
count_bing_negative <- count_bing_negative %>%
  select(-rating) %>%
  data.frame()
review_clean <- review_clean %>%
  left_join(count_bing_negative, by = "id")
review_clean$pro_bing_negative<-100*review_clean$count_bing_negative/review_clean$length

# For each review, grouped by id and rating, count the number of positive words according to BING lexicon
count_bing_positive <- review_words %>%
  filter(bing.sentiment == "positive") %>%
  group_by(id, rating) %>%
  summarize(count_bing_positive = n())

# Merge the result with the review dataset
count_bing_positive <- count_bing_positive %>%
  select(-rating) %>%
  data.frame()
review_clean <- review_clean %>%
  left_join(count_bing_positive, by = "id")
review_clean$pro_bing_positive<-100*review_clean$count_bing_positive/review_clean$length

# Output the cleaned review dataset 
write.csv(review_clean, "Review_What Happened_clean.csv", row.names = FALSE)

# For now, concentrate on each word instead of each review. Aggregate the data by word. 
# For each word, calculate the number of reviews in which it appears, and store the results in a column named "count_review";
# For each word, calculate how many times it appears in overall, and store the results in a column named "count_overall".
# Only consider words that appear in more than one single review.
word_count <- review_words %>%
  count(id, rating, word) %>%
  group_by(word) %>%
  summarize(count_review = n(),
            count_overall = sum(n),
            average_rating = mean(rating)) %>%
  filter(count_review >= 2) %>%
  arrange(average_rating)

# Join word_count with AFINN lexicon
word_count <- word_count %>%
  inner_join(AFINN)
unique(word_count$afinn.score)
unique(review_words$afinn.score)
# Plot average rating of review with this word against AFINN score of word
ggplot(word_count, aes(afinn.score, average_rating, group = afinn.score,fill=cut(afinn.score,9))) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Average rating of review containing this word")+
  scale_fill_discrete(h = c(345, 32))
#From the plot, there is an overall pattern of positive correlation between AFINN score of word and average rating of review containing this word, though the pattern seems to be not quite significant. It remains uncertain whether these words can be used to predict ratings effectively.