### Preparation ###
library(data.table)
library(dplyr)
library(tidyr)
library(tidytext)
library(tm)
library(caret)
library(RTextTools)
library(ROCR)
library(randomForest)

setwd("C:/Chanel/NYU/Messy Data and Machine Learning, Section 001 Resources/project/project/What Happened/")
review_clean <- read.csv("Review_What Happened_clean.csv", stringsAsFactors = FALSE)

set.seed(1000)
# Re-classify 5 categories of ratings to create binary outcomes-positive reviews and negative/neutral reviews
review_clean$positive_review <- 0
review_clean$positive_review[review_clean$rating == 3 | review_clean$rating == 4 | review_clean$rating == 5] <- 1

### Construct the Document-Term Matrix for Unigrams ###
# Tokenize and remove stop words
review.words <- review_clean %>% unnest_tokens(word, review)
review.words <- review.words %>% anti_join(stop_words)

# Saving top 100 most common words
all_count<-review.words %>% 
  count(word) %>% 
  arrange(desc(n)) 
top100_all<-all_count[1:100,]
top100_all <-as.vector(top100_all$word)

# Construct dtm 
all_dtm <- review.words %>% 
  count(id, word) %>%
  cast_dtm(id, word, n) %>% 
  tidy() %>% 
  rename("id" = document,
         "word" = term) %>% 
  mutate(id = as.integer(id)) %>% 
  filter(word %in% top100_all) %>% 
  spread(key = word, value = count) 
all_dtm <-all_dtm%>% left_join(review_clean,by="id")

# Change NA to 0
all_dtm <- all_dtm %>% replace(list = is.na(all_dtm), values = 0)

# Drop features that will not be used in the model(keep the 100 DTM elements and four proportions as selected features)
model_dtm <- all_dtm %>% select(-id,-booktitle,-rating,-review,-length,
                                -average_score,-median_score,
                                -count_afinn_negative,-count_afinn_positive,-count_bing_positive,-count_bing_negative)
model_dtm$positive_review<-as.factor(model_dtm$positive_review)

### Machine learning: Logistic Regression Using Document Frequencies of Unigrams as Features ###
# Randomly shuffling dtm and spliting into 75% train and 25% test 
shuff <- model_dtm %>% sample_frac(1,replace=F)
ind   <- 1:ceiling(nrow(shuff)*0.75)
train <- shuff[ind,]
test  <- shuff[-ind,]

# Fit logistic model on training set using document frequencies of the top 100 words as predictors
logit.fit <- glm(train$positive_review ~ ., 
                  data = train, family = binomial(link = "logit"))

# Obtain orders of coefficients
names <- rownames(coef(summary(logit.fit)))
coefs <- as.data.frame(round(coef(summary(logit.fit)),2), row.names = 1:length(names))
coefs$Variable <- names
coefs <- coefs[,c(5,1:4)]
coefs<-coefs %>% arrange(Estimate)

write.csv(coefs,file="Result of logistic regression for unigrams.csv")

# Make predictions on test set
test$logit.prob <- predict(logit.fit, newdata = test, type = "response")
logit.pred <- prediction(test$logit.prob, test$positive_review)

# Calculate AUC
logit.perf <- performance(logit.pred,"auc")
logit.auc<-100*logit.perf@y.values[[1]]
logit.auc
# 98.11903

# Confusion matrix
logit.table <- table(true =test$positive_review, 
                      pred = as.integer(test$logit.prob >= 0.50))
logit.table

# Accuracy
logit.acc <- sum(diag(logit.table)) / nrow(test)
logit.acc
# 0.9906542

### Construct the Document-Term Matrix for Bigrams ###
# Tokenize into bigrams
review.bigram <- review_clean %>% unnest_tokens(bigram, review,token = 'ngrams', n=2)

# Separate bigrams into word1 and word2
review.bigram <- review.bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filter bigrams that contain stop words in either of the two words
review.bigram <- review.bigram %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

# Unite sperated words into bigrams
review.bigram <- review.bigram %>%
  unite(bigram, word1, word2, sep = " ")

# Save top 100 most common bigrams
bigram_count <- review.bigram %>% 
  count(bigram)%>%
  arrange(desc(n))
top100_bigram <- bigram_count[1:100,]
top100_bigram <- as.vector(top100_bigram$bigram)

# Construct dtm 
bigram_dtm <- review.bigram %>% 
  count(id, bigram) %>%
  cast_dtm(id, bigram, n) %>% 
  tidy() %>% 
  rename("id" = document,
         "bigram" = term) %>% 
  mutate(id = as.integer(id)) %>% 
  filter(bigram %in% top100_bigram) %>% 
  spread(key = bigram, value = count) 
bigram_dtm <-bigram_dtm%>% left_join(review_clean,by="id")

# Change NA to 0
bigram_dtm <- bigram_dtm %>% replace(list = is.na(bigram_dtm), values = 0)

# Drop features that will not be used in the model
model2_dtm <- bigram_dtm %>% select(-id,-booktitle,-rating,-review,-length,
                                -average_score,-median_score,
                                -count_afinn_negative,-count_afinn_positive,-count_bing_positive,-count_bing_negative,
                                -pro_afinn_negative,-pro_afinn_positive,-pro_bing_positive,-pro_bing_negative)
model2_dtm$positive_review<-as.factor(model2_dtm$positive_review)

### Machine learning: Logistic Regression Using Document Frequencies of Bigrams as Features ###
# Randomly shuffling dtm and spliting into 75% train and 25% test 
shuff2 <- model2_dtm %>% sample_frac(1,replace=F)
ind2   <- 1:ceiling(nrow(shuff2)*0.75)
train2 <- shuff2[ind2,]
test2  <- shuff2[-ind2,]

# Fit logistic model on training set using DTM for the top 100 bigrams as predictors
logit2.fit <- glm(train2$positive_review ~ ., 
                 data = train2, family = binomial(link = "logit"))

# Obtain orders of coefficients
names2 <- rownames(coef(summary(logit2.fit)))
coefs2 <- as.data.frame(round(coef(summary(logit2.fit)),2), row.names = 1:length(names2))
coefs2$Variable <- names2
coefs2 <- coefs2[,c(5,1:4)]
coefs2<-coefs2 %>% arrange(Estimate)

write.csv(coefs2,file="Result of logistic regression for bigrams.csv")

# Make predictions on test set
test2$logit2.prob <- predict(logit2.fit, newdata = test2, type = "response")
logit2.pred <- prediction(test2$logit2.prob, test2$positive_review)

# Calculate AUC
logit2.perf <- performance(logit2.pred,"auc")
logit2.auc<-100*logit2.perf@y.values[[1]]
logit2.auc
100-logit2.auc
# 70.09671

# Confusion matrix
logit2.table <- table(true =test2$positive_review, 
                     pred = as.integer(test2$logit2.prob >= 0.50))
logit2.table

# Accuracy
logit2.acc <- sum(diag(logit2.table)) / nrow(test2)
logit2.acc
# 0.9811543

### Machine learning: Random Forest Model Using Document Frequencies of Unigrams as Features ###
# Fit a random forest model on training set
rf.fit <- randomForest(train$positive_review~.,
                       data=train,ntree=200,na.action=na.exclude,importance=T)

# Feature importance plot
varImpPlot(rf.fit,type=2)

# Make predictions on test set
test$rf.prob <- predict(rf.fit, newdata = test, type = "response")
rf.pred <- prediction(as.numeric(test$rf.prob), as.numeric(test$positive_review))

#Calculate AUC
rf.perf <- performance(rf.pred,"auc")
rf.auc<-100*rf.perf@y.values[[1]]
rf.auc
# 96.875

# Confusion matrix
rf.table <- table(true =test$positive_review, 
                     pred = test$rf.prob)
rf.table

#Accuracy
rf.acc <- sum(diag(rf.table)) / nrow(test)
rf.acc
# 0.9925234

### Machine learning: Random Forest Model Using Document Frequencies of Bigrams as Features ###
# Replacing spaces in colnames to reduce errors when fitting random forest model
colnames(train2)<-gsub("\\s+","_",colnames(train2))
colnames(test2)<-gsub("\\s+","_",colnames(test2))

# Fit a random forest model on training set using document frequencies of bigrams as features
rf.fit2 <- randomForest(train2$positive_review~.,
                       data=train2,ntree=200,na.action=na.exclude)

# Feature importance plot
varImpPlot(rf.fit2,type=2,n.var=20)

# Make predictions on test set
test2$rf.prob2 <- predict(rf.fit2, newdata = test2, type = "response")
rf.pred2 <- prediction(as.numeric(test2$rf.prob2), as.numeric(test2$positive_review))

#Calculate AUC
rf.perf2 <- performance(rf.pred2,"auc")
rf.auc2<-100*rf.perf2@y.values[[1]]
rf.auc2
# 50

# Confusion matrix
rf.table2 <- table(true =test2$positive_review, 
                  pred = test2$rf.prob2)
rf.table2

#Accuracy
rf.acc <- sum(diag(rf.table)) / nrow(test)
rf.acc
# 0.9925234

sum(review_clean$positive_review==0)
# 524
sum(bigram_dtm$positive_review==1)
# 23

