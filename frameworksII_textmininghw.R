setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

# Frameworks II Text Mining Assignment Part I

library(tidyverse)

baby_reviews <- read_csv("baby_reviews.csv")

dim(baby_reviews) # 4978 rows, 3 columns
head(baby_reviews)

mean(baby_reviews$review_rating) # 4.227601

mean(nchar(baby_reviews$review)) # 441.8248

plot(nchar(baby_reviews$review), baby_reviews$review_rating) # no strong relationships
cor(nchar(baby_reviews$review), baby_reviews$review_rating) # slightly negative

str_count(string = baby_reviews$review ,pattern = '\\S+') %>% median() # 57

str_count(string = baby_reviews$review ,pattern = '\\S+') %>% max() # 1041

str_count(string = baby_reviews$review ,pattern = '\\S+') %>% min() # 2

library(qdap)

freq_terms(text.var = baby_reviews$review, top = 10) 
# the, and

freq_terms(text.var = baby_reviews$review, top = 10, stopwords = Top200Words) 
# baby, easy, love

# Part II

library(tidytext)

baby_reviews %>%
  unnest_tokens(word, review) %>%
  nrow() # 421790 words (rows)

baby_reviews %>%
  unnest_tokens(word, review) %>%
  inner_join(get_sentiments("bing")) %>%
  filter(sentiment == "positive") %>%
  dim() # 24462 positive words

baby_reviews %>%
  unnest_tokens(word, review) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment) %>%
  summarize(n()) # 24462 positive, 8739 negative words

# 24462 + 8739 = 33201

24462/33201 # 0.736785

baby_reviews %>%
  unnest_tokens(word, review) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(review_rating) %>%
  summarize(pos = sum(sentiment == "positive"),
            neg = sum(sentiment == "negative"),
            prop = pos / neg) %>%
  arrange(prop) # 5 has highest proportion of positive words

baby_reviews %>%
  unnest_tokens(word, review) %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment == "surprise") %>%
  count() # 3815 surprise words

baby_reviews %>%
  unnest_tokens(word, review) %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment == "anticipation") %>%
  count() # 8429 anticipation words

baby_reviews %>%
  unnest_tokens(word, review) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarize(avg_score = mean(score)) %>%
  arrange(avg_score) %>% # lowest average scores are -3 for entire review
  filter(avg_score == -3) # ids are 91, 238, 2598 in quiz

baby_reviews %>%
  unnest_tokens(word, review) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarize(avg_score = mean(score)) %>% 
  summarize(mean(avg_score)) # 1.38 average score

# Part III

library(tm)

corpus <- Corpus(VectorSource(baby_reviews$review))

corpus <- tm_map(corpus, FUN = content_transformer(tolower))
corpus <- tm_map(corpus, FUN = removePunctuation)
corpus <- tm_map(corpus, FUN = removeWords, c(stopwords("english")))
corpus <- tm_map(corpus, FUN = stripWhitespace)

corpus
summary(corpus)

dict <- findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(baby_reviews$review))),
                      lowfreq = 0)

dict_corpus <- Corpus(VectorSource(dict))

corpus <- tm_map(corpus, FUN = stemDocument)

corpus[[100]][[1]] %>%
  str_count(pattern = "amazon") # mentioned 1 time

dtm <- DocumentTermMatrix(corpus)
dtm
dim(dtm) # 11730 terms

xdtm <- removeSparseTerms(dtm, sparse = .90)

dim(xdtm) # only 47 terms remaining

xdtm <- as.data.frame(as.matrix(xdtm))

colnames(xdtm) <- stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')

colnames(xdtm) <- make.names(colnames(xdtm))

sort(colSums(xdtm),decreasing = T) # use is the most prevalent word

xdtm <- xdtm %>%
  mutate(rating = baby_reviews$review_rating) %>%
  filter(rating == 5) 

sort(colSums(xdtm),decreasing = T) # love is third most prevalent word

# Part IV

dtm_tfidf = DocumentTermMatrix(x=corpus,
                               control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),dictionary = dict_corpus,type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)

baby_reviews <- cbind(review_rating = baby_reviews$review_rating,xdtm)
baby_reviews_tfidf <- cbind(review_rating = baby_reviews$review_rating,xdtm_tfidf)

set.seed(1031)
split <- sample(1:nrow(baby_reviews),size = 0.7*nrow(baby_reviews))
train <- baby_reviews[split,]
test <- baby_reviews[-split,]
summary(train)

nrow(test) # 1494 rows

library(rpart); library(rpart.plot)
tree <- rpart(review_rating ~ ., train)
rpart.plot(tree) 
# love leads to better review ratings
# easier and perfect do not lead to lower ratings (both lead to higher as well)

reg <- lm(review_rating ~ .,train)
summary(reg) # use (most common word) is not statistically significant

test_preds <- predict(tree, newdata = test)
error <- test_preds - test$review_rating
sqrt(mean(error^2)) # rmse is 1.058494

test_preds2 <- predict(reg, newdata = test)
error2 <- test_preds2 - test$review_rating
sqrt(mean(error2^2)) # rmse is 1.054743

