setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

# Recommender System Review for Final Exam

library(recommenderlab)

# Load jokes dataset (Jester5k)

data(Jester5k)

# Here is a joke

JesterJokes[[48]]

# Data is stored as real data matrix

Jester5k

# Jester5k is a sparse matrix with only 72% elements rated.

nratings(Jester5k)/(ncol(Jester5k)*nrow(Jester5k)) # 0.724212

# Which recommenderlab functions are available for real data matrices?

methods(class = 'realRatingMatrix')

# Which models are available?

names(recommenderRegistry$get_entries(dataType='realRatingMatrix'))

dim(Jester5k)
str(Jester5k)

# Implementation of image by recommenderlab generates a heatmap.

image(Jester5k[1:5,1:5])

# To view the raw data, we transform it to a matrix form.

as(Jester5k,'matrix')[1:5, 1:5]

# Another alternative for viewing raw data is to use the 
# getRatingMatrix from recommenderlab

getRatingMatrix(Jester5k)[1:5,1:5]

# Extract joke ratings from jokes 56 - 60 from u7676

as(Jester5k,'matrix')['u7676',c('j56','j57','j58','j59','j60')]

# Q: How many jokes did u7676 rate?
# A: Using recommenderlab functions

nratings(Jester5k['u7676',]) # 60

# A: Using traditional ways

sum(!is.na(as(Jester5k,'matrix')['u7676',])) # 60

# Q: What was the mean rating of jokes by u7676?
# A: Using recommenderlab functions

mean(getRatings(Jester5k['u7676']))

# A: Using traditional ways

mean(as(Jester5k,'matrix')['u7676',],na.rm=T)

# Tidy format

data.frame(as(Jester5k,'matrix'))%>%
  rownames_to_column(var = 'user')%>%
  gather(key = 'joke',value = rating,2:101)%>%
  head()

# Split dataset

set.seed(1706)
split = sample(x = nrow(Jester5k),size = 0.8*nrow(Jester5k))
train = Jester5k[split,]
test = Jester5k[-split,]

# Explore train

summary(rowCounts(train))

ggplot(data=data.frame(no_of_jokes_rated = rowCounts(train)),aes(x=no_of_jokes_rated))+
  geom_histogram(bins=50,fill='seagreen3')+ylab('Number of raters')+xlab('Number of Jokes rated')+
  scale_x_continuous(breaks = seq(0,100,20),limits=c(0,110))+
  scale_y_continuous(breaks=seq(0,1200,200),limits=c(0,1200))

# What can you say about the number of ratings each jokes received?

summary(colCounts(train))

data.frame(no_of_ratings=colCounts(train))%>%
  rownames_to_column()%>%
  ggplot(aes(x=as.numeric(gsub(rowname,pattern = '[a-z]',replacement = '')), y=no_of_ratings))+
  geom_col(fill='seagreen3')+xlab('Joke Id')+ylab('Number of ratings')+coord_flip()+ theme_bw()

# Average rating of first six jokes

head(colMeans(train))

# Distribution of average joke ratings

ggplot(data=data.frame(joke_ratings = colMeans(train)),aes(x=joke_ratings))+
  geom_histogram(fill='seagreen3')

ggplot(data=data.frame(joke_ratings = colMeans(train)),aes(x=joke_ratings))+
  geom_density(color='seagreen3', size=1.2)

# Average rating for first six users

head(rowMeans(train))

# All data

ggplot(data=data.frame(joke_ratings = rowMeans(train)),aes(x=joke_ratings))+
  geom_histogram(fill='seagreen3')

# Prepare data (normalize)

getRatings(train['u7676'])

# data before centering

summary(rowMeans(train))

# data after centering

getRatings(normalize(train, method='center')['u7676',])

ggplot(data=data.frame(joke_ratings = rowMeans(normalize(train))),aes(x=joke_ratings))+
  geom_histogram(fill='seagreen3')

# Rating of j50 for first 25 users before centering

getRatings(train[1:25,'j50'])

# After centering

getRatings(normalize(train)[1:25,'j50'])

# We generalize user data, not item data

summary(getRatings(normalize(train)[,'j50']))

# Finally, one could go one step ahead of centering by standardizing the data 
# (i.e., center and express in standard deviation units). 

# Data before standardizing

getRatings(train['u7676'])
summary(rowMeans(train))

# After standardizing

getRatings(normalize(train, method='Z-score')['u7676',])
summary(getRatings(normalize(train, method='Z-score')['u7676',]))


# Similarity function 

similarity(normalize(train)[1:5],method = 'euclidean') # Euclidean distance
similarity(normalize(train)[1:5],method = 'cosine') # Cosine similarity
similarity(normalize(train)[1:5],method = 'pearson') # Pearson correlation


# Construct recommendations
# Non-personalized recommendations: popular

recommenderRegistry$get_entries(dataType='realRatingMatrix')$POPULAR_realRatingMatrix

recom_popular = Recommender(train,method='POPULAR',parameter=list(normalize='center'))

# Top N recommendations

pred_popular_topN = predict(recom_popular,newdata=test,type='topNList',n=5)

getList(pred_popular_topN)['u1840'] # user 1840 only

getList(pred_popular_topN)[1:20] # first 10 users

# Sort by rating

sort(colMeans(normalize(test,method='center')),decreasing = T)

as(test,'matrix')['u2841',is.na(as(test,'matrix')['u2841',])] # jokes not rated by user 2841

# recommend jokes for user 2841

getList(pred_popular_topN)['u2841']

# generate ratings for each joke

pred_popular = predict(recom_popular,newdata=test,type='ratings')

as(test,'matrix')['u1840',]

# predicted ratings for jokes not yet rated for this user

as(pred_popular,'matrix')['u1840',]

# Ratings for jokes 51-60 by users 51-60 in the test

as(test,'matrix')[51:60,51:60]

# Predicted Ratings for jokes 51-60 by users 51-60 in the test

as(pred_popular,'matrix')[51:60,51:60]


# User-based collaborative filtering

recommenderRegistry$get_entries(data='realRatingMatrix')$UBCF_realRatingMatrix

recom_ubcf = Recommender(train, method='UBCF', parameter=list(method='cosine',nn=25, normalize='center'))

# Top N recommendations

pred_ubcf_topN = predict(recom_ubcf,newdata=test,method='topNList',n=5)

getList(pred_ubcf_topN)[1:5]

getList(pred_ubcf_topN)['u1840']

slotNames(pred_ubcf_topN)

pred_ubcf_topN@items['u1840']

# Ratings

pred_ubcf = predict(recom_ubcf,newdata=test,type='ratings')

as(test,'matrix')['u1840',]

# Recommendations for Jokes not rated

as(pred_ubcf,'matrix')['u1840',] 

# Ratings for jokes 51-60 by users 51-60 in the test

as(test,'matrix')[51:60,51:60]

# Predicted Ratings for jokes 51-60 by users 51-60 in the test

as(pred_ubcf,'matrix')[51:60,51:60]


# Item-based Collaborative Filtering

recommenderRegistry$get_entries(data='realRatingMatrix')$IBCF_realRatingMatrix # see parameters for IBCF

recom_ibcf = Recommender(train, method='IBCF', parameter=list(k=30, method='cosine',normalize='center'))
recom_ibcf

# Top N recommendations

pred_ibcf_topN = predict(recom_ibcf,newdata=test,method='topNList',n=5)

getList(pred_ibcf_topN)[1:5]

getList(pred_ibcf_topN)['u1840']

# Ratings

pred_ibcf = predict(recom_ibcf,newdata=test,type='ratings')

# Jokes rated

as(test,'matrix')['u1840',]

# Recommendations for jokes not yet rated by user

as(pred_ibcf,'matrix')['u1840',] 

as(test,'matrix')[51:60,51:60]

as(pred_ibcf,'matrix')[51:60,51:60]

# Evaluation scheme

min(rowCounts(Jester5k))

# 80% of data in train

es = evaluationScheme(Jester5k,method='split',train=0.8, given=30)

getData(es,'train')
getData(es,'known')
getData(es,'unknown')

# Total number of ratings = train + known + unknown

nratings(Jester5k) == nratings(getData(es,'train')) + 
                      nratings(getData(es,'known')) + 
                      nratings(getData(es,'unknown')) 

# Number of items used to generate recommendations; value of 'given'

rowCounts(getData(es,'known'))[1:20]

# Remaining items being held out for computing error.

rowCounts(getData(es,'unknown'))[1:20]  

# Build recommender for UBCF

recom_ubcf = Recommender(data = getData(es,'train'),
                         method='UBCF',
                         parameter = list(method='cosine',nn=25,normalize='center'))

# Generate recommendations

pred_ubcf = predict(recom_ubcf,newdata=getData(es,'known'), type='ratings')

# Evaluate 

calcPredictionAccuracy(pred_ubcf,data = getData(es,'unknown'))

# Next let us calculate prediction accuracy for an IBCF

recommenderRegistry$get_entries(data='realRatingMatrix')$IBCF_realRatingMatrix

recom_ibcf = Recommender(data = getData(es,'train'),
                         method='IBCF',
                         parameter = list(method='cosine',k=30,normalize='center'))

# Generate

pred_ibcf = predict(recom_ibcf,newdata=getData(es,'known'), type='ratings')

# Evaluate

calcPredictionAccuracy(pred_ibcf,data = getData(es,'unknown'))

# let us use k-fold cross-validation, first, to evaluate just UBCF

set.seed(1031)

es = evaluationScheme(Jester5k,method='cross-validation',k=10,given=30)

ev = evaluate(x = es,method='UBCF',parameter=list(method='cosine',nn=25), type='ratings')

avg(ev)

# Evaluate a set of recommenders

recommender_algorithms = list(random = list(name='RANDOM'),
                              popular = list(name='POPULAR'),
                              ubcf = list(name='UBCF'),
                              ubcf_50 = list(name='UBCF',parameters=list(nn=50)),
                              ubcf_100 = list(name='UBCF',parameters=list(nn=100)),
                              ibcf = list(name='IBCF'),
                              ibcf_10 = list(name='IBCF', parameters=list(k=10)))

ev = evaluate(x = es,method=recommender_algorithms, type='ratings')

results = matrix(unlist(avg(ev)),ncol=3)

colnames(results) = c('RMSE','MSE','MAE')

rownames(results) = c('random','popular','ubcf','ubcf_50','ubcf_100','ibcf','ibcf_10')

results

plot(ev)

