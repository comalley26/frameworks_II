setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

# Frameworks II Association Rules HW Part I

library(tidyverse)
library(arules)
library(stringr)

data(Groceries)

dim(Groceries) # 9835 rows (transactions) 169 columns (items)

itemFrequencyPlot(Groceries, support = 0.02, cex.names=0.8, 
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))

# Yogurt and Whole Milk are in top 5 most bought items

crosstab <- crossTable(Groceries, measure="count", sort=TRUE)

crosstab[1:6, 1:6] # confirmed top 5 most bought items

rules1 <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.01))

summary(rules1) # 610 rules, max lift is 3.3723

rules2 <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.001))

summary(rules2) # 41100 rules

x1 <- inspect(rules1)
x1 <- x1[x1$count!=0,]
x1 <- x1[x1$lhs!="{}",]
x1 <- x1[x1$rhs!="{}",]
comma_index <- str_detect(x1$lhs, ",")
x1 <- x1[!comma_index, ]

summary(x1); dim(x1) # 426 rows
map(x1, class)

x1[order(x1$confidence, decreasing = T),] # butter -> whole milk is highest

soda_index <- str_detect(x1$lhs, "soda")

x1[soda_index, -2] # support for whole milk is 0.04006101

yogurt_index <- str_detect(x1$lhs, "yogurt")

x1[yogurt_index, -2] 

x1[x1$lhs == "{yogurt}", -2] %>% arrange(desc(support)) # whole milk is highest

# Part II

product_ratings <- read.csv("product_ratings_data.csv")

dim(product_ratings); head(product_ratings)

library(recommenderlab)

data_matrix <- as(product_ratings, Class = 'realRatingMatrix')
as(data_matrix, "matrix")

dim(data_matrix) # 5000 rows 100 columns
colMeans(data_matrix)
rowMeans(data_matrix)

product_ratings %>%
  filter(UserID == "u10023", Product == "prod_14") # 4  

set.seed(1031)
split = sample(nrow(data_matrix),size = 0.9*nrow(data_matrix))
train = data_matrix[split,]
test = data_matrix[-split,]

dim(train) # 4500 rows 100 columns
dim(test) # 500 rows 100 columns

train["u20150",] # 44 ratings

train[,"prod_25"] # 3761 ratings

getRatings(train) %>% table() # most ratings are 3
  
mean(getRatings(train[,'prod_100'])) # 2.824095

norm_train <- normalize(train)

mean(getRatings(norm_train[,'prod_100'])) # 0.08174056

similarity(norm_train[0:5],method = 'cosine')
similarity(normalize(train)[1:5,], normalize(train)[1:5,],method='cosine', which='users')

# Part III

recommenderRegistry$get_entries(data='realRatingMatrix')$UBCF_realRatingMatrix

recom_ubcf = Recommender(train, method='UBCF')

pred_ubcf_topn = predict(recom_ubcf,newdata=test,type='topNList',n=5)

getList(pred_ubcf_topn)['u10088'] # products 35, 56, 31, 14, 6

pred_ubcf_ratings = predict(recom_ubcf,newdata=test,type='ratings')

getList(pred_ubcf_ratings)['u10088'] # prod_1 is 2.844956

# see parameters for IBCF
recommenderRegistry$get_entries(data='realRatingMatrix')$IBCF_realRatingMatrix 

recom_ibcf = Recommender(train, method='IBCF')

pred_ibcf_topn = predict(recom_ibcf,newdata=test,type='topNList',n=5)

getList(pred_ibcf_topn)['u10088'] # products 30, 51, 52, 70, 3

pred_ibcf_ratings = predict(recom_ibcf,newdata=test,type='ratings')

getList(pred_ibcf_ratings)['u10088'] # prod_1 is 2.794343

set.seed(1031)

es <- evaluationScheme(data_matrix,method='split',train = 0.8, given=30)

recom <- Recommender(getData(es,'train'),method='IBCF')

pred_ibcf <- predict(recom,newdata = getData(es,'known'),type='ratings')

accuracy_ibcf <- calcPredictionAccuracy(x = pred_ibcf,data = getData(es,'unknown'))

accuracy_ibcf # RMSE is 1.310662

recom2 <- Recommender(getData(es,'train'),method='UBCF')

pred_ubcf <- predict(recom2,newdata = getData(es,'known'),type='ratings')

accuracy_ubcf <- calcPredictionAccuracy(x = pred_ubcf,data = getData(es,'unknown'))

accuracy_ubcf # RMSE is 1.184539

recom3 <- Recommender(getData(es,'train'),method='UBCF', parameter=list(nn=100))

pred_ubcf2 <- predict(recom3,newdata = getData(es,'known'),type='ratings')

accuracy_ubcf2 <- calcPredictionAccuracy(x = pred_ubcf2,data = getData(es,'unknown'))

accuracy_ubcf2 # RMSE is 1.1698414

recom_pop = Recommender(getData(es,'train'), method='POPULAR')
recom_pop

pred_pop <- predict(recom_pop,newdata = getData(es,'known'),type='ratings')

accuracy_pop <- calcPredictionAccuracy(x = pred_pop,data = getData(es,'unknown'))

accuracy_pop # RMSE is 1.171067
