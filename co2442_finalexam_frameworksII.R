setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

# Frameworks II Final Exam

# Section I - Clustering

data <- read.csv("fastfood_survey_Exam.csv")

names(data)
summary(data) # No NA values

sum(is.na(data)) # confirmed no NA's

data_scaled <- scale(data)

data_scaled[1,1] # 0.7041814

data_scaled %>%
  as.data.frame() %>%
  select(speed_of_service) %>%
  head(1) # confirmed

dist_cluster <- dist(data_scaled)

length(dist_cluster) # 193131

ward_hclust <- hclust(dist_cluster, method = "ward.D2")

# Visualize to find number of clusters

plot(cut(as.dendrogram(ward_hclust),h=5)$upper)

plot(cut(as.dendrogram(ward_hclust),h=5)$upper)
rect.hclust(tree=ward_hclust,k = 2,border='tomato')

plot(cut(as.dendrogram(ward_hclust),h=5)$upper)
rect.hclust(tree=ward_hclust,k = 3,border='tomato')

library(factoextra)
library(gridExtra)

grid.arrange(fviz_dend(x = ward_hclust,k=2),
             fviz_dend(x = ward_hclust,k=3),
             fviz_dend(x = ward_hclust,k=4))

# Looks like 3 cluster solution (could be 4)

# Cophenetic correlation

cor(cophenetic(ward_hclust),dist_cluster) # 0.7974118

# 3 cluster solution

hcluster_3 <- cutree(ward_hclust, k = 3)

table(hcluster_3) # smallest cluster has 37

data_3clusters <- cbind(data, cluster = hcluster_3)
data_3clusters %>% names()

round(prop.table(table(data_3clusters$cluster,data_3clusters$speed_of_service),1),2)*100
round(prop.table(table(data_3clusters$cluster,data_3clusters$variety),1),2)*100
round(prop.table(table(data_3clusters$cluster,data_3clusters$popularity_with_children),1),2)*100 # this is it chief
round(prop.table(table(data_3clusters$cluster,data_3clusters$cleanliness),1),2)*100
round(prop.table(table(data_3clusters$cluster,data_3clusters$convenience),1),2)*100
round(prop.table(table(data_3clusters$cluster,data_3clusters$taste),1),2)*100


# Section II - Recommender

library(recommenderlab)
data(MovieLense)

dim(MovieLense)
summary(MovieLense)

# How many movies did user 12 rate?

getRatingMatrix(MovieLense)[1:5,1:5]

nratings(MovieLense[12,]) # 51 ratings
sum(!is.na(as(MovieLense,'matrix')[12,])) # confirmed

# What was the mean rating of movies by user 12?

mean(getRatings(MovieLense[12])) # 4.392157
mean(as(MovieLense,'matrix')[12,],na.rm=T) # confirmed

# Split into train and test
set.seed(1706)
split = sample(nrow(MovieLense),size = 0.8*nrow(MovieLense))
train = MovieLense[split,]
test = MovieLense[-split,]

dim(train) # 754 rows

# UBCF

recommenderRegistry$get_entries(data='realRatingMatrix')$UBCF_realRatingMatrix # settings already okay

recom_ubcf = Recommender(train, method='UBCF')

pred_ubcf_topn = predict(recom_ubcf,newdata=test,type='topNList',n=3)

getList(pred_ubcf_topn)['12'] # Titanic, The Full Monty, LA Confidential

# Popular recommender

recom_pop = Recommender(train, method='POPULAR')

pred_pop <- predict(recom_pop,newdata = test, type='topNList', n = 3)

getList(pred_pop)['12'] # Fargo, Titanic, Shawshank Redemption

# Section III - Time Series

library(fpp2)
data(ausbeer)

summary(ausbeer)
dim(ausbeer)

ausbeer%>% View()

# make ts object and split into train and test

aus <- ts(ausbeer,start=c(1956,01),frequency=4)
train <- window(aus,start=c(1956,01),end=c(2005,4))
test <- window(aus,start=c(2006,01),end=c(2010,2))

length(test) # 18 quarters in test data

avg_model <- meanf(train, h = 18)

avg_model$fitted # mean value for train predictions is 414.69

predict(avg_model, test) # 414.69 is point forecast for test as well

accuracy(avg_model, x = aus) 
# test RMSE is 88.73987
# test RMSE is 37.73400

# Auto ARIMA

auto_arima_model <- auto.arima(train)

summary(auto_arima_model) # (1, 1, 2)(0, 1, 1)[4]

forecast(auto_arima_model, h = 18) # Q1 2008 is 421.7143

accuracy(forecast(auto_arima_model, h = 18), aus) # test RMSE is 8.476387

# Neural Net section does not require R code
