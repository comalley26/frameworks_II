setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

# Clustering Review for Final Exam

# Load data
wine <- read.table('wine.csv', header=TRUE, sep=';')

str(wine)

library(caret)
library(broom)

# Split into train (70% of data) and test (30% of data) 
set.seed(1706)

split = createDataPartition(y=wine$quality,p = 0.7,list = F,groups = 100)
train = wine[split,]
test = wine[-split,]

# linear model

linear <- lm(quality ~ ., data = train)

summary(linear) %>% tidy()

# Sum of Squared Errors
sseLinear <- sum(linear$residuals^2)
sseLinear # 1937.101

# Apply to test data
predLinear <- predict(linear,newdata=test)
sseLinear <- sum((predLinear-test$quality)^2)

# Time to cluster
# Remove the outcome variable
trainMinusDV <- subset(train,select=-c(quality))
testMinusDV <- subset(test,select=-c(quality))

# Normalize the data
preproc <- preProcess(trainMinusDV)

trainNorm <- predict(preproc,trainMinusDV)

testNorm <- predict(preproc,testMinusDV)

mean(trainNorm$chlorides); mean(testNorm$chlorides)

# Hierarchical clustering using Ward D2 method

distances <- dist(trainNorm,method = 'euclidean') # Distance method is Euclidean
clusters <- hclust(d = distances,method = 'ward.D2')

library(dendextend)

# Visualize

plot(color_branches(as.dendrogram(clusters),k = 2,groupLabels = F))

clusterGroups = cutree(clusters,k=2)

# To express the clusters on a scatterplot, we flatten the data with 
# eleven variables into 2 dimensions by conducting a factor analysis 
# with varimax rotation using the psych package.

# This is done because it is not possible to visualize 11-dimensional data.

library(psych)
temp = data.frame(cluster = factor(clusterGroups),
                  factor1 = fa(trainNorm,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(trainNorm,nfactors = 2,rotate = 'varimax')$scores[,2])

ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

# K-means clustering

km <- kmeans(x = trainNorm,centers = 2,iter.max=10000,nstart=100) #km$centers
mean(km$cluster==clusterGroups) # % match between results of hclust and kmeans

# Plotting total within sum of squares (elbow plot)

within_ss = sapply(1:10,FUN = function(x) 
  kmeans(x = trainNorm,centers = x,iter.max = 1000,nstart = 25)$tot.withinss)

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# Slope flattens at 2 (elbow suggests 2 clusters)

# Ratio plot

ratio_ss = sapply(1:10,FUN = function(x) {
  km = kmeans(x = trainNorm,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss})

ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# Silhouette Plot

library(cluster)

silhoette_width = sapply(2:10,FUN = function(x) 
  pam(x = trainNorm,k = x)$silinfo$avg.width)

ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

# The elbow plots support a two cluster solution while silhoette plot 
# recommends a four cluster solution. We are going to go with two clusters as 
# dividing the data into four groups would not leave enough data to fit a model.

# Applying data on testing set

library(flexclust)

km_kcca = as.kcca(km,trainNorm) # flexclust uses objects of the classes kcca
clusterTrain = predict(km_kcca)
clusterTest = predict(km_kcca,newdata=testNorm)

# Distribution of wines across train

table(clusterTrain)

# Distribution of wines across test

table(clusterTest)

# Split based on cluster membership

train1 = subset(train,clusterTrain==1)
train2 = subset(train,clusterTrain==2)
test1 = subset(test,clusterTest==1)
test2 = subset(test,clusterTest==2)

# Predict for each cluster, then combine results

lm1 = lm(quality~.,train1)
lm2 = lm(quality~.,train2)

pred1 = predict(lm1,newdata=test1)
pred2 = predict(lm2,newdata=test2)

sse1 = sum((test1$quality-pred1)^2); sse1

sse2 = sum((test2$quality-pred2)^2); sse2

predOverall = c(pred1,pred2)
qualityOverall = c(test1$quality,test2$quality)
sseOverall = sum((predOverall - qualityOverall)^2); sseOverall

# Compare results (model on clusters is lower)
paste('sse for model on entire data',sseLinear)
paste('sse for model on clusters',sseOverall)

# Predict using a tree model

library(rpart); library(rpart.plot)

tree = rpart(quality ~ ., train, minbucket=10)
predTree = predict(tree, newdata = test)
sseTree = sum((predTree - test$quality)^2); sseTree

# Cluster then predict using tree

tree1 = rpart(quality~.,train1,minbucket=10)
tree2 = rpart(quality~.,train2,minbucket=10)
pred1 = predict(tree1,newdata=test1)
pred2 = predict(tree2,newdata=test2)

sse1 = sum((test1$quality-pred1)^2); sse1
sse2 = sum((test2$quality-pred2)^2); sse2

predTreeCombine = c(pred1,pred2)
qualityOverall = c(test1$quality,test2$quality)
sseTreeCombine = sum((predTreeCombine - qualityOverall)^2); sseTreeCombine

# Compare results (Combined is lower)
paste('sse for model on entire data',sseTree)
paste('sse for model on clusters',sseTreeCombine)


## Clustering for Segmentation

data = read.csv(file = 'jpm_cluster.csv',stringsAsFactors = F)

# Naming columns based on survey. 
# Also, assigning value labels to categories of demographic variables.

names(data) = c('id','performance','fees_commissions','depth_of_products','ability_resolve_problems','online_services','choice_of_providers','quality_of_advice','knowledge_of_reps','rep_knowing_your_needs','professional_services','provider_knows_me','quality_of_service',
                'age','marital_status','education')
data$age = factor(data$age,labels = c('27-57','58-68','69-75','75+'))
data$marital_status = factor(data$marital_status,labels=c('married','not married'))
data$education = factor(data$education,labels=c('no college','some college','college graduate','some graduate school','masters degree','doctorate'))
str(data)

# columns 2 to 13 include variables for clustering

data_cluster = data[,2:13]
head(data_cluster[,1:4])

# Impute values and examine

library(mice)
set.seed(617)

data_cluster = complete(mice(data_cluster))

head(data_cluster[,1:4])

# For distance-based clustering methods, scale of the variable 
# (e.g., seconds vs minutes) affects the weight assigned to the variable. 
# Therefore, it is best to standardize variables.

data_cluster = scale(data_cluster)
head(data_cluster[,1:4])

# Multiple variables measuring the same dimension should be replaced by 
# the underlying factor or a representative variable.

# Distance between pairs of points is useful in identifying similarity 
# between a pair of points. 

d = dist(x = data_cluster,method = 'euclidean') 

# There are different approaches to grouping observations and 
# these are called clustering methods. These methods differ based on 
# whether they are based on distance (single, complete, linkage), 
# variance (ward.D2, ward.D) or centroid (median, centroid).

clusters = hclust(d = d,method='ward.D2')

# Create a dendrogram

plot(clusters)

# Cophenetic correlation coefficient (CPC) is a goodness of fit statistic 
# for hierarchical clustering which assesses how well a dendrogram 
# matches the true distance metric

# Less than 0.3 is weak, 0.3-0.7 is medium, 0.7 or higher is strong

cor(cophenetic(clusters),d)

# Only display the tree branches above a height of 5

plot(cut(as.dendrogram(clusters),h=5)$upper)

# Rectangles around two cluster solution

plot(clusters)
rect.hclust(tree=clusters,k = 2,border='tomato')

# 3 cluster visual

plot(clusters)
rect.hclust(tree=clusters,k = 3,border='tomato')

# 4 cluster visual

plot(clusters)
rect.hclust(tree=clusters,k = 4,border = 'tomato')

# 2 to 4 cluster visuals with dendextend package

library(dendextend)
plot(color_branches(as.dendrogram(clusters),k = 2,groupLabels = F))
plot(color_branches(as.dendrogram(clusters),k = 3,groupLabels = F))
plot(color_branches(as.dendrogram(clusters),k = 4,groupLabels = F))

# 2 to 4 clusters with fviz_dend

library(factoextra)
fviz_dend(x = clusters,k=2)
fviz_dend(x = clusters,k=3)
fviz_dend(x = clusters,k=4)

# grid.arrange to show all at once

library(gridExtra)
grid.arrange(fviz_dend(x = clusters,k=2),
             fviz_dend(x = clusters,k=3),
             fviz_dend(x = clusters,k=4))

# Selecting number of clusters

h_segments = cutree(tree = clusters,k=4)
table(h_segments)

# Varimax rotation

library(psych)
temp = data.frame(cluster = factor(h_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

# Another similar visual

clusplot(data_cluster,
         h_segments,
         color=T,shade=T,labels=4,lines=0,main='Hierarchical Cluster Plot')

# K-means clustering

# Start with 3 centers

set.seed(617)
km = kmeans(x = data_cluster,centers = 3,iter.max=10000,nstart=25)

# Number of observations

table(km$cluster)

# Total Sum of Squares = Between Sum of Squares + Total Within Sum of Squares

paste(km$totss,'=',km$betweenss,'+',km$tot.withinss,sep = ' ')

km$totss == km$betweenss + km$tot.withinss # True

# Interpret results with elbow plot (total within sum of squares)

within_ss = sapply(1:10,FUN = function(x) kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)$tot.withinss)

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# Ratio plot (ratio of between SS to total SS)

ratio_ss = sapply(1:10,FUN = function(x) {km = kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)
km$betweenss/km$totss} )

ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# Silhouette plot (reflects how well points fit in their respective clusters)

silhoette_width = sapply(2:10,FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)

ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

# Silhouette indicates 4 cluster solution (lowest point in graph)

km = kmeans(x = data_cluster,centers = 4,iter.max=10000,nstart=25)

# Look at cluster sizes

k_segments = km$cluster
table(k_segments)

# Varimax rotation for visual

temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

clusplot(data_cluster,
         k_segments,
         color=T,shade=T,labels=4,lines=0,main='k-means Cluster Plot')

# Model-based clustering

library(mclust)

clusters_mclust = Mclust(data_cluster)

# Optimal cluster solution performs best on BIC and Log Likelihood

summary(clusters_mclust)

clusters_mclust_3 = Mclust(data_cluster,G=3)
summary(clusters_mclust_3)

clusters_mclust_3$bic

# Plot of BIC

mclust_bic = -sapply(1:10,FUN = function(x) Mclust(data_cluster,G=x)$bic)
mclust_bic

ggplot(data=data.frame(cluster = 1:10,bic = mclust_bic),aes(x=cluster,y=bic))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# 6 is lowest but segments may not be practical 
# Go with 4 cluster solution like before

m_clusters = Mclust(data = data_cluster,G = 4)

# Look at clustering segments

m_segments = m_clusters$classification
table(m_segments)

# Varimax rotation

temp = data.frame(cluster = factor(m_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

clusplot(data_cluster,
         m_segments,
         color=T,shade=T,labels=4,lines=0,main='mclust Cluster Plot')

# Comparing clustering methods

table(h_segments)
table(k_segments)
table(m_segments)

# Combine with original data and visualize

data2 = cbind(data,h_segments, k_segments,m_segments)

data2 %>%
  select(performance:quality_of_service,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

data2 %>%
  select(performance:quality_of_service,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,performance:quality_of_service)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()

# Profile by segment

prop.table(table(data2$k_segments,data2[,14]),1)

# Heatmap

tab = prop.table(table(data2$k_segments,data2[,14]),1)
tab2 = data.frame(round(tab,2))

library(RColorBrewer)
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))

# Look at multiple variables at once

lapply(14:16,function(x) round(prop.table(table(data2$k_segments,data2[,x]),1),2)*100)

# Heatmaps of above crosstabulation 

lapply(14:16,function(x) {
  dat = round(prop.table(table(data2$k_segments,data2[,x]),1),2)*100
  dat = data.frame(dat)
  ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
    geom_tile()+
    geom_text(aes(label=Freq),size=6)+
    xlab(label = '')+
    ylab(label = '')+
    scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
})
