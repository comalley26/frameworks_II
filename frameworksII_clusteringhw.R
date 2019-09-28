setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

# Frameworks II Clustering Assignment Part I

library(tidyverse)

data <- read_csv("fastfood_survey.csv")

dim(data) # 622 rows, 21 columns

summary(data)

data_cluster <- data[, 1:11]

dim(data_cluster)

sum(is.na(data_cluster$cleanliness)) #23 NA's

na.omit(data_cluster) %>% dim() # 556 rows left after NA's omitted

library(mice)
set.seed(1706)
data_cluster <- complete(mice(data_cluster))

data_cluster$cleanliness[10] # 1

data_cluster <- scale(data_cluster)

data_cluster[10, 4] # -3.87963

# Clustering Assignment Part II

dist_cluster <- dist(data_cluster, method = "euclidian")

length(dist_cluster) # 193131

ward_hclust <- hclust(dist_cluster, method = "ward.D2")
summary(ward_hclust)

plot(ward_hclust)

cor(cophenetic(ward_hclust), dist_cluster) # 0.810743

rect.hclust(tree=ward_hclust,k = 2,border='tomato')

library(factoextra)
library(gridExtra)

fviz_dend(x = ward_hclust, k = 4)

grid.arrange(fviz_dend(x = ward_hclust,k=2),
             fviz_dend(x = ward_hclust,k=3),
             fviz_dend(x = ward_hclust,k=4),
             fviz_dend(x = ward_hclust,k=5),
             fviz_dend(x = ward_hclust,k=6))

cluster.2 <- cutree(ward_hclust, k = 2)

table(cluster.2) # 41 in cluster 2
sum(cluster.2 == 1); sum(cluster.2 == 2) # 41 in cluster 2

cluster.3 <- cutree(ward_hclust, k = 3)

table(cluster.3) # 41 in cluster 3
sum(cluster.3 == 1); sum(cluster.3 == 2); sum(cluster.3 == 3) # 41 in cluster 3

set.seed(1706)
kclust.2 <- kmeans(data_cluster, centers = 2, iter.max = 100)

table(kclust.2$cluster) # 43 in cluster 1

kclust.3 <- kmeans(data_cluster, centers = 3, iter.max = 100)

table(kclust.3$cluster) # 39 in cluster 3

within_ss <- sapply(2:10,FUN = function(x) kmeans(x = data_cluster,
                                                 centers = x,
                                                 iter.max = 100)$tot.withinss)
within_ss # 3rd value is 3732.962 - WRONG
kclust.3$tot.withinss # 3732.962 confirmed - WRONG

sum(kclust.3$betweenss) / sum(kclust.3$totss) # 0.4535263

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1)) # 2 clusters, maybe 3

library(cluster)

# Use map_dbl to run many models with varying value of k
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = data_cluster, k = k)
  model$silinfo$avg.width
})

sil_width # 0.5936349 for 2 cluster, 0.1739848 for 3 cluster

# Generate a data frame containing both k and sil_width
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

sil_df # 0.5936349 for 2 cluster, 0.1739848 for 3 cluster

# Plot the relationship between k and sil_width
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

library(mclust)

mclusters <- Mclust(data_cluster)

summary(mclusters) # 4 clusters

mclusters.2 <- Mclust(data_cluster, G = 2)

summary(mclusters.2) # 171 in second cluster

table(kclust.2$cluster, cluster.2)
sum(kclust.2$cluster != cluster.2) # 4 different values

table(kclust.2$cluster, mclusters.2$classification) 
sum(kclust.2$cluster != mclusters.2$classification) # 128 different values

# Part III

set.seed(1706)

cluster.pt3 <- kmeans(data_cluster, centers = 3, iter.max = 100)
cluster.pt3$cluster

data_pt3 <- cbind(as.data.frame(data_cluster), cluster = as.factor(cluster.pt3$cluster))

data_pt3 %>% 
  group_by(cluster) %>% 
  summarise_all(funs(mean(.))) %>%
  View() 
# Group 3 highest for everything except drive through
# Group 2 lowest for everything except drive through and popularity with children

ogdata_pt3 <- cbind(as.data.frame(data), 
                    cluster = as.factor(cluster.pt3$cluster))

ogdata_pt3[, 1:11] <- as.data.frame(data_cluster)

table(ogdata_pt3$cluster, ogdata_pt3$income)
round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$income),1),2)*100

round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$gender),1),2)*100
# cluster 3 has largest percentage of females

round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$marital_status),1),2)*100
# cluster 3 has smallest percentage of singles

round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$dollars_avg_meal),1),2)*100
# cluster 2 has highest average meal cost

round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$number_children),1),2)*100

ogdata_pt3 %>%
  group_by(cluster) %>%
  summarize(avg_num_kids = mean(as.numeric(number_children), na.rm = T)) 
# cluster 3 has most kids

round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$own_rent),1),2)*100
# cluster 1 rents most / has least ownership

round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$dwelling),1),2)*100
# cluster 2 has most people living in house, cluster 1 has most people in apt

round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$occupation),1),2)*100
# cluster 2 has highest percentage of professionals
# cluster 3 has most stay at home moms

round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$education),1),2)*100
# cluster 3 has least education

round(prop.table(table(ogdata_pt3$cluster,ogdata_pt3$age),1),2)*100
# cluster 1 is the youngest