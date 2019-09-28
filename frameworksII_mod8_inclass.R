
setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

cat(paste(readLines('transactions.csv'),'\n',collapse=''))

library(arules); library(arulesViz)

items <- read.transactions('transactions.csv',format='basket',sep=',')
as(items,'matrix')

items
dim(items) # 9 transactions 5 items
summary(items)

itemFrequencyPlot(items, support = 0.0, cex.names=0.8, 
                  type = "relative", horiz = TRUE, col = "steelblue2", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item"))

# compute the frequency of occurence of various pairs of items
crossTable(items,measure='count',sort=T) 
crossTable(items,measure='support',sort=T) # prevalence of item set
crossTable(items,measure='lift',sort=T) # strength of association

rules_all <- apriori(items,parameter=list(support=0,confidence=0))

summary(rules_all)

x <- inspect(rules_all)

x = x[x$count!=0,]
x[order(x$lift,x$support, decreasing = T),]

# Only keeping rules with a support>0.001 and confidence>0.05
rules1 <- apriori(items,parameter = list(support = 0.001, confidence = 0.05))

summary(rules1)
inspect(rules1)

# Only keeping rules with a support>0.2 and confidence>0.2
rules2 <- apriori(items,parameter = list(support = 0.2, confidence = 0.2))

summary(rules2)
x <- inspect(rules2)

x[order(x$lift,x$support, decreasing = T),]

library(RColorBrewer)
plot(rules2) 

# defaults spelt out with a bit of color and jitter
plot(rules2,
     method='scatterplot',
     measure=c('support','confidence'),
     control=list(jitter=1, 
                  col = rev(brewer.pal(9, "Greens")[4:9])),
     shading = "lift")

# Lift on y-axis
plot(rules2,
     method='scatterplot',
     measure=c('support','lift'),
     shading='confidence')

# Rules by order
plot(rules2,method='scatterplot',measure=c('support','lift'),shading='order')

# Grouped matrix
plot(rules2, 
     method="grouped", 
     measure = 'support',
     shading='lift')

plot(rules_all[quality(rules_all)$support>0,], 
     method="matrix", 
     measure="lift", 
     control=list(reorder=TRUE))

# 3D matrix 
plot(rules_all[quality(rules_all)$support>0,], 
     method="matrix", 
     measure="lift", 
     control=list(reorder=TRUE), 
     engine='3d')

plot(rules2, method="graph",control=list(type="items"), shading = "lift")

rules_diapers = apriori(items,
                        parameter=list(support=0.001,confidence=0.05),
                        appearance = list(lhs='diapers'))

rules_diapers = subset(rules_diapers, subset= lift>1)
inspect(rules_diapers)

plot(rules_diapers,
     control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),
     shading = "lift")

library(readxl)
library(ggplot2)
library(dplyr)
retail <- read_excel('Online_retail.xlsx')
retail <- retail[complete.cases(retail), ]

library(dplyr)

str(retail)
glimpse(retail)

retail$Date = as.Date(retail$InvoiceDate)
retail$Description = as.factor(retail$Description)
baskets = retail %>%
  arrange(desc(CustomerID))%>%
  group_by(CustomerID,Date)%>%
  summarise(items = paste(Description,collapse=','))
baskets$CustomerID=NULL
baskets$Date=NULL
write.csv(baskets,'baskets.csv',quote = F,row.names = F)

items = read.transactions('baskets.csv', format = 'basket', sep=',',skip=1)
dim(items)

itemFrequencyPlot(items, support = 0.04, cex.names=0.8, 
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))








