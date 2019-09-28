setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

# PCA / Factor Analysis Review for Final Exam

# PCA

# Load wine data

wine = read.table('wine.csv',header=TRUE,sep=';')

str(wine)

library(caret)

# Split data

set.seed(1706)
split = createDataPartition(y=wine$quality,p = 0.7,list = F,groups = 100)
train_wine = wine[split,]
test_wine = wine[-split,]

# Drop quality (outcome variable) from dataset

train = train_wine[,1:11]
test = test_wine[,1:11]

# Correlations of variables

round(cor(train,use='complete.obs'), 3)

corMatrix = as.data.frame(cor(train))
corMatrix$var1 = rownames(corMatrix)

corMatrix %>%
  gather(key=var2,value=r,1:11)%>%
  ggplot(aes(x=var1,y=var2,fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradient2(low = 'red',high='green',mid = 'white')+
  theme(axis.text.x=element_text(angle=90))

# Create correlation plots

library(corrplot)
corrplot(cor(train),type = 'lower',col = c('red','white','green'),method = 'pie',diag = F,order='hclust')

library(ggcorrplot)
ggcorrplot(cor(train),colors = c('red','white','green'),hc.order = T,type = 'lower')

# Bartlett's Test of Sphericity
# Looks to see if there are at least some non-zero correlations by 
# comparing correlation matrix to an identity matrix

library(psych)
cortest.bartlett(cor(train),n = nrow(train))

# KMO Measure of Sampling Adequacy (MSA)
# Compares partial correlation matrix to pairwise correlation matrix. 

KMO(cor(train))

# Determine Number of Components
# A dataset with p variables will generate p components. 

# Our goal is to pick out the top few components that capture 
# most of the variance in the original data.


# Scree Plot
# Line graph of eigen values for each component. 
# Ideal number of components indicated by a slope change in the line graph (elbow)

library(FactoMineR)
pca_facto = PCA(train,graph = F)

library(factoextra)
fviz_eig(pca_facto,ncp=11,addlabels = T)

# Using prcomp for PCA

pca = prcomp(train,scale. = T)
fviz_eig(pca,ncp = 11,addlabels = T)

# Eigen values
# According to the eigen-value criterion, 
# all components with eigen value greater than 1 are selected.

pca_facto$eig # 11 components

pca_facto$eig[pca_facto$eig[,'eigenvalue']>1,] # 4 components where eigen > 1

data.frame(component = 1:length(pca$sdev), eigen_value = (pca$sdev)^2)

# Parallel Analysis
# Simulate a dataset with same variables and observations as original dataset.
# Compute correlation matrix and eigen values.

fa.parallel(train,fa='pc')

# Parallel analysis suggests that the number of factors =  NA  
# and the number of components =  3

# Total variance explained

pca_facto$eig

# The results from each method differ widely:
# Scree Plot: 2, 3, or 6 components
# Eigen Value: 4 components
# Parallel Analysis: 3 components

# However, the use of any fewer than 5 components would explain 
# less than 70% of the original data. 
# So, we go with a six-component structure suggested by the Scree plot.

pca_facto = PCA(train,scale.unit = T,ncp = 6,graph = F)

# Examining elements comprising each component. 
# For any component, size of loadings indicate the importance 
# of the variable in desribing the component. 

pca_facto$var$contrib %>%
  round(2)

# Contributions of each variable to each component are charted out below.

charts = lapply(1:6,FUN = function(x) fviz_contrib(pca_facto,choice = 'var',axes = x,title=paste('Dim',x)))
grid.arrange(grobs = charts,ncol=3,nrow=2)

# Next, let us visually examine the relationships between variables.

fviz_pca_var(X = pca_facto,col.var = 'contrib',gradient.cols = c('red'),col.circle = 'steelblue',repel = T)

# Apply component structure to test set with FactoMineR

trainComponents = pca_facto$ind$coord
testComponents = predict(pca_facto,newdata=test)$coord

trainComponents = cbind(trainComponents,quality = train_wine$quality)
testComponents = cbind(testComponents,quality = test_wine$quality)

# Do the same with a prcomp object

trainComponents2 = pca$x[,1:6]
trainComponents2 = cbind(trainComponents2,quality = train_wine$quality)

testComponents2 = predict(pca,newdata = test)[,1:6]
testComponents2 = cbind(testComponents2,quality = test_wine$quality)



## Factor Analysis

# Load toothpaste data

data = read.csv('toothpaste.csv')

survey = data[,2:7]
head(survey)
summary(survey)

# Examine bivariate correlations

round(cor(survey), 3)

corrplot(cor(data[,2:7]),type = 'lower',,col = c('red','white','green'),method = 'square',diag = F)

ggcorrplot(cor(data[,2:7]),colors = c('red','white','green'),type = 'lower')

# Bartlett's Test of Sphericity

cortest.bartlett(cor(survey),n = 30)

# KMO Measure of Sampling Adequacy (MSA)

KMO(r = cor(survey))

# Determine number of factors with scree plot

scree(cor(survey),factors = T,pc = F) # 2 factors

# Eigen values

data.frame(factor = 1:ncol(survey), eigen = scree(cor(survey),factors = T,pc = F)$fv)

# Parallel analysis

fa.parallel(survey,fa='fa',fm = 'pa')

# Total variance explained (should be greater than 70%)

result = fa(r = survey,nfactors = 2,fm = 'pa',rotate = 'none')
result$Vaccounted

# Extracted communalities

data.frame(communality = result$communality)

# Mapping variables to factors

# Each variable is represented as a linear combination of factors. 
# An ideal factor solution is where each variable is expected to load on 
# (i.e., related to) only one factor. Such a result is easy to interpret.

# In practice, each variable may load on many factors. 
# This may still be acceptable so long as the loading on 
# one factor is large and on all other factors is small.

# When the pattern of loadings does not show a clear preference of a variable 
# for a factor, rotating the axes may help generate a clear mapping. 

# There are two types of rotation: Orthogonal (right angels and Oblique (any angle)

print(result$loadings, cut=0) # no rotations applied

# Excluding small loadings

print(result$loadings, cut=0.15) # 4 variables load on both factors

# Varimax rotation (orthogonal)

fa_varimax = fa(r = survey,nfactors = 2,fm = 'pa',rotate = 'varimax')
print(fa_varimax$loadings,cut=0.15)

# Oblique rotation with oblimin

fa_oblimin = fa(r = survey,nfactors = 2,fm = 'pa',rotate = 'oblimin')
print(fa_oblimin$loadings,cut=0.15)

# Interpreting results

print(fa_oblimin$loadings,cut=0.15, sort=T)

# Variables now are easily distinguished between Principal Axis 1 and 2

# Visualize

fa.diagram(fa_oblimin,sort = T)

# Representing the factor

# Average scores of variables reflecting the factor

factor1_avg = rowMeans(survey[,c('prevents_cavities','strengthens_gums','prevent_decay_not_imp')])
factor2_avg = rowMeans(survey[,c('attractive_teeth','freshens_breath','shiny_teeth')]) 

# Weighted average of variables reflecting the factor, 
# where weights are the factor loadings

factor1_score = fa_oblimin$scores[,'PA1']
factor2_score = fa_oblimin$scores[,'PA2']

# Pick a variable as a representative of the factor. 
# Here, we are selecting the variable with the largest factor loading.

factor1_surrogate = survey[,'prevents_cavities']
factor2_surrogate = survey[,'attractive_teeth']