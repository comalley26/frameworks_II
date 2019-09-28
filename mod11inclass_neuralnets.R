setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

digits = read.csv('train.csv')

dim(digits)

head(colnames(digits))
tail(colnames(digits))

digits$label = factor(digits$label,levels = 0:9)
set.seed(1031)
split = sample(x = c('train','validation','test'),
               size = nrow(digits),
               replace = T,
               prob = c(0.4,0.4,0.2))

train = digits[split=='train',]
validation = digits[split=='validation',]
test = digits[split=='test',]

viewDigit = function(num){
  img = matrix(as.numeric(train[num,2:785]),ncol=28)
  return(image(x = 1:28,y = 1:28,z = img,col=gray((0:255)/255)))
}
viewDigit(101)

library(nnet)
set.seed(1031)
model1 = nnet(label~.,
              data = train,
              size=5,
              decay=0.1,
              MaxNWts=10000,
              maxit=100)

pred_train_prob = predict(model1)
pred_train_prob[1,]
sum(pred_train_prob[1,])

pred_train = predict(model1,type='class')
train$label[1]

viewDigit(1)
pred_train[1]

table(train$label,pred_train)
mean(pred_train==train$label) # Accuracy

pred = predict(model1,newdata=validation,type='class') # Validation sample
table(validation$label,pred)
mean(pred == validation$label)

pred = predict(model1,newdata=test,type='class')
table(test$label,pred)
mean(pred == test$label)

library(caret)
confusionMatrix(factor(pred),test$label)

which(pred!=test$label) # Digits that the model got wrong

viewDigit(11) # Examine the first digit the model got wrong

# We are going to explore a popular deep learning framework called h2o.
# h2o is a powerful package useful for deep learning and also machine learning. 
# It is designed for scalability and is more flexible than nnet.

library(h2o)
h2o.init()
h2o.clusterInfo()

train_h2o = as.h2o(train)

validation_h2o = as.h2o(validation)

test_h2o = as.h2o(test)

model2 = h2o.deeplearning(x=2:785,
                          y = 1,
                          training_frame = train_h2o,
                          hidden = c(5),
                          seed=1031)

pred = h2o.predict(model2,newdata = validation_h2o)
mean(pred[1]==validation_h2o$label) # Accuracy
h2o.confusionMatrix(model2,validation_h2o) # Using a built-in function

pred = h2o.predict(model2,newdata = test_h2o)
mean(pred[1]==test_h2o$label) # Accuracy
h2o.confusionMatrix(model2,test_h2o) # Using a built-in function

# Neural net with 2 hidden layers, 5 in each layer

model3 = h2o.deeplearning(x=2:785,
                          y = 1,
                          training_frame = train_h2o,
                          hidden = c(5,5),
                          seed=1031)

pred = h2o.predict(model3,validation_h2o)
mean(pred[1]==validation_h2o$label) # Accuracy went up
h2o.confusionMatrix(model3,validation_h2o) # Using a built-in function

# Neural net with 3 hidden layers, 10 in each layer

model4 = h2o.deeplearning(x=2:785,
                          y = 1,
                          training_frame = train_h2o,
                          hidden = c(10,10,10),
                          seed=1031) 

pred = h2o.predict(model4,validation_h2o)
mean(pred[1]==validation_h2o$label) # Accuracy went up
h2o.confusionMatrix(model4,validation_h2o)

# Neural net with 3 hidden layers, 50 in each layer

model5 = h2o.deeplearning(x=2:785,
                          y = 1,
                          training_frame = train_h2o,
                          hidden = c(50,50,50),
                          seed=1031)

pred = h2o.predict(model5,validation_h2o)
mean(pred[1]==validation_h2o$label) # Accuracy went up
h2o.confusionMatrix(model5,validation_h2o)

# Neural net with 4 hidden layers, 10 in each layer

model6 = h2o.deeplearning(x=2:785,
                          y = 1,
                          training_frame = train_h2o,
                          hidden = c(10,10,10,10),
                          seed=1031)

pred = h2o.predict(model6,validation_h2o)
mean(pred[1]==validation_h2o$label) # Accuracy went down
h2o.confusionMatrix(model6,validation_h2o)