library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation
library('corrplot') # visualisation
library('ggfortify') # visualisation
#library('qgraph') # visualisation
library('dplyr') # data manipulation
library('readr') # data input
library('tibble') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('xgboost') # classifier

setwd("D:/Research/Kaggle/Merc")

train = read.csv('./train.csv')
test = read.csv('./test.csv')

data = subset(data, select = -c(ID))

data = subset(data, select = -c(X0,X1,X2,X3,X4,X5,X6,X8))
dataFactor = subset(data, select = c(X0,X1,X2,X3,X4,X5,X6,X8))

str(data)
colnames(data)


M <- cor(data)
corrplot(M, method="circle")


train %>%
  select(y,X0:X8) %>%
  mutate_each(funs(as.integer), starts_with("X")) %>%
  cor(use="complete.obs", method="spearman") %>%
  corrplot(type="lower", method="number", diag=FALSE)

length(colnames(train))

hist(train$y, col = 'gray')
lines(train$y, col = 'red')

missingValues = (sum(train==0))/(length(colnames(train)) * nrow(train)) * 100
print(missingValues) # 82%


polygon(density(train$y), col="red", border="blue")

ggplot(density(train$y), aes(x=values)) + geom_density()
densityPlot(train$y)

library(rpart)
library(ada)
library(randomForest)
library(caret) 




rf = randomForest(y ~ ., data=data, mtry=3,
             importance=TRUE, na.action=na.omit)
rf.predict = predict(rf, test)



#####################################
######## Apply AdaBoost #############
#####################################
set.seed(1234)

# AdaBoost
results = list()
cv.svm.xvalpred=rep(0,nrow(data))
xvs=rep(1:10,length=nrow(data))
xvs=sample(xvs)
sum = 0
for(i in 1:10){
  train=data[xvs!=i,]
  test=data[xvs==i,]
  glub=ada(y~ ., loss="exponential", data=train)
  result <- RMSE(test$y, predict(glub,test))
  print(result)
}


