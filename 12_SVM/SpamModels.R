###########################################################################
# Comparing Different Models
# For Spam Classification
# Models Used:
#   1. Logistic Regression
#   2. KNN
#   3. SVM - different kernels
# Data used: SpamAssasign corpus
###########################################################################


library(ggplot2)
library(tm)
library(e1071)
library(glmnet)
library(class)

setwd("/Projects/Machine Learning R/12_SVM")

load('dtm.RData')

set.seed(1990)
indices <- sample(1:nrow(dtm),size=0.5*nrow(dtm))

gtrain.x <- dtm[indices,3:ncol(dtm)]
gtrain.y <- dtm[indices,1]

gtest.x <- dtm[-indices,3:ncol(dtm)]
gtest.y <- dtm[-indices,1]
rm(dtm)


#################################################################################
# Linear Regression with Regularization 
#################################################################################

#carrying out regularization with lambda tuning
performance <- data.frame()
for(i in 1:5) {
  indices <- sample(1:nrow(gtrain.x),size=0.5*nrow(gtrain.x))
  
  train.x <- gtrain.x[indices,3:ncol(gtrain.x)]
  train.y <- gtrain.y[indices]
  
  test.x <- gtrain.x[-indices,3:ncol(gtrain.x)]
  test.y <- gtrain.y[-indices]
  
  regularized.logit.fit <- glmnet(train.x,train.y,family=c('binomial'))
  lambdas <- regularized.logit.fit$lambda
  for(lambda in lambdas){
    predictions <- ifelse(predict(regularized.logit.fit, test.x, s=lambda)>0,1,0)
    mse <- mean(predictions!=test.y)#sqrt(mean((predictions-test.y)^2))
    performance <- rbind (performance, data.frame(Iteration=i, Lambda=lambda, MSE=mse))
  }  
}

head(performance)
ggplot(performance,aes(x=Lambda,y=MSE))+
  geom_point()+
  scale_x_log10()

best.lambda <- performance[(max(which(performance$MSE==min(performance$MSE)))),2]
subset(performance,Lambda==best.lambda)

#################################################################################
# K Nearest Neighbors 
#################################################################################

performance_knn <- data.frame()
limit <- sqrt(nrow(gtrain.x))
for(i in 1:limit){
  predictions <- knn(train=gtrain.x,test=gtest.x,cl=gtrain.y,k=i)
  mse <- mean(predictions!=gtest.y)
  performance_knn <- rbind(performance_knn,data.frame(K=i,MSE=mse))
}

ggplot(performance_knn,aes(x=K,y=MSE))+
  geom_point()+
  scale_x_log10()

best.k <- with(performance_knn,which(performance_knn$MSE==min(performance_knn)))
subset(performance_knn,K==best.k)
# 20 % error rate

#################################################################################
# Support Vector Machine 
#################################################################################

##### Linear #####
performance_svm <- data.frame()
for(cost in 1:20){
  linear.svm <- svm(x=gtrain.x,y=gtrain.y,type='C-classification',kernel='linear',cost=cost)
  predictions <- predict(linear.svm,gtest.x)
  mse <- mean(predictions!=gtest.y)
  performance_svm <- rbind(performance_svm,data.frame(Cost=cost,MSE=mse))
}

ggplot(performance_svm,aes(x=Cost,y=MSE))+
  geom_point()

best.cost <- with(performance_svm,which(performance_svm$Cost==min(performance_svm$Cost)))
subset(performance_svm,Cost==best.cost)

##### Polynomial #####
performance_svm1 <- data.frame()
for(cost in 1:20){
  poly.svm <- svm(x=gtrain.x,y=gtrain.y,type='C-classification',kernel='polynomial',cost=cost)
  predictions <- predict(poly.svm,gtest.x)
  mse <- mean(predictions!=gtest.y)
  performance_svm1 <- rbind(performance_svm1,data.frame(Cost=cost,MSE=mse))
}

ggplot(performance_svm1,aes(x=Cost,y=MSE))+
  geom_point()

best.cost1 <- with(performance_svm1,which(performance_svm1$Cost==min(performance_svm1$Cost)))
subset(performance_svm1,Cost==best.cost1)

