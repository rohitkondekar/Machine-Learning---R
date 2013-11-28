#Chapter 5,6

library(ggplot2)
setwd("/Projects/Machine Learning R/5 - Regression/data")

height.weights <- read.csv(file="01_heights_weights_genders.csv",header=T)

fitted.regression <- lm(height.weights$Weight~height.weights$Height)
summary(fitted.regression)

coef(fitted.regression)

#to predict weights use predict function - uses regression
predict(fitted.regression)

#to calculate residuals manually -
errors = height.weights$Weight-predict(fitted.regression)

#plot the residuals against the truth: - the graph should not have any perticular relation
plot(fitted.regression, which=1)
#contradictory example
x<-1:10
y<-x^2
plot(lm(y~x),which=1)


#x <- runif(200,0,1000)
#xsquare <- x^2
x <- seq(-10, 10, by = 0.01)
y <- 1 - x^2 + rnorm(length(x), 0, 5)

ggplot(data.frame(X=x,Y=y),aes(x=x,y=y))+
  geom_point()+
  geom_smooth(se=FALSE)

x.squared <- x^2

ggplot(data.frame(XSquared = x.squared, Y = y), aes(x = XSquared, y = Y)) + geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

##########################################################
# Polynomial Regression
#sin wave

set.seed(1);

x <- seq(0,1,by=0.01)
y <- sin(2*pi*x)+rnorm(length(x),0,0.1)

df <- data.frame(x,y)

ggplot(df,aes(x=x,y=y))+
  geom_point()

#trying general regression model
summary(lm(y~x))

ggplot(df,aes(x=x,y=y))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

#adding x^2 x^3
x2 <- x^2
x3 <- x^3
summary(lm(y~x+x2+x3)) #97%

length(poly(x,degree=14))

#lm with poly function
summary(lm(y~poly(x,degree=14)))

#checking out the graph of prediction
model <- lm(y~poly(x,degree=7))
df$PredictedY <- predict(model)

ggplot(df,aes(x=x,y=PredictedY))+
  geom_point()+
  geom_line()

##############################################################################
#Doing cross validation
#Cross-validation, at its core, simply refers to this ability of ours to 
#simulate testing our model on future data by ignoring part of our historical data 
#during the model-fitting process. 

set.seed(1);

x <- seq(0,1,by=0.01)
y <- sin(2*pi*x)+rnorm(length(x),0,0.1)

indices <- sample(1:length(x),size=round(0.5*length(x)))
training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

training.df <- data.frame(x=training.x,y=training.y)
test.df <- data.frame(x=test.x,y=test.y)

#now to find out which degree to use - we use RMSE for different degrees
RMSE <- function(y,y1){
  rmse <- sqrt(mean((y-y1)^2))
  return(rmse)
}

performance <- data.frame()
for(d in 1:12){
  poly.fit <- lm(y~poly(x,degree=d),data=training.df)
  performance <- rbind(performance,data.frame(Degree=d,
                                              Data="Training",
                                              RMSE=RMSE(training.df$y,predict(poly.fit))))
  
  performance <- rbind(performance,data.frame(Degree=d,
                                              Data="Test",
                                              RMSE=RMSE(test.df$y,predict(poly.fit,newdata=test.df))))
}

ggplot(performance,aes(x=Degree,y=RMSE,linetype=Data))+
  geom_line()+
  geom_point()

###########################################################################################
#Doing Regularization
#simplifies the model and prevents overfitting

set.seed(1)
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)
n <- length(x)
indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices] 
training.y <- y[indices]
test.x <- x[-indices] 
test.y <- y[-indices]

df <- data.frame(X = x, Y = y)
training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h) {
  return(sqrt(mean((y - h) ^ 2))) 
}

install.packages('glmnet')
library(glmnet)
library(ggplot2)

glmnet.fit <- with(training.df,glmnet(x=poly(X,degree=10),Y))
lamdas <- glmnet.fit$lambda

#now loop over lambdas to find the best match - having the least SE
performance <- data.frame()
for(lambda in lamdas) {
  performance <- rbind(performance, data.frame(Lambda=lambda, 
                                               RMSE= rmse(test.y,with(test.df,predict(glmnet.fit,poly(X,10),s=lambda)))))
}

ggplot(performance,aes(x=Lambda,y=RMSE))+
  geom_line()+
  geom_point()

#we need to get the minima
best.lambda <- performance$Lambda[which(performance$RMSE==min(performance$RMSE))]

glmnet.fit <- with(df,glmnet(x=poly(X,degree=10),Y))

coef(glmnet.fit,s=best.lambda)

#As you can see from looking at this table, we end up using only 3 nonzero coefficients, 
#even though the model has the ability to use 10. Selecting a simpler model like this one, 
#even when more complicated models are possible, is the major strategy behind regu- larization. 
#And with regularization in our toolkit, we can employ polynomial regression with a large degree
#and still keep ourselves from overfitting the data.

###########################################################################################

#Text Regression
#Here we use input as text to carry out regression

#Task : To give you a sense of this problem, we’ll work through a simple case study 
#in which we try to predict the relative popularity of the top-100-selling books 
#that O’Reilly has ever published using only the descriptions of 
#those books from their back covers as input. 

require(glmnet)
require(ggplot2)
require(tm)
setwd("/Projects/Machine Learning R/5 - Regression/data")

ranks <- read.csv(file="oreilly.csv",header=T,stringsAsFactors=FALSE)
nrow(ranks)
#now analysis is depending on the terms in the description
#hence creating a term document matrix

documents <- data.frame(Text=ranks$Long.Desc.)
row.names(documents) <- 1:nrow(documents)

corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,removeWords,stopwords('english'))

dtm <- DocumentTermMatrix(corpus) #its DTM not TDM - term document matrix

x<-as.matrix(dtm)
y<-rev(1:100)


#regression analysis
set.seed(1)
performance <- data.frame()

for(lambda in c(0.1, 0.25, 0.5, 1, 2, 5)){
  for(i in 1:50){
    indices <- sample(1:100,80)
    training.x <- x[indices,]
    training.y <- y[indices]
    
    test.x <- x[-indices,]
    test.y <- y[-indices]

    glm.fit <- glmnet(training.x,training.y)
    predicted.y <- predict(glm.fit,test.x,s=lambda)
    rmse <- sqrt(mean((predicted.y - test.y) ^ 2))
    performance <- rbind(performance, data.frame(Lambda = lambda,
                                                 Iteration = i, RMSE = rmse))
    
  }
}

ggplot(performance, aes(x = Lambda, y = RMSE)) +   
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') + 
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point')

#as we can see the RMSE is decreasing as we increase lambda
#hence this model has failed

#Doing Logistic Regression
#predicting whether book is in TOP 50
#logistic regression - glmnet(x,y,family='binomial')
#glmnet(x,y,family='binomial') - returns 2 types of numbers +ve and -ve : different groups

y <- rep(c(1,0),each=50) #whether it is in top 50 or not

set.seed(1)
performance <- data.frame()

for(i in 1:250){
  indices <- sample(1:100,80) #following 80:20 ratio
  training.x <- x[indices,]
  training.y <- y[indices]
  
  test.x <- x[-indices,]
  test.y <- y[-indices]
  
  for(lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1)){
    glm.fit <- glmnet(training.x,training.y,family="binomial")
    predicted.y <- ifelse(predict(glm.fit,test.x,s=lambda)>0,1,0)
    error.rate <- mean(predicted.y!=test.y)
    performance <- rbind(performance, data.frame(Lambda=lambda,
                                                 Iteration=i,
                                                 Error.Rate=error.rate))
    
  }
}

head(performance)
ggplot(performance,aes(x=Lambda,y=Error.Rate))+
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar')+
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point')+
  scale_x_log10()

#clearly we can see that as lambda increases error rate increases
#our model is quite successfull
  