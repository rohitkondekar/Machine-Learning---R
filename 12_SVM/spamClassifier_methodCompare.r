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
setwd("/Projects/Machine Learning R/12_SVM")

######### Input Paths ######### 
spam.path <- "data/spam/" 
spam2.path <- "data/spam_2/" 
easyham.path <- "data/easy_ham/" 
easyham2.path <- "data/easy_ham_2/" 
hardham.path <- "data/hard_ham/" 
hardham2.path <- "data/hard_ham_2/"
######### ######### ######### 

#function to get just the text content
#starts whr there is a first line break

get.msg <- function(path){
  con <- file(path,open="rt",encoding="latin1")
  text <- readLines(con)
  msg <- text[seq(which(text=="")[1]+1,length(text),by=1)]
  close(con)
  return(paste(msg,collapse="\n"))
}

get.dtm <- function(data){
  corpus <- Corpus(VectorSource(data))
  corpus <- tm_map(corpus,tolower)
  control <- list(stopwords=TRUE,removeNumbers=TRUE,removePunctuation=TRUE, minDocFreq=2, weight = weightTfIdf)
  dtm <- DocumentTermMatrix(corpus,control)
  return(dtm)
}

spam.files <- list.files(spam.path)
spam.files <- spam.files[which(spam.files!="cmds")]
spam.data <- sapply(spam.files,function(p) get.msg(paste(spam.path,p,sep="")))
rm(spam.files)

hardham.files <- list.files(hardham.path)
hardham.files <- hardham.files[which(hardham.files!="cmds")]
hardham.data <- sapply(hardham.files,function(p) get.msg(paste(hardham.path,p,sep="")))
rm(hardham.files)


names(spam.data) <- paste(names(spam.data),"spam",sep="_")
names(hardham.data) <- paste(names(hardham.data),"ham",sep="_")


spam.dtm <- get.dtm(spam.data)
str(spam.dtm)

hardham.dtm <- get.dtm(hardham.data)
str(hardham.dtm)

rm(spam.data)
rm(hardham.data)

#combination of both corpuses
combined.dtm <-c(spam.dtm,hardham.dtm)
str(combined.dtm)

rm(spam.dtm)
rm(hardham.dtm)

combined.matrix <- as.matrix(combined.dtm)

str(combined.matrix)
combined.matrix[1,1:10]

####################################################
# Trying out Models

indices <- sample(1:nrow(combined.matrix),size=0.5*nrow(combined.matrix))
training.combined.matrix <- combined.matrix[indices,]
testing.combined.matrix <- combined.matrix[-indices,]

train.x <- training.combined.matrix[,1:ncol(training.combined.matrix)]
train.y <- ifelse(grepl(pattern="spam",x=row.names(training.combined.matrix))==TRUE,1,0)

test.x <- testing.combined.matrix[,1:ncol(testing.combined.matrix)]
test.y <- ifelse(grepl(pattern="spam",x=row.names(testing.combined.matrix))==TRUE,1,0)



# Model 1 - Regularized Logistic Regression

regularized.logit.fit <- glmnet(train.x,train.y,family=c('binomial'))

lambdas <- regularized.logit.fit$lambda


as.numeric(predict(regularized.logit.fit, test.x, s=lambdas[1])>0)

performance <- data.frame()

for(lambda in lambdas){
  predictions <- predict(regularized.logit.fit, test.x, s=lambda)
  predictions <- as.numeric(predictions)
  rmse <- sqrt(mean((predictions-test.y)^2))
  performance <- rbind (performance, data.frame(Lambda=lambda, RMSE=rmse))
}

str(performance)

ggplot(performance,aes(x=Lambda,y=RMSE))+
  geom_point()+
  geom_line()+
  scale_x_log10()

best.lambda <- performance[which(performance$RMSE==min(performance$RMSE)),1]
subset(performance,Lambda==best.lambda)


#
