################################################
# We are trying to build a binary Classifier - spam or ham?
# Steps:
# 1. Get the mail data :- SpamAssassin public corpus
# We will be using tm - text mining package to analyse data
# 
# 2. Technique: count the number of each words and determine if the message is spam - counting method.
#
# Read data - take only the text part 
# Generate a Term Document matrix out of it
#
# Using this TDM - we calculate the percentage of documents in which a given term occurs.
#                    - (no. of documents having that term) / (tot no. of docs)
# Using this TDM - find frequency of each word in the entire corpus

require(tm)
require(ggplot2)

setwd("/Projects/ML - R/Spam classifier")

#set different paths for different datas

spam.path <- "data/spam/" 
spam2.path <- "data/spam_2/" 
easyham.path <- "data/easy_ham/" 
easyham2.path <- "data/easy_ham_2/" 
hardham.path <- "data/hard_ham/" 
hardham2.path <- "data/hard_ham_2/"


#function to get just the text content
#starts whr there is a first line break

get.msg <- function(path) {
  con <- file(path,open="rt",encoding="latin1")
  text <- readLines(con)
  msg <- text[seq(which(text=="")[1]+1,length(text),by=1)]
  close(con)
  return(paste(msg,collapse="\n"))
}

#######################
#Generating Spam Training Data
#######################

#apply that function to every file in the directory
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds")]
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path,p,sep="")))

head(all.spam)


#generate Term Document Matrix (TDM) using tm package
get.tdm <- function(vec) {
  doc.corpus <- Corpus(VectorSource(vec))
  control <- list(stopwords=TRUE,removeNumbers=TRUE,removePunctuation=TRUE,minDocFreq=2)
  doc.tdm <- TermDocumentMatrix(doc.corpus,control)
  return(doc.tdm)
}

spam.tdm <- get.tdm(all.spam)
head(spam.tdm)

#convert this into data frame with word name and count across all the documents
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)

spam.df <- data.frame(cbind(names(spam.counts),as.numeric(spam.counts)),stringsAsFactors=FALSE)
names(spam.df) <- c("term","frequency")

str(spam.df)
spam.df$frequency <- as.numeric(spam.df$frequency)

#we calculate the percentage of documents in which a given term occurs
spam.occurance <- sapply(1:nrow(spam.matrix),function(i) {length(which(spam.matrix[i,]>0))/ncol(spam.matrix)})

# find density of each word
spam.density <- spam.df$frequency/sum(spam.df$frequency)

#add it to data frame
spam.df <- transform(spam.df,density=spam.density, occurance=spam.occurance)

#print sorted in reverse order
head(spam.df[with(spam.df,order(-spam.df$occurance)),])
head(spam.df[with(spam.df,order(-spam.df$density)),])
head(spam.df[with(spam.df,order(-spam.df$frequency)),])

#------------------------------------------------------------------------
#Get training data for ham
ham.docs <- dir(easyham.path)
ham.docs <- ham.docs[which(ham.docs!="cmds")]
ham.docs <- ham.docs[1:500]

all.ham <- sapply(ham.docs, function(p) get.msg(paste(easyham.path,p,sep="")))
head(all.ham)

#generate Term Document Matrix (TDM) 
ham.tdm <- get.tdm(vec=all.ham)

ham.matrix <- as.matrix(ham.tdm)
ham.counts <- rowSums(ham.matrix)

# names retrieves names as it a named vector
ham.df <- data.frame(cbind(names(ham.counts)),as.numeric(ham.counts),stringsAsFactors=FALSE)
names(ham.df) <- c("term","frequency")

#no. of documents in which it occurs/ tot docs
ham.occurance <- sapply(1:nrow(ham.matrix), function(i) { length(which(ham.matrix[i,]>0))/ncol(ham.matrix) })
head(ham.occurance)
ham.density <- ham.df$frequency/sum(ham.df$frequency)

ham.df <- transform(ham.df, density=ham.density, occurance=ham.occurance)
head(ham.df[with(ham.df,order(-ham.df$occurance)),])

#-------------------------------------

#########
# Function to return probabilty of whether a message is spam or not.
# keeping initial probability of message been spam as 0.5
# probability for the words which are not in email : 0.000001
#########

classify.email <- function(path, training.df, prior=0.50, c=1e-6)
{
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  
  msg.match <- intersect(names(msg.freq),training.df$term)
  
  #for not matched
  if(length(msg.match)<1){
    return(prior*c^(length(msg.freq)))
    
  }#for matched get occurance values
  else{
    match.prob<- training.df$occurance[match(msg.match,training.df$term)] #match returns the postion in the secong vector
    return(prior*prod(match.prob)*c^(length(msg.freq)-length(msg.match)))
  }
}

hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs!="cmds")]

#checking with training data as spam
hardham.spamtest <- sapply(hardham.docs,function(p) classify.email(paste(hardham.path,p,sep=""),spam.df))
head(hardham.spamtest)

#checking with training data as ham
hardham.hamtest <- sapply(hardham.docs,function(p) classify.email(paste(hardham.path,p,sep=""),ham.df))
head(hardham.hamtest)

#Result !!
hardham.res <- ifelse(hardham.spamtest>hardham.hamtest,TRUE,FALSE)
summary(ham.res)

#----------------------------------------------------------------------

#doing this for classifying all email types
spam.classifier <- function(path){
  email.spam <- classify.email(path,spam.df,prior=0.2)
  email.ham <- classify.email(path,ham.df,prior=0.8)
  return(c(email.spam,email.ham,ifelse(email.spam>email.ham,1,0)))
}

#for easyham2
easyham2.docs<-dir(easyham2.path)
easyham2.docs<-easyham2.docs[which(easyham2.docs!="cmds")]
easyham2.test <- lapply(easyham2.docs,function(p) spam.classifier(paste(easyham2.path,p,sep="")))

easyham2.matrix <- do.call(rbind,easyham2.test)
easyham2.final <- cbind(easyham2.matrix,"EASYHAM")

#for spam2
spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs!="cmds")]
spam2.test <- lapply(spam2.docs,function(p) spam.classifier(paste(spam2.path,p,sep="")))

spam2.matrix <- do.call(rbind,spam2.test)
spam2.final <- cbind(spam2.matrix,"SPAM")

#for hardHAM2
hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs!="cmds")]
hardham2.test <- lapply(hardham2.docs,function(p) spam.classifier(paste(hardham2.path,p,sep="")))

hardham2.matrix <- do.call(rbind,hardham2.test)
hardham2.final <- cbind(hardham2.matrix,"HARDHAM")

class.matrix <- rbind(easyham2.final,spam2.final,hardham2.final)
class.df <- as.data.frame(class.matrix,stringsAsFactors=FALSE)

head(class.df)
names(class.df)<-c("pr.spam","pr.ham","isSpam","ordData")

class.df$pr.spam <- as.numeric(class.df$pr.spam)
class.df$pr.ham <-as.numeric(class.df$pr.ham)
class.df$isSpam <-as.logical(as.numeric(class.df$isSpam))


#------------ Table view of final result----------
#creating a df for displating probability - confusion matrix
easyham2.df <- class.df[which(class.df$ordData=="EASYHAM"),]
spam2.df <- class.df[which(class.df$ordData=="SPAM"),]
hardham2.df<- class.df[which(class.df$ordData=="HARDHAM"),]

final.matrix<-rbind(list("EASYHAM",nrow(easyham2.df[easyham2.df$isSpam==FALSE,])/nrow(easyham2.df)*100,nrow(easyham2.df[easyham2.df$isSpam==TRUE,])/nrow(easyham2.df)*100))
final.matrix<-rbind(final.matrix,rbind(list("SPAM",nrow(spam2.df[spam2.df$isSpam==FALSE,])/nrow(spam2.df)*100,nrow(spam2.df[spam2.df$isSpam==TRUE,])/nrow(spam2.df)*100)))
final.matrix<-rbind(final.matrix,rbind(list("HARDHAM",nrow(hardham2.df[hardham2.df$isSpam==FALSE,])/nrow(hardham2.df)*100,nrow(hardham2.df[hardham2.df$isSpam==TRUE,])/nrow(hardham2.df)*100)))
final.df <- as.data.frame(final.matrix,stringsAsFactors=FALSE)
final.df

#------------ Graph view of final result----------
class.plot <- ggplot(class.df,aes(x=log(pr.ham),y=log(pr.spam))) +
    stat_abline(yintercept = 0, slope = 1) +
    geom_point(aes(color=ordData))+
    xlab("log[Pr(HAM)]") +
    ylab("log[Pr(SPAM)]")

ggsave(filename="./classifiedData.png",plot=class.plot,height=10,width=10)
