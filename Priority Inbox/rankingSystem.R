################################################
# Here we will be building a ranking system - for emails
#
# We have four features we will use during training. 
#
# Steps - & features encorporated:
# 1. We begin by ordering the messages chronologically because 
#    in this case much of what we are interested in predicting is contained in the temporal dimension.
#    The first half of these messages are used to train our ranker. 
#
# 2.  The first is a proxy for the social feature, which measures the 
#     volume of messages from a given user in the training data. 
#
# 3. Next, we attempt to compress the temporal measurements by looking for 
#    threads and ranking active threads higher than inactive ones.
#
# 4. Finally, we add two content features based on frequent terms in email subjects and message bodies.
################################################

library(tm)
library(ggplot2)
library(plyr)
setwd("/Projects/Machine Learning R/Priority Inbox")

data.path = "../Spam classifier/data/"
ham.path = paste(data.path,"easy_ham/",sep="")

#------------------------------------------------------------------------------------------------------
#Parsing Methods

#Step 1 : get full message
get.fullmsg <- function(path){
  con <- file(path,open="rt",encoding="latin1")
  text <- readLines(con)
  close(con)
  return(text)
}


#Step 2 : get sender's email address

#used grepl - grep which returns logical value for everything
#used strplit - split string

get.from <- function(msg.vec){
  from<-msg.vec[grepl("From:",x=msg.vec)]
  from<-strsplit(x=from,split="[<>: ]")[[1]]
  from<-from[which(from!="" & from!=" ")]
  from<-from[grepl(pattern="@",x=from)]
  return(from)
}

#Step 2 : get msg part - the body - starts from first blank line

get.msg <- function(msg.vec){
  msg.vec<-msg.vec[which(msg.vec=="")[1]:length(msg.vec)]
  return(paste(msg.vec,collapse="\n"))
}


#Step 3: get Subject

get.subject <- function(msg.vec){
  msg.vec<-msg.vec[grepl(pattern="Subject:",x=msg.vec)]
  if(length(msg.vec)>0){
    return(strsplit(x=msg.vec,split="Subject: ")[[1]][2])
  }
  else{
    return("")
  }
}

#Step 4 : get Date

get.date <- function(msg.vec){
  msg.grep <- grepl(pattern="^Date:",x=msg.vec)
  msg.date <- msg.vec[which(msg.grep==TRUE)][1] #only the first date
  msg.date <- strsplit(x=msg.date,split="\\+|\\-|: ")[[1]][2] #colon space - takes out Date: thing
  msg.date <- gsub(pattern="^\\s+|\\s+$",replacement="",x=msg.date) #remove trailing whitespaces
  msg.date <- strtrim(msg.date,width=25) #to handle cases such as Date: Wed, 04 Dec 2002 11:36:32 GMT
  return(msg.date)
}

#-------------------------------------------------------------------------------------------------------

#Consolidation into 1 function - does all the parsing

parse.email <- function(path){
  msg.full <- get.fullmsg(path)
  msg.date <- get.date(msg.full)
  msg.subject <- get.subject(msg.full)
  msg.from <- get.from(msg.full)
  msg <- get.msg(msg.full)
  return(c(msg.date,msg.from,msg.subject,msg,path)) #We include the path string as the final 
                      #column because it will make ordering the data easier during the testing phase.
}

#read docs - easyham
easyham.docs <- dir(ham.path)
easyham.docs <- easyham.docs[which(easyham.docs!="cmds")]
easyham.parse<-lapply(easyham.docs, function(p) parse.email(paste(ham.path,p,sep="")))
head(easyham.parse)

easyham.matrix <- do.call(rbind,easyham.parse)
head(easyham.matrix)
easyham.matrix <- easyham.matrix[,1:5] #--- don't know why a extra column of date is coming at the end

allparse.df <- as.data.frame(easyham.matrix,stringsAsFactors=FALSE)
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")

#put the date column into date format as we need to sort them in chronological order
#there are 2 formats of date in the data
# Wed, 04 Dec 2002 11:36:32
# 04 Dec 2002 11:49:23

date.converter <- function(date){
  pattern1 <- "%a, %d %b %Y %X"
  pattern2 <- "%d %b %Y %X"
  pattern1.convert <- strptime(x=date,format=pattern1)
  pattern2.convert <- strptime(x=date,format=pattern2)
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]
  return(pattern1.convert)
}

allparse.df$Date <- date.converter(allparse.df$Date)
str(allparse.df)

#to lower case - uniform
allparse.df$From.EMail<-tolower(allparse.df$From.EMail)
allparse.df$Subject<-tolower(allparse.df$Subject)

#order occording to date
priority.df<-allparse.df[with(allparse.df,order(Date)),]

#training data - first half
priority.train <- priority.df[1:round(nrow(priority.df)/2),]
priority.train$Date <- as.POSIXct(priority.train$Date) #this is needed because of problem with plyr thing : 
                                                       #Use POSIXct dates in data.frames, not POSIXlt

#---------------------------------------------------------------------------------------------------------
#Dataframes available

#priority.df - whole data
#priority.train - training data
############################################################################

#Feature 1 - sender counts
#Analyzing sender counts

from.weigth <- ddply(priority.train,.(From.EMail), summarize, Freq=length(Subject))

head(from.weigth)
from.weigth <- from.weigth[with(from.weigth,order(-from.weigth$Freq)),]


#Example - graph
#just taking an example and plotting
from.ex <- from.weigth[which(from.weigth$Freq>6),]
head(from.ex)
from.ex <- from.ex[with(from.ex,order(from.ex$Freq)),]

Senderplot <- ggplot(from.ex)+
        geom_rect(aes(xmin=1:nrow(from.ex)-0.5,
                      xmax=1:nrow(from.ex)+0.5,
                      ymin=0,
                      ymax=Freq,
                      fill="darkgrey",
                      color="black")) +
        scale_x_continuous(breaks=1:nrow(from.ex), labels = from.ex$From.EMail) +
        scale_fill_manual(values = c("darkgrey" = "darkgrey"), guide = "none") +
        scale_color_manual(values = c("black" = "black"), guide = "none") + 
        coord_flip() +
        ylab("Number of Emails Received (truncated at 6)") +
        xlab("Sender Address")

ggsave(filename="./emailSenderAnalysis.png",plot=Senderplot,scale=2)

## Log weighing scheme-
#log1p - log(x+1) - no issues when x=0
from.ex <- transform(from.ex, Weight=log1p(from.ex$Freq),log10Weight=log10(from.ex$Freq+1))

#now we will plot this to visualize the effect and decide which log to use - e or 10
logPlot <- ggplot(from.ex,aes(x=1:nrow(from.ex)))+
  geom_line(aes(y=from.ex$Weight, linetype="ln"))+
  geom_line(aes(y=from.ex$log10Weight, linetype="log10"))+
  geom_line(aes(y=from.ex$Freq, linetype="Absolute"))+
  scale_linetype_manual(values = c("ln" = 1,
                                   "log10" = 2,
                                   "Absolute" = 3),
                        name = "Scaling") +
  xlab("") +
  ylab("Number of emails Receieved")

ggsave(filename="./logPlot.png",plot=logPlot)


#######################################################################################
#Second Feature : threads : Again, our assumption in building this feature is that time is 
# important, and therefore threads that have more messages sent over a short period of time are 
# more active and consequently more important.

#threads are recognized using re:
#data is like: re: a re: b ......

find.threads <- function(email.df){
  response.threads<-strsplit(email.df$Subject,"re: ")
  is.Thread <- sapply(response.threads, function(p) ifelse(p[1]=="",TRUE,FALSE))
  threads <- response.threads[is.Thread]
  senders <- email.df$From.EMail[is.Thread]
  threads<-sapply(threads,function(p) paste(p[2:length(p)],collapse="re: ")) #all threads for perticular sender collapsed
  return(cbind(senders,threads))
}

threads.matrix<-find.threads(priority.train)

#create a weighting based on the senders who are most active in threads
#secondary volume-based weighting.

#weights for each user in particular time period - in thread activity

email.thread <- function(threads.matrix){
  senders <- threads.matrix[,1]
  #to count the frequency of senders in the threads
  senders.freq <- table(senders) #------turns the matrix into table with total count of each sender **
  #or could have been done using plyr
  #senders.freq <- ddply(as.data.frame(threads.matrix), .(senders), summarize, Freq=length(senders))
  
  senders.matrix <- cbind(names(senders.freq), senders.freq, log(senders.freq+1))
  senders.df <- as.data.frame(senders.matrix,stringsAsFactors=FALSE)
  names(senders.df) <- c("From.EMail","Freq","Weight")
  row.names(senders.df) <- 1:nrow(senders.df) #suppresses duplication of email in row names
  senders.df$Freq <- as.numeric(senders.df$Freq)
  senders.df$Weight <- as.numeric(senders.df$Weight)
  return(senders.df)
}

#Feature stored in this
senders.df<-email.thread(threads.matrix)

##################################################################################################
#Feature 3 : thread weights - depending on message activity
# Another weighing scheme depending on active thread - threads which are active weigh more
# weighing scheme - freq/(total time elapsed) 

get.threads <- function(threads.matrix,email.df){
  threads<-unique(threads.matrix[,2])
  threads.count<-lapply(threads, function(p) get.count(p,email.df))
  threads.matrix <- do.call(rbind,threads.count)
  return(cbind(threads,threads.matrix))
}

get.count <- function(thread,email.df){
  thread.times <- email.df$Date[which(email.df$Subject == thread
                                      | email.df$Subject == paste("re:", thread))]
  freq <-length(thread.times)
  min.time <- min(thread.times)
  max.time <- max(thread.times)
  time.span <- as.numeric(max.time-min.time)
  if(freq<2){
    return(c(NA,NA,NA))
  }
  else{
    thread.weight <- freq/time.span
    log.thread.weight <- 10+log10(thread.weight) # as thread.weight would be very small value and its log would be negative
    return(c(freq,time.span,log.thread.weight))
  }
}

thread.weights <- get.threads(threads.matrix,priority.train)
thread.weights <- as.data.frame(thread.weights,stringsAsFactors=FALSE)
names(thread.weights) <- c("Thread","Freq","Response","Weight")
thread.weights$Freq <- as.numeric(thread.weights$Freq)
thread.weights$Weight <- as.numeric(thread.weights$Weight)
thread.weights$Response <- as.numeric(thread.weights$Response)
thread.weights <- subset(thread.weights, is.na(thread.weights$Freq) == FALSE) #removes rows with single entry
head(thread.weights)

############################################################
#Feature 4 : frequent terms - in thread
# Final Weighing data - frequent terms in the threads

#firstly count terms - not needed as such here - used in term frequency
terms.count <- function(thread.vec,control){
  vec.corpus <- Corpus(VectorSource(thread.vec))
  vec.tdm <- TermDocumentMatrix(vec.corpus,control)
  return(rowSums(as.matrix(vec.tdm)))
}

thread.terms <- terms.count(thread.weights$Thread,control=list(stopwords=stopwords()))
#thread.terms <- names(thread.terms)

term.weights <- sapply(names(thread.terms), function(p) mean(thread.weights$Weight[grepl(p,thread.weights$Thread,fixed=TRUE)]))
term.weights <- data.frame(Term=names(term.weights), Weight=term.weights, stringsAsFactors=FALSE, row.names=1:length(term.weights))

############################################################
#Feature 5: Term frequency - in all the emails

msg.terms <- terms.count(priority.train$Message,control=list(stopwords=stopwords(),removePunctuation=TRUE,removeNumbers=TRUE))
msg.weights <- data.frame(Terms=names(msg.terms),Freq=log10(msg.terms),stringsAsFactors=FALSE,row.names=1:length(msg.terms))
msg.weights <- subset(msg.weights,msg.weights$Freq>0)

############################################################
# Features extracted :: 
#
# 1. from.weigth - weight for each perticular sender - depending on how many mails he has sent
# 2. senders.df - senders activity in threads
# 3. thread.weights - active threads weigh more
# 4. term.wights - term weights in threads
# 5. msg.weights - term weights in whole training data

#helper function to get weights in threads/terms:

get.weights <- function(search.term,weight.df,term=TRUE){
  if(length(search.term)>0){
    if(term){ # is a term 
      term.match <- match(names(search.term),weight.df$Term)
    }
    else{
      term.match <- match(search.term,weight.df$Thread)
    }
    match.weight <- weight.df$Weight[which(!is.na(term.match))]
    if(length(match.weight)>0){
      return(mean(match.weight))
    }
    else{
      return(1)
    }
  }
  else{
    return(1)
  }
}

#calculating Rank ----

rank.message <- function(path){
  msg <- parse.email(path)
  
  #1 - from.weigth
  from <- ifelse(length(which(from.weigth$From.EMail==msg[2]))>0,
                 from.weigth$Freq[which(from.weigth$From.EMail==msg[2])],1) #1 as it wont change the products
  #2 - senders.df
  threads.from <- ifelse(length(which(senders.df$From.EMail==msg[2]))>0,
                         senders.df$Weight[which(senders.df$From.EMail==msg[2])],1)
  
  #3 - threads.weight - active threads
  subj <- strsplit(tolower(msg[3]),"re: ")
  is.thread <- ifelse(subj[[1]][1]=="",TRUE,FALSE)
  if(is.thread){
    activity <- get.weights(subj[[1]][2],thread.weights,FALSE)
  }
  else{
    activity <- 1
  }
  
  #4 - term.weights - weights based on terms in thread
  thread.terms <- terms.count(msg[3],control=list(topwords=stopwords()))
  thread.terms.weight <- get.weights(thread.terms,term.weights)
  
  #5 - msg.weights - weights based on terms in totality
  msg.terms <- terms.count(msg[3],control=list(stopwords=stopwords(),
                                               removePunctuation=TRUE, removeNumbers=TRUE))
  msg.terms.weights <- get.weights(msg.terms, msg.weights)
  
  #calculate rank:
  #print(paste(from,threads.from,activity,thread.terms.weight,msg.terms.weights,sep=" "))
  rank <- prod(from,threads.from,activity,thread.terms.weight,msg.terms.weights)
  return(c(msg[1],msg[2],msg[3],rank,path))
                         
}
#rank.message("../Spam classifier/data/easy_ham/01065.b1ad1862ff2ceeb1bfc9cbdf3f87ebee")


##########################################################################################
#Executing the Ranker
#first half is used as training data to get a threshold value for priority messages

train.paths <- priority.df$Path[1:round(nrow(priority.df)/2)]
test.paths <- priority.df$Path[((round(nrow(priority.df) / 2)) + 1):nrow(priority.df)]

train.ranks <- lapply(train.paths,rank.message)
train.ranks.matrix <- do.call(rbind,train.ranks)
train.ranks.matrix <- cbind(train.ranks.matrix, "TRAINING")
train.ranks.df <- data.frame(train.ranks.matrix, stringsAsFactors=FALSE)
names(train.ranks.df) <- c("Date", "From", "Subj", "Rank", "Path", "Type")
head(train.ranks.df)
train.ranks.df$Rank <- as.numeric(train.ranks.df$Rank)

priority.threshold <- median(train.ranks.df$Rank)
train.ranks.df$Priority <- ifelse(train.ranks.df$Rank >= priority.threshold,1,0)

# Visualize the results to locate threshold
threshold.plot <- ggplot(train.ranks.df, aes(x=Rank))+
  stat_density(aes(fill="darkred")) + #density
  geom_vline(xintercept = priority.threshold, linetype = 2) +
  scale_fill_manual(values = c("darkred" = "darkred"), guide = "none")

ggsave(plot = threshold.plot,
       filename = "./01_threshold_plot.png",
       height = 4.7,
       width = 7)


test.paths<-test.paths[grepl(pattern="../Spam classifier",x=test.paths)] #some issue here with priority.df

#now do for testing data
test.ranks <- lapply(test.paths,rank.message)
test.ranks.matrix <- do.call(rbind,test.ranks)
test.ranks.matrix <- cbind(test.ranks.matrix, "TESTING")
test.ranks.df <- data.frame(test.ranks.matrix, stringsAsFactors=FALSE)
names(test.ranks.df) <- c("Date", "From", "Subj", "Rank", "Path", "Type")
head(test.ranks.df)
test.ranks.df$Rank <- as.numeric(test.ranks.df$Rank)

test.ranks.df$Priority <- ifelse(test.ranks.df$Rank >= priority.threshold,1,0)

#combine the dataset
final.df <- rbind(train.ranks.df, test.ranks.df)
final.df$Date <- date.converter(final.df$Date)
final.df <- final.df[rev(with(final.df, order(Date))), ]
final.df <- final.df[rev(with(final.df, order(Rank))), ]

testing.plot <- ggplot(subset(final.df, Type == "TRAINING"), aes(x = Rank)) +
  stat_density(aes(fill = Type, alpha = 0.65)) +
  stat_density(data = subset(final.df, Type == "TESTING"),
               aes(fill = Type, alpha = 0.65)) +
  geom_vline(xintercept = priority.threshold, linetype = 2) +
  scale_alpha(guide = "none") +
  scale_fill_manual(values = c("TRAINING" = "darkred", "TESTING" = "darkblue"))

ggsave(plot = testing.plot,
       filename = "./02_testing_plot.png",
       height = 4.7,
       width = 7)