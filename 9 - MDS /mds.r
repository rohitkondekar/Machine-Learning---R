################################################################
# Clustering Based on Similarity
# Classical Multidimensional Scaling
# multidimensional scaling (MDS) for clustering observations
# based on a measure of distance among the observations.
# US Senators Cluster?

#Data format

# 1. Congress Number
# 2. ICPSR ID Number: 5 digit code assigned by the ICPSR as
#    corrected by Howard Rosenthal and myself. 
# 3. State Code: 2 digit ICPSR State Code.
# 4. Congressional District Number (0 if Senate)
# 5. State Name
# 6. Party Code: 100 = Dem., 200 = Repub. (See PARTY3.DAT)
# 7. Occupancy: ICPSR Occupancy Code -- 0=only occupant; 1=1st occupant; 2=2nd occupant; etc. 
# 8. Last Means of Attaining Office: ICPSR Attain-Office Code -- 1=general election;
#    2=special election; 3=elected by state legislature; 5=appointed 
# 9. Name
# 10 - to the number of roll calls + 10: Roll Call Data --
#    0=not a member, 1=Yea, 2=Paired Yea, 3=Announced Yea,
#    4=Announced Nay, 5=Paired Nay, 6=Nay, 7=Present (some Congresses, also not used some Congresses),
#    8=Present (some Congresses, also not used some Congresses), 
#    9=Not Voting
################################################################
library(foreign)
library(ggplot2)
setwd("/Projects/Machine Learning R/9 - MDS ")

dir.name <- "./roll_call/"
dta.files <- list.files(dir.name)

#read .dta files
rollcall.data <- lapply(dta.files,function(p) read.dta(file=paste(dir.name,p,sep=""),convert.factors=FALSE))

class(rollcall.data)
head(rollcall.data[[1]])

#simplified coding
# 99 state is of vice president - rarely votes - remove
# Vote > 6 = 0 - non votes
# Vote 1-3 = 1 - yay votes
# Vote 4-6 = -1 - nay votes

rollcall.simplified <- function(df){
  no.pres <- subset(df,state<99)
  for(i in 10:ncol(no.pres)){
    no.pres[,i] <- ifelse(no.pres[,i]>6,0,no.pres[,i])
    no.pres[,i] <- ifelse(no.pres[,i]>0 & no.pres[,i]<4,1,no.pres[,i])
    no.pres[,i] <- ifelse(no.pres[,i]>1,-1,no.pres[,i])
  }
  return(as.matrix(no.pres[10:ncol(no.pres)]))
}

rollcall.simple <- lapply(rollcall.data,rollcall.simplified)
head(rollcall.simple)

#Now going on to doing MDS
#steps:
# 1. create distance matrix using dist function on multiplication of matrix with its transpose
# 2. do 2-dimensional MDS using cmdscale
rollcall.dist <- lapply(rollcall.simple,function(m) dist(m %*% t(m)))

rollcall.mds <- lapply(rollcall.dist, function(d) as.data.frame(cmdscale(d,k=2)*-1))
head(rollcall.mds)

congresses <- 101:111
#add in names of senators
for(i in 1:length(rollcall.mds)){
  names(rollcall.mds[[i]]) <- c('x','y')
  congress <- subset(rollcall.data[[i]],state<99)
  congress.names <- sapply(as.character(congress$name), function(n) strsplit(n, "[, ]")[[1]][1])
  rollcall.mds[[i]] <- transform(rollcall.mds[[i]], name=congress.names, 
                                 party=as.factor(congress$party), congress=congresses[i])
}

#lets check out 11th congress
ggplot(rollcall.mds[[11]],aes(x=x,y=y))+
         scale_shape(name="Party", breaks=c("100","200","328"), labels=c("Dem.", "Rep.", "Ind."), solid=FALSE)+
         scale_color_manual(name="Party", values=c("100"="black","200"="dimgray", "328"="grey")) +
        geom_point(aes(color=party))
  
