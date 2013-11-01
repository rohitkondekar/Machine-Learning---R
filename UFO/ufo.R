######################## UFO File analysis ######################## ######################## 
#
# Central Question : is there any seasonal trend in the sightings?
#
# Format: "DateOccurred","DateReported","Location","ShortDescription", 
          "Duration","LongDescription"
######################### ######################## ######################## ###############

#read file using delimiter
ufo<-read.delim(file="ufo_awesome.tsv",header=FALSE,sep="\t", na.string="", stringsAsFactors=FALSE)

#give names to column
names(ufo)<-c("DateOccurred","DateReported","Location","ShortDescription", 
              "Duration","LongDescription")

#notice some rows have malformed dates
#ufo$DateOccurred<-as.Date(x=ufo$DateOccurred,format="%Y%m%d")

#find and remove bad rows
head(ufo[which(nchar(ufo$DateOccurred)>8 | nchar(ufo$DateReported)>8),1])
bad.rows<-ifelse(nchar(ufo$DateOccurred)>8 | nchar(ufo$DateReported)>8,TRUE,FALSE)
ufo<-ufo[!bad.rows,]

#convert date after removing ad rows
ufo$DateOccurred<-as.Date(x=ufo$DateOccurred,format="%Y%m%d")
ufo$DateReported<-as.Date(x=ufo$DateReported,format="%Y%m%d")

#next we need to clean location data
#split it using regular expressions

#function which takes a string as input and splits it by comma
get.location <- function(l) {
  split.location <- tryCatch(strsplit(l,",")[[1]], error=function(e) return(c(NA,NA)))
  clean.location <- gsub("^ ","",split.location)
  if(length(clean.location)>2){
    return(c(NA,NA))
  }
  else{
    return(clean.location)
  }
}

#now use the function to clean the location : using lapply
city.state<-lapply(ufo$Location,get.location)

#to form a matrix out of this 
location.matrix<-do.call(rbind,city.state)

#transform dataframe i.e. insert city and state as separate data
#could have simply added a column - but used transform
ufo <- transform(ufo, USCity=location.matrix[,1], USState=tolower(location.matrix[,2]), stringsAsFactors=FALSE)

#to remove the entries which are not from US
us.states <-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il", "in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh", "nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt", "wa","wi","wv","wy")

ufo$USState<-us.states[match(ufo$USState,us.states)]
#make cities as NA
ufo$USCity[is.na(ufo$USState)]<-NA

#use only US Data
ufo.us<-subset(ufo,!is.na(ufo$USState))

#check out the time scale : - a few outliers like 1400 reportings
summary(ufo.us$DateOccurred)

#plot graph to check
quick.hist<-ggplot(data=ufo.us, aes(x=ufo.us$DateOccurred))+geom_histogram()
  +scale_x_date(breaks="50 years")
#save it to a file
ggsave(filename="./USDateOccuredHistogram.png",plot=quick.hist,height=6,width=18)

#only 1990 - 2013 dates considered
#hence subset the data
ufo.us<-subset(ufo.us, ufo.us$DateOccurred >= as.Date("1990-01-01"))
head(ufo.us)

#now plot
quick.hist<-ggplot(data=ufo.us, aes(x=ufo.us$DateOccurred))+geom_histogram()+
  scale_x_date(breaks="5 years")

print(quick.hist)

#print number of rows
nrow(ufo.us)

#now analysis has to be done to address the central question
#is there any seasonal trend?? - insert a new column year-month - for analysis
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred,format="%Y-%m")
head(ufo.us)

#next we want to sumup all state-date things
#we use plyr function for this -- ddplyr : So ddply takes a data frame as input and returns a data frame 
#as output, and l_ply takes a list as input and returns nothing as output.

# Note the use of the '.' function to allow
# group and sex to be used without quoting
#or we could have done the same thing using:  sumup <- ddply(ufo.us, c("USState","YearMonth"), nrow)

library(plyr)
library(scales)
sumup <- ddply(ufo.us, .(USState,YearMonth), nrow)
head(sumup)

#notice few months are missing as it doesnot have value for not reporting
#need to add them to analyse

date.seq<-seq.Date(from=(min(ufo.us$DateOccurred)),to=(max(ufo.us$DateOccurred)),by="month")
head(date.seq)
length(date.seq)

#convert format
date.string<- strftime(date.seq,format="%Y-%m")
head(date.string)

#now generate a table of all states - all dates - using lapply for this and vector us.states
states.date<- lapply(us.states,function(s) cbind(s,date.string))

typeof(states.date)
#create data frame out of it
states.date <- data.frame(do.call(rbind,states.date), stringsAsFactors=FALSE)
head(states.date)

#now merge dataframes
all.sightings <- merge(x=states.date,y=sumup,by.x=c("s","date.string"),by.y=c("USState","YearMonth"),all=TRUE)
head(all.sightings)

#change names to appropriate
names(all.sightings)<-c("State","YearMonth","Sightings")
#change NAs to 0
all.sightings$Sightings[is.na(all.sightings$Sightings)]<-0
head(all.sightings)

#convert YearMonth to date using date.seq vector
all.sightings$YearMonth<-as.Date(rep(date.seq,length(us.states)))
head(all.sightings)

all.sightings$State<-as.factor(toupper(all.sightings$State))

#Now we are ready for visualization - constructing plot

state.plot <- ggplot(data=all.sightings,aes(x=YearMonth,y=Sightings))+
  geom_line(aes(color="darkblue"))+
  facet_wrap(~State,nrow=10,ncol=5)+
  theme_bw()+
  scale_color_manual(values=c("darkblue"="darkblue"),legend=FALSE)+
  scale_x_date(breaks= date_breaks("5 years"), labels = date_format("%Y"))+
  xlab("Time")+ylab("Number of Sightings")+
  labs(title="Number of UFO sightings by Month-Year and U.S. State (1990-2010)")

ggsave(filename="./final_plot.png",plot=state.plot,width=14,height=8.5)
print(state.plot)