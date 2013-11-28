#Dimensionality Reduction
#PCA - principle component analysis
#Replace 25 columns with just 1 column depicting all the information
#We want to create a single column that tells us how the market is doing on each day 
#by combining information in the 25 columns that we have access to; 
#we’ll call that single column an index of the market. 

#The main idea of PCA is to create a new set of 25 columns that are ordered based on 
#how much of the raw information in our data set they contain. 
#The first new column, called the first principal component, or just the principal component 
#for short, will often contain the vast majority of the structure in the entire data set.

library(ggplot2)
library(reshape)
library(lubridate) # for getting date out of string
setwd("/Projects/Machine Learning R/8_PCA/data/")

prices<-read.csv(file="./stock_prices.csv")
head(prices)
str(prices)

prices$Date <- as.character(prices$Date)
prices$Date <- ymd(prices$Date)

prices <- subset(prices, Date!= ymd("2002-02-01"))
prices <- subset(prices, Stock != 'DDR')

date.stock.matrix<-cast(data=prices,formula=Date~Stock,value='Close')

#to check out if any NA left out
for(i in 1:length(date.stock.matrix)){
  print(c(i,any(is.na(date.stock.matrix[,i]))))
}

#length(date.stock.matrix$DDR[is.na(date.stock.matrix$DDR)])
#date.stock.matrix <- date.stock.matrix[complete.cases(date.stock.matrix),]
#str(date.stock.matrix)

cor.matrix <- cor(date.stock.matrix[2:26])

#get a sense of correlation to find out whether PCA is possible
correlations <- as.numeric(cor.matrix)

ggplot(data.frame(Correlations=correlations),aes(x=Correlations,fill=1))+
  geom_density()+
  theme(legend.position='none')

#Correlation seems positive and PCA is suitable

#PCA Step - using princomp
pca <- princomp(date.stock.matrix[,2:ncol(date.stock.matrix)])

#get the loadings for 1st PCA
principal.component <- pca$loadings[,1]

ggplot(data.frame(principal.component),aes(x=principal.component,fill=1))+
  geom_density()+
  theme(legend.position='none')

market.index <- predict(pca)[,1]

#As we can see the values returned are negative
#to just verify they are in accordance
#we will compare it with orginial index - DJI - Dow Jones Index

dji <- read.csv(file="./DJI.csv")
head(dji)

dji$Date <- ymd(as.character(dji$Date))
dji <- subset(dji, Date > ymd('2001-12-31')) 
dji <- subset(dji, Date != ymd('2002-02-01'))

dji.date <- with(dji,rev(Date))
dji.close <- with(dji,rev(Close))

comparison <- data.frame(Date = dji.date, MarketIndex = market.index, DJI = dji.close)

head(comparison)
ggplot(comparison,aes(x=MarketIndex,y=DJI))+
  geom_point()+
  geom_smooth(method = 'lm', se = TRUE)

#negate the values to get it in form
ggplot(comparison,aes(x=-MarketIndex,y=DJI))+
  geom_point()+
  geom_smooth(method = 'lm', se = TRUE)

comparison$MarketIndex <- -scale(comparison$MarketIndex)
comparison$DJI <- scale(comparison$DJI)

#Now compare both indexs according to date
alt.comparison <- melt(comparison,id.vars='Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison,aes(x=Date,y=Price,group=Index,color=Index))+
  geom_line()+
  geom_point()
