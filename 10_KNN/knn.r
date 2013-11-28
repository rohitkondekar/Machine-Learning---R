################################################
# Recommendation System for R Packages
# Simplified Data
# Algorithm KNN
# Similarity between packages is calculated using correlation
# Then correlation is converted to distance measure
################################################

library(class)
library(reshape) #for cast 
setwd("/Projects/Machine Learning R/10_KNN")

installations <- read.csv(file="./data/installations.csv")
head(installations)

#Converting Long Form to Wide Form

user.package.matrix <- cast(data=installations,formula=User~Package,value='Installed')
head(user.package.matrix)

#coding user information as row names
row.names(user.package.matrix) <- user.package.matrix$User
user.package.matrix <- user.package.matrix[2:length(user.package.matrix)]

similarities<-cor(user.package.matrix)

# Now we need to convert this correlation to a distance measure to feed it to KNN
# we need -1 to be infinity and 1 to be 0
# transformation: log(val/2+0.5)

distances <- -log((similarities/2)+0.5)
head(distances)

#get k nearest neighbours
# order sorts the elements themself according to their actuall value rather than sorting the values.
k.nearest.neighbors <- function(df, i, k=25){
  return(order(df[i,])[2:(k+1)])
}

k.nearest.neighbors(distances, 1)
#once we get the neighbors - the probability is how many of them have been installed
#i.e. mean of those
installation.probability <- function(user, package, user.package.matrix, distances, k=25){
  neighbors <- k.nearest.neighbors(distances, package,k)
  return(mean(sapply(neighbors, function(neighbor) return(user.package.matrix[user,neighbor]))))
}

installation.probability(1, 1, user.package.matrix, distances)


most.probable.packages <- function(user, user.package.matrix, distances, k = 25) {
  return(order(sapply(1:ncol(user.package.matrix), function (package)
    {
      installation.probability(user,package, user.package.matrix, distances,k=k)
    }), decreasing = TRUE))}


user <- 1
listing <- most.probable.packages(user, user.package.matrix, distances)
print(names(user.package.matrix)[listing[1:10]])