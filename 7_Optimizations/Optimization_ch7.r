###########################
#Chapter - 7
#Optimization and breaking codes
###########################

setwd("/Projects/Machine Learning R/7_Optimizations")
height.to.weight <- function(height,a,b){
  return(a+b*height)
}

heights.weights <- read.csv(file="./01_heights_weights_genders.csv")
coef(lm(heights.weights$Weight~heights.weights$Height))

#original value: 
#(Intercept) heights.weights$Height 
#-350.737192               7.717288 

#Lets do this using optimization

squared.error <- function(heights.weights,a,b){
  predictions <- with(heights.weights,height.to.weight(Height,a,b))
  errors <- with(heights.weights,Weight-predictions)
  return(sum(errors^2))
}

root.mean.squared.error <- function(heights.weights,a,b){
  predictions <- with(heights.weights,height.to.weight(Height,a,b))
  errors <- with(heights.weights,Weight-predictions)
  return(sqrt(sum(errors^2)/length(predictions)))
}

#to get a sense
for (a in seq(-1, 1, by = 1)) {
  for (b in seq(-1, 1, by = 1)) {
    print(squared.error(heights.weights, a, b)) }
}

#using optim function
optim(par=c(0,0),fn=function(x){
  root.mean.squared.error(heights.weights,x[1],x[2])
})

#gives approx same result as lm

########################################################################

english.letters <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                     'w', 'x', 'y', 'z')

caesar.cipher <- list()
inverse.caesar.cipher <- list()

for(index in 1:length(english.letters)){
  caesar.cipher[[english.letters[index]]] <- english.letters[index %% 26 + 1]
  inverse.caesar.cipher[[english.letters[index %% 26 + 1]]] <- english.letters[index]
}

print(caesar.cipher)

#function to translate string
apply.cipher.to.string <- function(string,cipher){
  output <- ''
  for (i in 1:nchar(string)) {
    output <- paste(output,cipher[[substr(string,i,i)]],sep="")
  }
  return(output)
}

apply.cipher.to.text <- function(text, cipher) {
  output <- c()
  for (string in text)
  {
    output <- c(output, apply.cipher.to.string(string, cipher))
  }
  return(output)
}

apply.cipher.to.text(c('sample', 'text'), caesar.cipher)

