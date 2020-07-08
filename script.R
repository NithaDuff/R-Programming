myfn <- function(){
  x<- rnorm(100)
  mean(x)
}

scnd <- function(x){
  x+rnorm(length(x))
}