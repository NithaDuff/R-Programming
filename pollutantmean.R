pollutantmean <- function(directory,pollutant, id = 1:332){
  data <- list.files(directory,full.names = T)
  mns<-c()
  for(i in id){
    clear_data <- na.omit(read.csv(data[i]))
    mns <- c(mns,mean(clear_data[[pollutant]]))
  }
  mean(na.omit(mns))
}