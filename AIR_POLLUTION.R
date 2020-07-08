pollutantmean <- function(directory,pollutant, id = 1:332){
  data <- list.files(directory,full.names = T)
  mns<-c()
  for(i in id){
    clear_data <- na.omit(read.csv(data[i]))
    if(length(clear_data!=0)){
      mns <- c(mns,mean(clear_data[[pollutant]]))
    }
  }
  mean(mns)
}

complete <- function(directory,id = 1:332){
  dat <- data.frame()
  data <- list.files(directory,full.names = T)
  for(i in id){
    clear_data <- na.omit(read.csv(data[i]))
    dat <- rbind(dat,c(i,nrow(clear_data)))
  }
  colnames(dat)<-c("id","nobs")
  dat
}

corr <- function(directory, threshold = 0){
  data <- list.files(directory,full.names = T)
  dat <- c()
  for(i in 1:332){
    clear_data <- na.omit(read.csv(data[i]))
    if(nrow(clear_data)>threshold){
      dat <- c(dat,cor(clear_data$sulfate,clear_data$nitrate))
    }
  }
  dat
}