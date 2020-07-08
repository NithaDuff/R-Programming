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