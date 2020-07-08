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