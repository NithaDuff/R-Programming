rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  li <- list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
             "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
             "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
  
  cl <- data.frame(out,stringsAsFactors = FALSE)
  sp <- split(cl,cl$State)
  d <- li[[outcome]]
  fin <- matrix()
  res <- data.frame() 
  
  if(is.null(d)){
    message("Enter valid outcome.")
    stop()
  }
  if(num == "best"){
    num <- 1
  }
  else if(num=="worst"){
    num <- length(rank)
  }
  for(s in sp){
    rank <- order(as.numeric(s[[d]]),s$Hospital.Name)
    res <- rbind(res,s[rank[num],c("Hospital.Name","State")])
  }
  res
}