rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  li <- list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
             "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
             "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
  
  cl <- data.frame(out,stringsAsFactors = FALSE)
  s <- split(cl,cl$State)
  d <- li[[outcome]]
  stateHsp <- s[[state]]
  
  if(is.null(stateHsp)){
    message("Enter valid State")
    stop()
  }
  if(is.null(d)){
    message("Enter valid outcome.")
    stop()
  }
  
  stateHsp <- stateHsp[complete.cases(stateHsp),]
  rank <- order(as.numeric(stateHsp[[d]]),stateHsp$Hospital.Name)
  if(num == "best"){
    num <- 1
  }
  else if(num=="worst"){
    num <- length(rank)
  }
  stateHsp[rank[num],"Hospital.Name"]
}
