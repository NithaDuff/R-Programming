best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate (11,17,23)
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  li <- list("Heart Attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
             "Heart Failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
             "Pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
  
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
  least <- min(as.numeric(stateHsp[[d]]),na.rm = T)
  isbest <- as.numeric(stateHsp[[d]]) == least
  stateHsp[which(isbest),"Hospital.Name"]
}