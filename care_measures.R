outcome0 <- "pneumonia"
state <- "WI"
num <- "worst"

## Read outcome data
outcome <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")

state_all <- unique(outcome$State)
outcome_all <- c("heart attack", "heart failure", "pneumonia")


## Check that state and outcome are valid
if (!is.element(state, outcome$State)) {
  stop("invalid state")
}

if (!is.element(outcome0, outcome_all)) {
  stop("invalid outcome")
} else if (outcome0 == "heart attack") {
  c0 <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
} else if (outcome0 == "heart failure") {
  c0 <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
} else if (outcome0 == "pneumonia") {
  c0 <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
}


## Return hospital name in that state with lowest 30-day death rate

temp <-  as.numeric(outcome[outcome$State == state & outcome[, c0] != "Not Available", c0])
temp2 <-  outcome[outcome$State == state & outcome[, c0] != "Not Available", ]
r0 <- rank(temp, ties.method = "min")
r1 <- r0
for (i in 1:length(temp))
{
  t1 <- which(r0==i)
  if( length(t1)>1) {
    
    temp3 <- temp2[t1, ]
    temp4 <- temp3$Hospital.Name
    r2 <- rank(temp4)
    r0[t1] <- i-1+r2
  }
}




if (num=="best") { 
  num <-1
}

if (num=="worst"){
  num <- length(temp)
}

if (num > length(temp)) {
  temp2[r0[num], "Hospital.Name"]
  
} else {

    temp2[which(r0 == num), "Hospital.Name"]
}
