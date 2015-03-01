rankhospital <- function(state=FALSE, outcome=FALSE, num = "best") {
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeChoices <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if(state == FALSE || !state %in% outcomes$State) stop ("invalid state")
    if(outcome == FALSE || !outcome %in% outcomeChoices) stop ("invalid outcome")
    
    ## Return hospital name in that state with the given rank 30-day death rate
    
    # Get all hospitals in the specified state
    stateHospitals <- outcomes[outcomes$State == state,]
    
    # Would be ideal to pass outcome to function rather than repeating if/elsesource("submitscript3.R")
    if (outcome == "heart attack") {
        # Coerce mortality to numeric value, so we can eliminate NAs
        stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        # Remove hospitals without mortality value
        stateHospitals <- stateHospitals[complete.cases(stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
        # Sort hospitals by given outcome
        stateHospitals <- stateHospitals[order(stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, stateHospitals$Hospital.Name),]
    } else if (outcome == "heart failure") {
        # Coerce mortality to numeric value, so we can eliminate NAs
        stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        # Remove hospitals without mortality value
        stateHospitals <- stateHospitals[complete.cases(stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
        # Sort hospitals by given outcome
        stateHospitals <- stateHospitals[order(stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, stateHospitals$Hospital.Name),]
    } else if (outcome == "pneumonia") {
        # Coerce mortality to numeric value, so we can eliminate NAs
        stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        # Remove hospitals without mortality value
        stateHospitals <- stateHospitals[complete.cases(stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
        # Sort hospitals by given outcome
        stateHospitals <- stateHospitals[order(stateHospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, stateHospitals$Hospital.Name),]
    }  
    # Print name of hospital with lowest value for given outcome
    if (num == 'best') {
        print(stateHospitals[1,2])
    } else if (num == 'worst') {
        print(stateHospitals[nrow(stateHospitals),2])
    } else {
        print(stateHospitals[num,2])
    }
    
}