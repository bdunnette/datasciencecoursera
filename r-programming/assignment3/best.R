best <- function(state=FALSE, outcome=FALSE) {
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeChoices <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if(state == FALSE || !state %in% outcomes$State) stop ("invalid state")
    if(outcome == FALSE || !outcome %in% outcomeChoices) stop ("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death


    ## rate


}