rankall <- function(outcome = FALSE, num = "best") {
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    column <- if (outcome == "heart attack") {
        outcomes[, 11] <- as.numeric(outcomes[, 11])
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        outcomes[, 17] <- as.numeric(outcomes[, 17])
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        outcomes[, 23] <- as.numeric(outcomes[, 23])
        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("invalid outcome")
    }
    
    data_by_state <- split(outcomes[, c("Hospital.Name", "State", column)], outcomes$State)
    
    ranking_hospital <- function(state_data, num) {
        ordered_state_data <- order(state_data[3], state_data$Hospital.Name, na.last=NA)
        
        if (num == "best") {
            state_data$Hospital.Name[ordered_state_data[1]]
        } else if (num == "worst") {
            state_data$Hospital.Name[ordered_state_data[length(ordered_state_data)]]
        } else if (is.numeric(num)) {
            state_data$Hospital.Name[ordered_state_data[num]]
        } else {
            stop("invalid num")
        }
    }
    
    hospitals_of_rank <- lapply(data_by_state, ranking_hospital, num)
    
    data.frame(hospital = unlist(hospitals_of_rank), state = names(hospitals_of_rank))
}