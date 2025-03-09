rm(list = ls())

setwd("D:/MS Data Analytics/Fall 2024/DAT 511/Week 6/Data")

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Validate state input
  if (!(state %in% unique(data$State))) {
    stop("invalid state")
  }
  
  ## Validate outcome input (Only allow "heart attack")
  if (outcome != "heart failure") {
    stop("invalid outcome")
  }
  
  ## Identify the correct column for heart attack mortality rate
  outcome_col <- 17  # Column for heart attack 30-day mortality rate
  
  ## Filter data for the given state
  state_data <- data[data$State == state, ]
  
  ## Convert outcome column to numeric and remove NA values
  state_data[, outcome_col] <- suppressWarnings(as.numeric(state_data[, outcome_col]))
  state_data <- state_data[!is.na(state_data[, outcome_col]), ]
  
  ## Order data by outcome rate and hospital name
  state_data <- state_data[order(state_data[, outcome_col], state_data$Hospital.Name), ]
  
  ## Determine rank to return
  if (num == "best") {
    return(state_data$Hospital.Name[1])  # Best hospital (lowest rate)
  } else if (num == "worst") {
    return(state_data$Hospital.Name[nrow(state_data)])  # Worst hospital (highest rate)
  } else if (is.numeric(num) && num > 0 && num <= nrow(state_data)) {
    return(state_data$Hospital.Name[num])  # Specific rank
  } else {
    return(NA)  # If rank exceeds number of hospitals
  }
}

rankhospital ("TX", "heart failure", 4)
rankhospital ("TX", "heart failure", "best")
rankhospital ("TX", "heart failure", "worst")
rankhospital ("TX", "heart failure", 10000)