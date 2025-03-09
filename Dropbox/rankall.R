rm(list = ls())

setwd("D:/MS Data Analytics/Fall 2024/DAT 511/Week 6/Data")

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Validate outcome input and assign correct column
  if (outcome == "heart attack") {
    outcome_col <- 11
  } else if (outcome == "heart failure") {
    outcome_col <- 17
  } else {
    stop("invalid outcome")
  }
  
  ## Convert the selected outcome column to numeric
  data[, outcome_col] <- suppressWarnings(as.numeric(data[, outcome_col]))
  
  ## Split data by state
  state_groups <- split(data, data$State)
  
  ## Function to get hospital by rank for each state
  get_ranked_hospital <- function(state_data) {
    state_data <- state_data[!is.na(state_data[, outcome_col]), ]  # Remove NAs
    
    ## If no hospitals have data for the outcome, return NA
    if (nrow(state_data) == 0) {
      return(NA)
    }
    
    ## Sort by mortality rate, then hospital name alphabetically
    state_data <- state_data[order(state_data[, outcome_col], state_data$Hospital.Name), ]
    
    if (num == "best") {
      return(state_data$Hospital.Name[1])  # Best hospital
    } else if (num == "worst") {
      return(state_data$Hospital.Name[nrow(state_data)])  # Worst hospital
    } else if (is.numeric(num) && num > 0 && num <= nrow(state_data)) {
      return(state_data$Hospital.Name[num])  # Specific rank
    } else {
      return(NA)  # If rank exceeds number of hospitals
    }
  }
  
  ## Apply function to each state and create data frame
  hospital_list <- sapply(state_groups, get_ranked_hospital)
  
  result <- data.frame(hospital = hospital_list, state = names(hospital_list), row.names = NULL)
  
  return(result)
}


head(rankall("heart attack", 20) , 10)
head(rankall("heart attack", "best") , 10)
head(rankall("heart attack",20) , 10)
tail(rankall("heart failure", "worst") , 5)
