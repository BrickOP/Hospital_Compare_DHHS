rm(list = ls())

setwd("D:/MS Data Analytics/Fall 2024/DAT 511/Week 6/Data")

#Reading the file 
outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
head(outcome)

#Plotting a histogram of the 30-day death rates for heart attacks

outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11], main = "30-Day Mortality Rates for Heart Attack",
     xlab = "Mortality Rate", ylab = "Frequency", col = "blue")

# Finding the best hospital

best <- function(state, outcome) {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Validate state input
  if (!(state %in% unique(data$State))) {
      stop("invalid state")
  }
  
  # Validate outcome input
  if (!(outcome == "heart attack")) {
    stop("invalid outcome")
  }
  
  # Extract relevant data for the given state and outcome
  state_data <- data[data$State == state, c(2, 11)]  # Hospital name and outcome column
  colnames(state_data) <- c("Hospital", "Rate")  # Rename columns for clarity
  
  # Convert mortality rate column to numeric and remove NAs
  state_data$Rate <- as.numeric(state_data$Rate)
  state_data <- state_data[!is.na(state_data$Rate), ]
  
  # Identify the hospital(s) with the lowest mortality rate
  min_rate <- min(state_data$Rate)
  best_hospitals <- state_data[state_data$Rate == min_rate, "Hospital"]
  
  # Return the first hospital in alphabetical order
  return(sort(best_hospitals)[1])
}

best ("TX", "heart attack")
best ("ZZ", "heart attack")
best ("TX", "wrong outcome")
