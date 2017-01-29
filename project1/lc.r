getGoodData <- function(filename) {
  lcData <- read.csv(filename)
  library(dplyr)
  
  baseData <- select(lcData, int_rate, loan_status, loan_amnt, grade, emp_length, 
                     home_ownership, annual_inc, delinq_2yrs, dti)
  baseData <- arrange(baseData, loan_status)
  # split(baseData, start:end)
  return(baseData)
}

getGoodStanding <- function(df) {
  return(filter(df, !(loan_status %in% c("Charged Off", "Default"))))
}

getDefault <- function(df) {
  return(filter(df, loan_status %in% c("Charged Off", "Default")))
}

intRateToNumeric <- function(df) {
  df$int_rate <- sapply(df$int_rate, function(x) as.numeric(substr(x, 1, nchar(as.character(x))-1)))
  return(df)
}