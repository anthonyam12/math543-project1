run <- function(filename) {
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