run <- function(filename) {
  lcData <- read.csv(filename)
  library(dplyr)
  
  attach(lcData)
  baseData <- select(lcData, int_rate, loan_status, loan_amnt, grade)
  baseData <- arrange(baseData, loan_status)
  # split(baseData, start:end)
}

percentColToDouble <- function(df, colName) {
  for(row in df[,colName]) {
    df[row,colName] <- percentToDouble(df[row, colName])
  }
  return(df)
}

percentToDouble <- function(pct) {
  return(1.0)
}

# 1-Charged Off, 2-Current, 3-Default, 4-Fully Paid, 5-In Grace Period, 6-Late(16-30 days), 7-Late(31-120 days)
numericLoanStatus <- function(df) {
  numbers <- 1:7
  print(numbers)
}