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
  # newer data sets would use `filter(df, !(loan_status %in% c("Charged Off", "Default")))`
  return(filter(df, loan_status == "Fully Paid"))
}

getDefault <- function(df) {
  # newer data sets would us `filter(df, loan_status %in% c("Charged Off", "Default"))`
  return(filter(df, loan_status == "Charged Off"))
}

intRateToNumeric <- function(df) {
  df$int_rate <- sapply(df$int_rate, function(x) as.numeric(substr(x, 1, nchar(as.character(x))-1)))
  return(df)
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# not a very useful feature
intRateAnalysis <- function(df) {
  library(lattice)
  
  par <- getGoodStanding(df)
  subpar <- getDefault(df)
  
  # somewhat useful
  summary(par$int_rate)
  summary(subpar$int_rate)
  
  # slightly more useful - probabl won't use all for the interest rate feature
  bwplot(df$loan_status ~ df$int_rate)
  hist(subpar$int_rate)
  hist(par$int_rate)
}

# not a very useful feature
loanAmntAnalysis <- function(df) {
  library(lattice)
  par <- getGoodStanding(df)
  subpar <- getDefault(df)
  
  bwplot(df$loan_status ~ df$loan_amnt)
  hist(subpar$loan_amnt)
  hist(par$loan_amnt)
} 

empLengthAnalysis <- function(df) {
  library(lattice)
  par <- getGoodStanding(df)
  subpar <- getDefault(df)
  
  plot(par$emp_length)
  plot(subpar$emp_length)
}

homeOwnershipAnalysis <- function(df) {
  library(lattice)
  par <- getGoodStanding(df)
  subpar <- getDefault(df)

  # percent of good loans and home ownership status
  parTab <- table(par$home_ownership)/nrow(par) * 100
  print(parTab)
  # percent of bad loans and home ownership status
  subparTab <- table(subpar$home_ownership)/nrow(subpar) * 100
  print(subparTab)
  
  barplot(parTab)
  barplot(subparTab)
}

annualIncAnalysis <- function(df) {
  library(lattice)
  par <- getGoodStanding(df)
  subpar <- getDefault(df)
  
  mean(df$annual_inc, na.rm=TRUE)
  mean(df$annual_inc, na.rm=TRUE, trim=.2)
  mean(df$annual_inc, na.rm=TRUE, trim=.1)
  
  avgIncPar <- mean(par$annual_inc, na.rm=TRUE, trim=.2)
  avgIncSubpar <- mean(subpar$annual_inc, na.rm=TRUE, trim=.2)
  print(paste("Income difference: ", as.character((1 - (avgIncSubpar/avgIncPar))*100), "%", sep=""))
  bwplot(par$loan_status ~ par$annual_inc, xlim=c(0, 150000))
  bwplot(subpar$loan_status ~ subpar$annual_inc, xlim=c(0, 150000))
}



