library(reshape2)
library(ggplot2)


# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

# Source the functions we will need for our analysis
source("code/functions.R")

# Set renter selectivity (s).
s <- 0.5

GetSuppTable <- function(preparam, p){
  beta <- GetBeta(preparam, p)
  param <- c(preparam, beta, base.prev = p)
  init <- GetInit(param)
  param.init <- c(param, init, d = s)
  years <- c(1:100)
  bbcosts <- SetCost()
  df <- GetCost(param.init, bbcosts, years)
  maxcost <- max(df$Total_Cost)
  cost10 <- df$Total_Cost[10]
  prev100 <- df$Prevalence[100]
  beta.year <- beta*365
  names(beta.year) <- NULL
  out <- c(beta = beta.year, peak_cost = maxcost, cost_at_10_years = cost10, 
           prev_at_100_years = prev100)
  return(out)
}

# Case: none
preparam <- SetParameters()
GetSuppTable(preparam, p = 0.05)

# Case: treatment rates subpop 2
preparam <- SetParameters()
preparam[1] <- 1/365
GetSuppTable(preparam, p = 0.25)

# Case: tenant turnover rate subpop 1
preparam <- SetParameters()
preparam[4] <- .2/365
preparam[5] <- 2.4/365
GetSuppTable(preparam, p = 0.03)

# Case: tenant turnover rate subpop 2
preparam <- SetParameters()
preparam[4] <- 1/365
preparam[5] <- 12/365
GetSuppTable(preparam, p = 0.12)

# Case: aversion to bed bugs subpop 2
preparam <- SetParameters()
preparam[3] <- 1.3  
preparam[2] <- 1  
GetSuppTable(preparam, p = 0.25)






