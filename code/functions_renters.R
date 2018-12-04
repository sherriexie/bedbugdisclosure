# ----------------------------------------------------------------------------
# SetODEs_renters function:
# ------------------------
# This is the same as the SetODEs function with additional equations to track
# the average number of moveouts and the average months spent infested to
# calculate moving costs and costs of unterated infection, respectively.


SetODEs_renters <-function(t, y, p){
  Sr <- y[1]
  Ir <- y[2]
  Sv <- y[3]
  Iv <- y[4]
  Sv2 <- y[5]
  trt <- y[6]
  mov <- y[7]
  inf <- y[8]
  
  # Note f(t) = b*Ir/(Sr+b*Ir)
  with(as.list(p),{
    dSr.dt <- -beta*Sr*Ir/N + gamma*Ir + n*(1-d)*(1-k*b*Ir/(Sr+b*Ir))*Sv2 + n*(1-k*b*Ir/(Sr+b*Ir))*Sv - m*Sr
    dIr.dt <- beta*Sr*Ir/N + n*k*b*Ir/(Sr+b*Ir)*Sv + n*(1-d)*k*b*Ir/(Sr+b*Ir)*Sv2 + n*(1-d)*Iv - gamma*Ir - b*m*Ir 
    dSv.dt <- m*Sr + 1/D*Sv2 - n*Sv 
    dIv.dt <- b*m*Ir - gamma*Iv - n*(1-d)*Iv
    dSv2.dt <- gamma*Iv - n*(1-d)*Sv2 - 1/D*Sv2
    dtrt.dt <- gamma*Ir + gamma*Iv
    dmov.dt <- m*Sr + b*m*Ir
    dinf.dt <- Ir + Iv
    return(list(c(dSr.dt, dIr.dt, dSv.dt, dIv.dt, dSv2.dt, dtrt.dt, dmov.dt, dinf.dt)))
  })
}

# ----------------------------------------------------------------------------
# GetCost_renters function:
# ----------------
# Same as GetCost function, but calculates costs to renters rather than 
# landlords
# Inputs: 
#   - Vector of parameter values & initial conditions
#   - Vector of bed bug-related costs
#   - Vector of years over which to run the simulation
# Output: 
#   - A data frame tracking the total and component costs, as well as 
#     prevalence for each year of the simulation
#   - This output is used to plot Figure 3

GetCost_renters <- function(p.set, renter.costs, years){
  
  # Set parameter values
  gamma <- p.set[1]
  k     <- p.set[2]
  b     <- p.set[3]
  m     <- p.set[4]
  n     <- p.set[5]
  N     <- p.set[6]
  D     <- p.set[7]
  beta  <- p.set[8]
  d     <- p.set[14]
  
  # Set initial conditions and time interval
  Sr0 <- p.set[10]
  Ir0 <- p.set[11]
  Sv0 <- p.set[12]
  Iv0 <- p.set[13]
  Sv20 <- 0  # Sv20 is set to 0 because we assume disclosure begins at time 0
  trt0 <- 0  # set treatment counter to 0 at time 0
  mov0 <- 0  # set move-out counter to 0 at time 0
  inf0 <- 0  # set infestation counter to 0 at time 0
  y0 <- c(Sr0, Ir0, Sv0, Iv0, Sv20, trt0, mov0, inf0)
  t <- seq(from=0, to=365*max(years)+1, by=1)
  
  # Set bed bug related costs to renters (ancillary treatment costs, 
  # moving costs, and costs of untreated infestation)
  ctrt <- renter.costs[1] 
  cmov <- renter.costs[2]
  cinf <- renter.costs[3]
  
  # We model the absence of disclosure by setting the disclosure index d = 0
  p <- list(beta=beta, gamma=gamma, b=b, d=d, D=D, k=k, m=m, n=n, N=N)
  p0 <- list(beta=beta, gamma=gamma, b=b, d=0, D=D, k=k, m=m, n=n, N=N)
  
  # We solve the ODE's for the model in the presence and absence of disclosure 
  # (out and out0, respectively)
  out <- ode(y=y0, times=t, func=SetODEs_renters, parms=p)
  out0 <- ode(y=y0, times=t, func=SetODEs_renters, parms=p0)
  
  # We use a for loop to calculate total and component costs for each year of 
  # the simulation
  cost <- trt <- mov <- inf <- prev <- pvac <- as.numeric()
  
  for(jj in 1:length(years)){
    
    # Get the first and last day of year jj
    first.day <- years[jj]*365 - 364
    last.day <- years[jj]*365 + 1
    
    # Calculate the DIFFERENCE between the disclosure and no disclosure simulations 
    # in the number of treatments (n.trt), move-outs (n.mov), and days infested (n.inf) 
    # that fell on year jj
    n.trt <- (out[,7][last.day] - out[,7][first.day]) - 
                (out0[,7][last.day] - out0[,7][first.day])
    n.mov <- (out[,8][last.day] - out[,8][first.day]) - 
                (out0[,8][last.day] - out0[,8][first.day])
    n.inf <- (out[,9][last.day] - out[,9][first.day]) - 
                (out0[,9][last.day] - out0[,9][first.day])
    
    # Total per unit treatment cost = 
    # (# of treatments) x (avg cost of bed bug treatment) / (total # units)
    trt[jj] <- n.trt*ctrt/N
    
    # Total per unit turnover cost = 
    # (# of turnover events) x (avg cost of turnover) / (total # units)
    mov[jj] <- n.mov*cmov/N
    
    # Total per unit vacancy cost = 
    # (# months vacant) x (average monthly rent) / (total # units)
    # Note: # months = # days / 30
    inf[jj] <- (n.inf/30)*cinf/N
    
    # Total cost is equal to the sum of the component costs
    cost[jj] <- trt[jj] + mov[jj] + inf[jj]
    
    # Prevalence at the end of year jj is simply the number of units in the 
    # Ir and Iv classes on the last day of the year divided by N
    prev[jj] <- (out[,3][last.day] + out[,5][last.day])/N
  }
  
  df <- data.frame(Year = years, Total_Cost = cost, Treatment = trt, 
                   Moveout = mov, Untreated_Infestation = inf,
                   Prevalence = prev)
  return(df)
}

# ----------------------------------------------------------------------------
# GetCost2 function:
# ----------------
#  - Same as GetCost2 function adapted to calculate cost to renters
#  - This function is used in the GetCostMatrix_renters function

GetCost2_renters <- function(p.set, bbcosts, year){
  
  # Set parameter values
  gamma <- p.set[1]
  k <- p.set[2]
  b <- p.set[3]
  m <- p.set[4]
  n <- p.set[5]
  N <- p.set[6]
  D <- p.set[7]
  beta <- p.set[8]
  d <- p.set[14]
  
  # Set initial conditions and time interval
  Sr0 <- p.set[10]
  Ir0 <- p.set[11]
  Sv0 <- p.set[12]
  Iv0 <- p.set[13]
  Sv20 <- 0  # Sv20 is set to 0 because we assume disclosure begins at time 0
  trt0 <- 0  # set treatment counter to 0 at time 0
  mov0 <- 0  # set turnover counter to 0 at time 0
  inf0 <- 0  # set infestation counter to 0 at time 0
  y0 <- c(Sr0, Ir0, Sv0, Iv0, Sv20, trt0, mov0, inf0)
  t <- seq(from=0, to=365*max(years)+1, by=1)
  
  # Set bed bug related costs to renters (ancillary treatment costs, 
  # moving costs, and costs of untreated infestation)
  ctrt <- renter.costs[1] 
  cmov <- renter.costs[2]
  cinf <- renter.costs[3]
  
  # We model the absence of disclosure by setting the disclosure index d = 0
  p <- list(beta=beta, gamma=gamma, b=b, d=d, D=D, k=k, m=m, n=n, N=N)
  p0 <- list(beta=beta, gamma=gamma, b=b, d=0, D=D, k=k, m=m, n=n, N=N)
  
  # We solve the ODE's for the model in the presence and absence of disclosure 
  # (out and out0, respectively)
  out <- ode(y=y0, times=t, func=SetODEs, parms=p)
  out0 <- ode(y=y0, times=t, func=SetODEs, parms=p0)
  
  # We use a for loop to calculate total and component costs for each year of 
  # the simulation
  cost <- trt <- mov <- inf <- as.numeric()
  
  first.day <- length(t) - 364
  last.day <- length(t)
  
  # Calculate the DIFFERENCE between the disclosure and no disclosure simulations 
  # in the number of treatments (n.trt), move-outs (n.mov), and days infested (n.inf) 
  # that fell on year jj
  n.trt <- (out[,7][last.day] - out[,7][first.day]) - 
            (out0[,7][last.day] - out0[,7][first.day])
  n.mov <- (out[,8][last.day] - out[,8][first.day]) - 
            (out0[,8][last.day] - out0[,8][first.day])
  n.inf <- (out[,9][last.day] - out[,9][first.day]) - 
           (out0[,9][last.day] - out0[,9][first.day])
  
  # Calculate component costs and total cost
  trt <- n.trt*ctrt/N
  mov <- n.mov*cmov/N
  inf <- (n.inf/30)*cinf/N
  cost <- trt + mov + inf
  names(trt) <- names(mov) <- names(inf) <- NULL
  
  return(c(total.cost=cost, treatment.cost=trt, moving.cost=mov, 
           infestation.cost=inf))
}

# ----------------------------------------------------------------------------
# GetCostMatrix_renters function:
# ------------------------------
# Same as GetCostMatrix function but adapted to calculate cost matrix from the
# perspective of renters
#
# Inputs: 
#   - Range of baseline prevalenc values
#   - Range of disclosure index values
#   - Number of years to run the simulation
# Output: 
#   - A matrix with separate columns for total cost and component costs with 
#     respect to renters
#   - Each row of the matrix represents a different combination of baseline \
#     prevalence & disclosure index values

GetCostMatrix_renters <- function(bprev.values, d.values, nyears){
  preparam <- SetParameters()
  bbcosts <- SetCost()
  # Get a vector of beta values that correspond to the range of baseline 
  # prevalence values we are looking to range our simulations over
  beta.values <- sapply(bprev.values, function(x) GetBeta(preparam, x))
  
  # Make a n x 9 matrix of parameter values where there is one row per beta/
  # baseline prevalence value
  reps <- length(bprev.values)
  preparmatrix <- cbind(t(replicate(reps, preparam)), beta.values, bprev.values)
  colnames(preparmatrix) <- NULL
  
  # Get initial conditions (Sr0, Ir0, etc.) for each beta/baseline prevalence
  # value
  init.matrix <- apply(preparmatrix, 1, function(x) GetInit(x))
  # Transpose the initial conditions matrix so that each row represents a set
  # of initial conditions
  init.matrix <- t(init.matrix)
  # Append initial conditions to parameter matrix 
  preparmatrix <- cbind(preparmatrix, init.matrix)
  colnames(preparmatrix) <- NULL
  
  # Expand parameter matrix to include disclosure index values where each row 
  # is a different combination of baseline prevalence and disclosure index 
  # values (there should be n1 x n2 rows total if n1 = number of baseline 
  # prevalence values and n2 = number of disclosure index values)
  d.values.reps <- rep(d.values, each=reps)
  reps2 <- length(d.values)
  parmatrix <- cbind(preparmatrix[rep(seq_len(nrow(preparmatrix)),reps2),],
                     d.values.reps)
  
  costs <- apply(parmatrix, 1, function(x) GetCost2_renters(x, bbcosts, nyears))
  costs <- t(costs)
  
  costs2 <- cbind(costs, baseline.prevalence = parmatrix[,9], 
                  disclosure.index = parmatrix[,14])
  
  return(costs2)  
}

