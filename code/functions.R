# -----------------------------------------------------------------------------
# SetParameters function: 
# ----------------------
# Sets parameter values equal to the point estimates given in Table 1

SetParameters <- function(){
  gamma <- 1/(6*30)
  k <- 0.3
  b <- 1.3
  m <- 1/(2*365)
  n <- 1/(2*30)
  N <- 1000
  D <- 365
  return(c(gamma=gamma, k=k, b=b, m=m, n=n, N=N, D=D))
}

# -----------------------------------------------------------------------------
# SetCost function: 
# ----------------
# Sets bed bug-related costs to literature values given in Table 1

SetCost <- function(){
  ctrt <- 1225
  ctov <- 1000
  cvac <- 1000
  return(c(cost.per.treatment = ctrt, cost.per.turnover = ctov, 
           cost.per.month.vacant = cvac))
}

# ----------------------------------------------------------------------------
# SetODEs function:
# ----------------
# This function sets up...
#   1. The ODE's that make up the disclosure model
#   2. dtrt.dt, which tracks the number of TREATMENTS. It equals the total 
#      movement out of Ir and Iv)
#   3. dtov.dt, which tracks the number of TURNOVERS. It equals  the total 
#      movement from vacant to rented units (i.e. Sv->Sr, Sv->Ir, Sv2->Sr, 
#      and Iv->Ir)
#   * 2 & 3 are used to calculate treatment and turnover costs. (Vacancy 
#     costs are calculated with the total time-amount of units that are
#     vacant, i.e. in class Sv, Sv2, and Iv)

SetODEs<-function(t,y,p){
  Sr <- y[1]
  Ir <- y[2]
  Sv <- y[3]
  Iv <- y[4]
  Sv2 <- y[5]
  trt <- y[6]
  tov <- y[7]
  vac <- y[8]
  
  # Note f(t) = b*Ir/(Sr+b*Ir)
  with(as.list(p),{
    dSr.dt <- -beta*Sr*Ir/N + gamma*Ir + n*(1-d)*(1-k*b*Ir/(Sr+b*Ir))*Sv2 + n*(1-k*b*Ir/(Sr+b*Ir))*Sv - m*Sr
    dIr.dt <- beta*Sr*Ir/N + n*k*b*Ir/(Sr+b*Ir)*Sv + n*(1-d)*k*b*Ir/(Sr+b*Ir)*Sv2 + n*(1-d)*Iv - gamma*Ir - b*m*Ir 
    dSv.dt <- m*Sr + 1/D*Sv2 - n*Sv 
    dIv.dt <- b*m*Ir - gamma*Iv - n*(1-d)*Iv
    dSv2.dt <- gamma*Iv - n*(1-d)*Sv2 - 1/D*Sv2
    dtrt.dt <- gamma*Ir + gamma*Iv
    dtov.dt <- n*Sv + n*(1-d)*Sv2 + n*(1-d)*Iv
    dvac.dt <- Sv + Sv2 + Iv
    return(list(c(dSr.dt, dIr.dt, dSv.dt, dIv.dt, dSv2.dt, dtrt.dt, dtov.dt, dvac.dt)))
  })
}

# ----------------------------------------------------------------------------
# GetBeta function: 
# ----------------
# Solves for the beta value that gives the desired baseline prevalence, i.e.
# the equilibrium prevalence in the ABSENCE of disclosure
GetBeta <- function(p, prev){
  gamma <- p[1]
  k <- p[2]
  b <- p[3]
  m <- p[4]
  n <- p[5]
  N <- p[6]
  
  Ir <- N*prev*(gamma + n) / (b*m + gamma + n)
  Iv <- N*prev*b*m / (b*m + gamma + n)
  Sv <- (gamma*Iv + N*m - N*prev*m)/(m+n)
  Sr <- N - Sv - N*prev
  f <- k*b*Ir/(Sr + b*Ir)
  
  beta <- (f*n*Sv + n*Iv - (gamma + b*m)*Ir)*(-N/(Sr*Ir))
  names(beta) <- "beta"
  return(beta)
}

# ----------------------------------------------------------------------------
# GetInit function:
# ----------------
# - Solves for the initial conditions (Sr0, Ir0, etc.) so that the disclosure 
#   simulation starts "at equilibrium" in the ABSENCE of 
#   disclosure
# - Input: parameter values and the desired baseline prevalence
# - Output: vector of Sr0, Er0, Ir0, etc. 

GetInit <- function(p){
  gamma <- p[1]
  k <- p[2]
  b <- p[3]
  m <- p[4]
  n <- p[5]
  N <- p[6]
  beta <- p[8]
  prev <- p[9]
  
  Ir <- N*prev*(gamma + n) / (b*m + gamma + n)
  Iv <- N*prev*b*m / (b*m + gamma + n)
  Sv <- (gamma*Iv + N*m - N*prev*m)/(m+n)
  Sr <- N - Sv - N*prev
  
  init.par <- c(Sr, Ir, Sv, Iv)
  names(init.par) <- c("Sr", "Ir", "Sv", "Iv")
  return(init.par)
}

# ----------------------------------------------------------------------------
# GetCost function:
# ----------------
# Inputs: 
#   - Vector of parameter values & initial conditions
#   - Vector of bed bug-related costs
#   - Vector of years over which to run the simulation
# Output: 
#   - A data frame tracking the total and component costs, as well as 
#     prevalence for each year of the simulation
#   - This output is used to plot Figure 3

GetCost <- function(p.set, bbcosts, years){

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
  tov0 <- 0  # set turnover counter to 0 at time 0
  vac0 <- 0  # set vacancy counter to 0 at time 0
  y0 <- c(Sr0, Ir0, Sv0, Iv0, Sv20, trt0, tov0, vac0)
  t <- seq(from=0, to=365*max(years)+1, by=1)

  # Set bed bug related costs (treatment, turnover, and vacancy costs)
  ctrt <- bbcosts[1] 
  ctov <- bbcosts[2]
  cvac <- bbcosts[3]
  
  # We model the absence of disclosure by setting the disclosure index d = 0
  p <- list(beta=beta, gamma=gamma, b=b, d=d, D=D, k=k, m=m, n=n, N=N)
  p0 <- list(beta=beta, gamma=gamma, b=b, d=0, D=D, k=k, m=m, n=n, N=N)
  
  # We solve the ODE's for the model in the presence and absence of disclosure 
  # (out and out0, respectively)
  out <- ode(y=y0, times=t, func=SetODEs, parms=p)
  out0 <- ode(y=y0, times=t, func=SetODEs, parms=p0)
  
  # We use a for loop to calculate total and component costs for each year of 
  # the simulation
  cost <- trt <- tov <- vac <- prev <- pvac <- as.numeric()
  
  for(jj in 1:length(years)){
    
    # Get the first and last day of year jj
    first.day <- years[jj]*365 - 364
    last.day <- years[jj]*365 + 1
    
    # Calculate the DIFFERENCE between the disclosure and no disclosure simulations 
    # in the number of treatments (n.trt), turnovers (n.tov), and days vacant (n.vac) 
    # that fell on year jj
    n.trt <- ((out[,7][last.day] - out[,7][first.day]) - 
              (out0[,7][last.day] - out0[,7][first.day]))
    n.tov <- ((out[,8][last.day] - out[,8][first.day]) - 
                (out0[,8][last.day] - out0[,8][first.day]))
    n.vac <- ((out[,9][last.day] - out[,9][first.day]) - 
                (out0[,9][last.day] - out0[,9][first.day]))

    # Total per unit treatment cost = 
    # (# of treatments) x (avg cost of bed bug treatment) / (total # units)
    trt[jj] <- n.trt*ctrt/N
  
    # Total per unit turnover cost = 
    # (# of turnover events) x (avg cost of turnover) / (total # units)
    tov[jj] <- n.tov*ctov/N
    
    # Total per unit vacancy cost = 
    # (# months vacant) x (average monthly rent) / (total # units)
    # Note: # months = # days / 30
    vac[jj] <- (n.vac/30)*cvac/N
    
    # Total cost is equal to the sum of the component costs
    cost[jj] <- trt[jj] + tov[jj] + vac[jj]
    
    # Prevalence at the end of year jj is simply the number of units in the 
    # Ir and Iv classes on the last day of the year divided by N
    prev[jj] <- (out[,3][last.day] + out[,5][last.day])/N
    
    # Proportion vacant at the end of year jj is simply the number of units in the 
    # Sv, Iv, and Sv2 classes on the last day of the year divided by N
    pvac[jj] <- (out[,4][last.day] + out[,5][last.day] + out[,6][last.day])/N

  }
  
  df <- data.frame(Year = years, Total_Cost = cost, Treatment = trt, 
                   Turnover = tov, Vacancy = vac, Prevalence=prev, 
                   Prop_Vacant=pvac)
  return(df)
}

# ----------------------------------------------------------------------------
# GetCost2 function:
# ----------------
#  - Same as GetCost function except it outputs a vector of values for a 
#    single year (instead of a data frame with one row per year)
#  - Additional output: percent vacancy at the end of the simulation
#  - This function is used in the GetCostMatrix function

GetCost2 <- function(p.set, bbcosts, year){
  
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
  tov0 <- 0  # set turnover counter to 0 at time 0
  vac0 <- 0  # set vacancy counter to 0 at time 0
  y0 <- c(Sr0, Ir0, Sv0, Iv0, Sv20, trt0, tov0, vac0)
  t <- seq(from=0, to=365*year, by=1)

  # Set bed bug related costs (treatment, turnover, and vacancy costs)
  ctrt <- bbcosts[1] 
  ctov <- bbcosts[2]
  cvac <- bbcosts[3]
  
  # We model the absence of disclosure by setting the disclosure index d = 0
  p <- list(beta=beta, gamma=gamma, b=b, d=d, D=D, k=k, m=m, n=n, N=N)
  p0 <- list(beta=beta, gamma=gamma, b=b, d=0, D=D, k=k, m=m, n=n, N=N)
  
  # We solve the ODE's for the model in the presence and absence of disclosure 
  # (out and out0, respectively)
  out <- ode(y=y0, times=t, func=SetODEs, parms=p)
  out0 <- ode(y=y0, times=t, func=SetODEs, parms=p0)
  
  # We use a for loop to calculate total and component costs for each year of 
  # the simulation
  cost <- trt <- tov <- vac <- prev <- as.numeric()
  
  first.day <- length(t) - 364
  last.day <- length(t)
  
  # Calculate the difference in the number of treatments, number of vacancies,
  # and total days vacant for disclosure vs. no disclosure
  n.trt <- ((out[,7][last.day] - out[,7][first.day]) - 
              (out0[,7][last.day] - out0[,7][first.day]))
  n.tov <- ((out[,8][last.day] - out[,8][first.day]) - 
              (out0[,8][last.day] - out0[,8][first.day]))
  n.vac <- ((out[,9][last.day] - out[,9][first.day]) - 
              (out0[,9][last.day] - out0[,9][first.day]))
  
  # Calculate component costs and total cost
  trt <- n.trt*ctrt/N
  tov <- n.tov*ctov/N
  vac <- (n.vac/30)*cvac/N
  cost <- trt + tov + vac
  names(trt) <- names(tov) <- names(vac) <- names(cost) <- NULL
  
  # Determine prevalence at the end of the simulation
  prev <- (out[,3][last.day] + out[,5][last.day])/N
  
  # Determine percent vacancy at the end of theh simulation
  pvac <- sum(out[,4][last.day], out[,5][last.day], out[,6][last.day])/N
  
  return(c(total.cost=cost, treatment.cost=trt, turnover.cost=tov, 
           vacancy.cost=vac, prevalence=prev, percent.vacancy=pvac))
}

# ----------------------------------------------------------------------------
# GetCostMatrix function:
# ----------------------
# Inputs: 
#   - Range of baseline prevalence values
#   - Range of disclosure index values
#   - Number of years to run the simulation
# Output: 
#   - A matrix with separate columns for total cost, component costs, 
#     prevalence, and percent vacancy
#   - Each row of the matrix represents a different combination of baseline \
#     prevalence & disclosure index values

GetCostMatrix <- function(bprev.values, d.values, nyears){
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
  
  costs <- apply(parmatrix, 1, function(x) GetCost2(x, bbcosts, nyears))
  costs <- t(costs)
  
  costs2 <- cbind(costs, baseline.prevalence = parmatrix[,9], 
                  disclosure.index = parmatrix[,14])
  
  return(costs2)  
}
