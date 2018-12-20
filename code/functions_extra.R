# ----------------------------------------------------------------------------
# GetCostVaryinggamma function:
#-----------------------------
# gamma.range is a vector of values
GetCostVaryinggamma <- function(gamma.range, bprev=0.05, d=0.5, years=1:20){
  costs <- data.frame(Year=NULL, Total_Cost=NULL, Treatment=NULL, 
                      Turnover=NULL, Vacancy=NULL, Prevalence=NULL, gamma=NULL)
  for(gamma in gamma.range){
    preparam <- SetParameters()
    preparam[1] <- gamma
    beta <- GetBeta(preparam, bprev)
    param <- c(preparam, beta, bprev)
    init <- GetInit(param)
    param.init <- c(param, init, d = d)
    bbcosts <- SetCost()
    cost.df <- GetCost(param.init, bbcosts, years)
    cost.df <- rbind(c(0, 0, 0, 0, 0, bprev), cost.df)
    cost.df$gamma <- gamma
    costs <- rbind(costs, cost.df)
  }
  return(costs)
}

# ----------------------------------------------------------------------------
# GetCostVaryingk function:
#-------------------------
# k.range is a vector of k values
GetCostVaryingk <- function(k.range, bprev=0.05, d=0.5, years=1:20){
  costs <- data.frame(Year=NULL, Total_Cost=NULL, Treatment=NULL, 
                      Turnover=NULL, Vacancy=NULL, Prevalence=NULL, k=NULL)
  for(k in k.range){
    preparam <- SetParameters()
    preparam[2] <- k
    beta <- GetBeta(preparam, bprev)
    param <- c(preparam, beta, bprev)
    init <- GetInit(param)
    param.init <- c(param, init, d = d)
    bbcosts <- SetCost()
    cost.df <- GetCost(param.init, bbcosts, years)
    cost.df <- rbind(c(0, 0, 0, 0, 0, bprev), cost.df)
    cost.df$k <- k
    costs <- rbind(costs, cost.df)
  }
  return(costs)
}

# ----------------------------------------------------------------------------
# GetCostVaryingb function:
#-------------------------
# b.range is a vector of b values

GetCostVaryingb <- function(b.range, bprev=0.05, d=0.5, years=1:20){
  costs <- data.frame(Year=NULL, Total_Cost=NULL, Treatment=NULL, 
                      Turnover=NULL, Vacancy=NULL, Prevalence=NULL, b=NULL)
  for(b in b.range){
    preparam <- SetParameters()
    preparam[3] <- b
    beta <- GetBeta(preparam, bprev)
    param <- c(preparam, beta, bprev)
    init <- GetInit(param)
    param.init <- c(param, init, d = d)
    bbcosts <- SetCost()
    cost.df <- GetCost(param.init, bbcosts, years)
    cost.df <- rbind(c(0, 0, 0, 0, 0, bprev), cost.df)
    cost.df$b <- b
    costs <- rbind(costs, cost.df)
  }
  return(costs)
}


# ----------------------------------------------------------------------------
# GetCostVaryingkhighb function:
#------------------------------
# k.range is a vector of k values

GetCostVaryingkhighb <- function(k.range, b.set, bprev=0.05, d=0.5, years=1:20){
  costs <- data.frame(Year=NULL, Total_Cost=NULL, Treatment=NULL, 
                      Turnover=NULL, Vacancy=NULL, Prevalence=NULL, k=NULL)
  for(k in k.range){
    preparam <- SetParameters()
    preparam[2] <- k
    preparam[3] <- b.set
    beta <- GetBeta(preparam, bprev)
    param <- c(preparam, beta, bprev)
    init <- GetInit(param)
    param.init <- c(param, init, d = d)
    bbcosts <- SetCost()
    cost.df <- GetCost(param.init, bbcosts, years)
    cost.df <- rbind(c(0, 0, 0, 0, 0, bprev), cost.df)
    cost.df$k <- k
    costs <- rbind(costs, cost.df)
  }
  return(costs)
}

# ----------------------------------------------------------------------------
# GetCostVaryingD function:
#-------------------------
# D.range is a vector of D values

GetCostVaryingD <- function(D.range, bprev=0.05, d=0.5, years=1:20){
  costs <- data.frame(Year=NULL, Total_Cost=NULL, Treatment=NULL, 
                      Turnover=NULL, Vacancy=NULL, Prevalence=NULL, 
                      Prop_Vacant=NULL, D=NULL)
  for(D in D.range){
    preparam <- SetParameters()
    preparam[7] <- D
    beta <- GetBeta(preparam, bprev)
    param <- c(preparam, beta, bprev)
    init <- GetInit(param)
    param.init <- c(param, init, d = d)
    bbcosts <- SetCost()
    cost.df <- GetCost(param.init, bbcosts, years)
    cost.df <- rbind(c(0, 0, 0, 0, 0, bprev, 0.073), cost.df)
    cost.df$D <- D
    costs <- rbind(costs, cost.df)
  }
  return(costs)
}

# ----------------------------------------------------------------------------
# GetCostVaryingm function:
#-------------------------
# m.range is a vector of m values

GetCostVaryingm <- function(m.range, bprev=0.05, d=0.5, years=1:20){
  costs <- data.frame(Year=NULL, Total_Cost=NULL, Treatment=NULL, 
                      Turnover=NULL, Vacancy=NULL, Prevalence=NULL, b=NULL)
  for(m in m.range){
    preparam <- SetParameters()
    preparam[4] <- m
    beta <- GetBeta(preparam, bprev)
    param <- c(preparam, beta, bprev)
    init <- GetInit(param)
    param.init <- c(param, init, d = d)
    bbcosts <- SetCost()
    cost.df <- GetCost(param.init, bbcosts, years)
    cost.df <- rbind(c(0, 0, 0, 0, 0, bprev), cost.df)
    cost.df$D <- D
    costs <- rbind(costs, cost.df)
  }
  return(costs)
}

# ----------------------------------------------------------------------------
# GetCostVaryingD function:
#-------------------------
# n.range is a vector of n values

GetCostVaryingn <- function(n.range, bprev=0.05, d=0.5, years=1:20){
  costs <- data.frame(Year=NULL, Total_Cost=NULL, Treatment=NULL, 
                      Turnover=NULL, Vacancy=NULL, Prevalence=NULL, b=NULL)
  for(n in n.range){
    preparam <- SetParameters()
    preparam[5] <- n
    beta <- GetBeta(preparam, bprev)
    param <- c(preparam, beta, bprev)
    init <- GetInit(param)
    param.init <- c(param, init, d = d)
    bbcosts <- SetCost()
    cost.df <- GetCost(param.init, bbcosts, years)
    cost.df <- rbind(c(0, 0, 0, 0, 0, bprev), cost.df)
    cost.df$n <- n
    costs <- rbind(costs, cost.df)
  }
  return(costs)
}

# ----------------------------------------------------------------------------
# PlotCostComponents function:
#----------------------------

PlotCostComponents <- function(bprev, d, title){
  
  bprev <- bprev
  d <- d
  
  # Set parameter values and bed bug-related costs.  
  preparam <- SetParameters()
  bbcosts <- SetCost()
  
  # Solve for beta to get the desired baseline prevalence in the absence of
  # disclosure. Append these and bprev to our vector of parameter values.
  beta <- GetBeta(preparam, bprev)
  param <- c(preparam, beta, base.prev = bprev)
  
  # Get initial conditions (Sr0, Ir0, etc.) and append these and d to our vector 
  # of parameter values.
  init <- GetInit(param)
  param.init <- c(param, init, d = d)
  
  # Set the years over which to run the simulation and get the cost of disclosure.
  years <- 1:20
  
  # Run the GetCost function which outputs a data frame of total and component
  # costs, along with prevalence over the years of the disclosure simulation.
  cost.df <- GetCost(param.init, bbcosts, years)
  
  # Add Year 0
  year0 <- data.frame(Year = 0, Total_Cost = 0, Treatment = 0, Turnover = 0, 
                      Vacancy = 0, Prevalence = bprev, Prop_Vacant = 0.073)
  cost.df <- rbind(year0, cost.df)
  
  # Separate data frame into total cost, component cost, and prevalence.
  # Component costs need to be transformed to long data format.
  totalcost.df <- cost.df[,1:2]
  prev.df <- cost.df[,c(1,6)]
  componentcost.df <- melt(cost.df[,c(1,3,4,5)], id.vars= "Year")
  
  # Convert prevalence so it's expressed in percentages
  prev.df$Prevalence <- prev.df$Prevalence*100
  
  r.cost <- 310-(-220)
  r.prev <- 9 - 0
  m.cost <- (310 -220)/2
  m.prev <- (9 + 0)/2
  m_transform <- r.prev/r.cost
  b_transform <- m.prev - m_transform*m.cost
  
  # These values sets the transformations for the secondary y axis:
  m_transform <- 0.01698113
  b_transform <- 3.735849
  
  # Plot Figure 3
  plot <- ggplot() +
    geom_bar(data=componentcost.df, stat = "identity", 
             aes(x=Year, y=value, fill= variable)) +
    scale_fill_manual(name="Cost Component", values=c("#1f78b4", "#b2df8a", 
                                                      "#a6cee3")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x=element_text(vjust=-2), axis.ticks.x=element_blank()) +
    theme(plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16,face="bold"),
          legend.text = element_text(size=16),
          legend.title = element_text(size=16)) +
    labs(x="Years Since Implementation of Disclosure", y = "Cost ($)") +
    geom_line(data=totalcost.df,aes(x=Year, y=Total_Cost, color = "black"), 
              color="black", linetype=2, size=1.3) +
    scale_color_discrete(name="", labels="Total Cost") +
    coord_cartesian(ylim=c(-220, 300)) +
    scale_y_continuous(sec.axis = sec_axis(~.*m_transform + b_transform, 
                                           name = "Prevalence (%)",
                                           breaks=c(0,3,6,9))) +
    theme(axis.line.y.right = element_line(color = "firebrick3"),
          axis.ticks.y.right = element_line(color = "firebrick3"),
          axis.title.y.right = element_text(color = "firebrick3"),
          axis.text.y.right = element_text(color = "firebrick3")) +
    geom_line(data=prev.df,aes(x=Year, y=(Prevalence-b_transform)/m_transform, 
                               color = "firebrick1"), color="firebrick1", 
              linetype=1, size=1.3) +
    ggtitle(title)
  
  return(plot)
}

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
# GetCost2_renters function:
# -------------------------
#  - Same as GetCost2 function adapted to calculate cost to renters
#  - This function is used in the GetCostMatrix_renters function

GetCost2_renters <- function(p.set, years){
  
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
  ctrt <- 800 
  cmov <- 400
  cinf <- 200
  
  # We model the absence of disclosure by setting the disclosure index d = 0
  p <- list(beta=beta, gamma=gamma, b=b, d=d, D=D, k=k, m=m, n=n, N=N)
  p0 <- list(beta=beta, gamma=gamma, b=b, d=0, D=D, k=k, m=m, n=n, N=N)
  
  # We solve the ODE's for the model in the presence and absence of disclosure 
  # (out and out0, respectively)
  out <- ode(y=y0, times=t, func=SetODEs_renters, parms=p)
  out0 <- ode(y=y0, times=t, func=SetODEs_renters, parms=p0)
  
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
  
  costs <- apply(parmatrix, 1, function(x) GetCost2_renters(x, nyears))
  costs <- t(costs)
  
  costs2 <- cbind(costs, baseline.prevalence = parmatrix[,9], 
                  disclosure.index = parmatrix[,14])
  
  return(costs2)  
}

# ----------------------------------------------------------------------------
# SetODEsDDM function:
# -------------------
# This is the same as the SetODEs function with the addition of multiple compartments
# of disclosed units, which allows for a non-exponential distribution of times spent
# in the disclosed comparment. This code involves the additional parameter
#
#   Dnum = number of disclosed compartments Sv2. Mean time of being in disclosed state 
#         (if no leaving due to renting) is 1/D with standard deviation 1/sqrt(Dnum)
#
# and Sv2 and DSv2.dt are now vectors with Dnum entries

SetODEsDDM<-function(t,y,p){
  # Dnum is number of Sv2 compartments
  Sr <- y[1]
  Ir <- y[2]
  Sv <- y[3]
  Iv <- y[4]
  Sv2 <- y[5:(5+Dnum-1)] #vector
  trt <- y[5+Dnum]
  tov <- y[5+Dnum+1]
  
  # Note f(t) = b*Ir/(Sr+b*Ir)
  with(as.list(p),{
    dSr.dt <- -beta*Sr*Ir/N + gamma*Ir + n*(1-d)*(1-k*b*Ir/(Sr+b*Ir))*sum(Sv2) + n*(1-k*b*Ir/(Sr+b*Ir))*Sv - m*Sr
    dIr.dt <- beta*Sr*Ir/N + n*k*b*Ir/(Sr+b*Ir)*Sv + n*(1-d)*k*b*Ir/(Sr+b*Ir)*sum(Sv2) + n*(1-d)*Iv - gamma*Ir - b*m*Ir 
    dSv.dt <- m*Sr + Dnum/D*Sv2[Dnum] - n*Sv 
    dIv.dt <- b*m*Ir - gamma*Iv - n*(1-d)*Iv
    
    dSv2.dt <- numeric(Dnum)
    dSv2.dt[1] <- gamma*Iv - n*(1-d)*Sv2[1] - Dnum/D*Sv2[1]
    
    if(Dnum>1){
      for(j in 2:Dnum){
        dSv2.dt[j] <- Dnum/D*Sv2[j-1] - n*(1-d)*Sv2[j] - Dnum/D*Sv2[j]
      }
    }
    
    dtrt.dt <- gamma*Ir + gamma*Iv
    dtov.dt <- n*Sv + n*(1-d)*sum(Sv2) + n*(1-d)*Iv
    dvac.dt <- Sv + sum(Sv2) +Iv
    
    return(list(c(dSr.dt, dIr.dt, dSv.dt, dIv.dt, dSv2.dt, dtrt.dt, dtov.dt, dvac.dt)))
  })
}

# ----------------------------------------------------------------------------
# GetCostDDM function:
# -------------------
# Inputs: 
#   - Vector of parameter values & initial conditions
#   - Vector of bed bug-related costs
#   - Vector of years over which to run the simulation
# Output: 
#   - A data frame tracking the total and component costs, as well as 
#     prevalence for each year of the simulation
#   - This output is similar to that of Figure 3 but for the model for fixed disclosure period

GetCostDDM <- function(p.set, bbcosts, years){
  
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
  Dnum <- p.set[15]
  
  # Set initial conditions and time interval
  Sr0 <- p.set[10]
  Ir0 <- p.set[11]
  Sv0 <- p.set[12]
  Iv0 <- p.set[13]
  Sv20 <- 0  # Sv20 is set to 0 because we assume disclosure begins at time 0
  trt0 <- 0  # set treatment counter to 0 at time 0
  tov0 <- 0  # set turnover counter to 0 at time 0
  vac0 <- 0  # set vacancy counter to 0 at time 0
  y0 <- c(Sr0, Ir0, Sv0, Iv0, Sv20*rep(0,Dnum), trt0, tov0, vac0)
  y00 <- c(Sr0, Ir0, Sv0, Iv0, Sv20, trt0, tov0, vac0)
  t <- seq(from=0, to=365*max(years)+1, by=1)
  
  # Set bed bug related costs (treatment, turnover, and vacancy costs)
  ctrt <- bbcosts[1] 
  ctov <- bbcosts[2]
  cvac <- bbcosts[3]
  
  # We model the absence of disclosure by setting the disclosure index d = 0
  p <- list(beta=beta, gamma=gamma, b=b, d=d, D=D, k=k, m=m, n=n, N=N, Dnum=Dnum)
  p0 <- list(beta=beta, gamma=gamma, b=b, d=0, D=D, k=k, m=m, n=n, N=N)
  
  # We solve the ODE's for the model in the presence and absence of disclosure 
  # (out and out0, respectively)
  out <- ode(y=y0, times=t, func=SetODEsDDM, parms=p)
  out0 <- ode(y=y00, times=t, func=SetODEs, parms=p0)
  
  # get total number in disclosed compartment
  if(Dnum>1){
    Sv2tot <- rowSums(out[,6:(6+Dnum-1)])
  } else {
    Sv2tot <- out[,6]
  }
  
  colf <- dim(out)[2] #final variable
  
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
    n.trt <- ((out[,colf-2][last.day] - out[,colf-2][first.day]) - 
                (out0[,7][last.day] - out0[,7][first.day]))
    n.tov <- ((out[,colf-1][last.day] - out[,colf-1][first.day]) - 
                (out0[,8][last.day] - out0[,8][first.day]))
    n.vac <- ((out[,colf][last.day] - out[,colf][first.day]) - 
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
    
    pvac[jj] <- (out[,4][last.day] + out[,5][last.day] + Sv2tot[last.day])/N
    
  }
  
  df <- data.frame(Year = years, Total_Cost = cost, Treatment = trt, 
                   Turnover = tov, Vacancy = vac, Prevalence=prev, 
                   Prop_Vacant=pvac)
  return(df)
}

