library(deSolve)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
#setwd("~/Dropbox/Research/SESYNC/Code")
#setwd("UPDATE_PATH/bedbugdisclosure")

# Source the functions we will need for our analysis
source("code/functions.R")
source("code/functions_DDM.R")

# Set baseline prevalence (p), renter selectivity (s), and years of simulation
p <- 0.05
s <- 0.5
nyears <- 10
Dnum <- 100

# Set parameter values manually
SetParametersManual <- function(){
  gamma <- 1/(6*30)
  k <- 0.3
  b <- 1.3
  m <- 1/(2*365)
  n <- 1/(3*30)
  N <- 1000
  D <- 365
  return(c(gamma=gamma, k=k, b=b, m=m, n=n, N=N, D=D))
}
preparam <- SetParametersManual()

# Solve for beta that will give the desired baseline prevalence
beta <- GetBeta(preparam, p)

# Either solve for initial conditions "at equilibrium"...
param <- c(preparam, beta, base.prev=p)
init <- GetInit(param)
y0 <- c(init, 0, 0, 0, 0)
names(y0)[5:8] <- c("Sv2","C_treat","C_turnover","C_vacancy")

# Format parameters and time to input into ode function
pp <- list(beta = beta, gamma = preparam[1], k = preparam[2], b = preparam[3],
           m = preparam[4], n = preparam[5], N = preparam[6], D = preparam[7],
           d = s)
t <- seq(from=0, to=365*nyears+1, by=1)

# Run the ode solver function
out <- ode(y=y0, times = t, func=SetODEs, parms=pp)

# Re-run with Dnum>1
y0 <- c(init, rep(0,Dnum), 0, 0, 0)
names(y0)[5:(5+Dnum-1)] <- rep('Sv2',Dnum)
names(y0)[(5+Dnum):(5+Dnum+2)] <- c("C_treat","C_turnover","C_vacancy")

pp <- list(beta = beta, gamma = preparam[1], k = preparam[2], b = preparam[3],
           m = preparam[4], n = preparam[5], N = preparam[6], D = preparam[7],
           d = s, Dnum=Dnum)

outDDM <- ode(y=y0, times = t, func=SetODEsDDM, parms=pp)

if(Dnum>1){
  Sv2tot <- rowSums(outDDM[,6:(6+Dnum-1)])
} else {
  Sv2tot <- outDDM[,6]
}

# compare relative difference between Dnum=1 and Dnum >1

prevI.0 <- 100*(out[,3]+out[,5])/pp$N
prevI.DDM <- 100*(outDDM[,3]+outDDM[,5])/pp$N
prevV.0 <- 100*(out[,4]+out[,5]+out[,6])/pp$N
prevV.DDM <- 100*(outDDM[,4]+outDDM[,5]+Sv2tot)/pp$N

diff.prev <- prevI.DDM-prevI.0 #in total prevalence
diff.vac <- prevV.DDM-prevV.0 #in % vacant

bbcosts <- SetCost()
ctrt <- bbcosts[1] 
ctov <- bbcosts[2]
cvac <- bbcosts[3]

colf <- dim(out)[2]
colf.DDM <- dim(outDDM)[2]

diff.Ctrt <- ctrt*(outDDM[,colf.DDM-2]-out[,colf-2])/pp$N #in cost of treatment per unit
diff.Ctov <- ctov*(outDDM[,colf.DDM-1]-out[,colf-1])/pp$N #in cost of turnover per unit
diff.Cvac <- cvac*(outDDM[,colf.DDM]-out[,colf])/(pp$N*30) #in cost of vacancy per unit
diff.Call <- diff.Ctrt+diff.Ctov+diff.Cvac  #total cost

#plot cost difference
plot(t/365, diff.Ctrt, type="l", col="#1f78b4", lty=1, lwd=1, ylim=c(-5,40),
     ylab="Difference in cumulative cost ($)", xlab="Year") 
lines(t/365, diff.Ctov, col="#b2df8a", lty=1, lwd=1) 
lines(t/365, diff.Cvac, col="#a6cee3", lty=1, lwd=1) 
lines(t/365, diff.Call, col="black", lty=1, lwd=3) 
legend("right", legend=c("Treatment","Turnover","Vacancy","Total"), bty="n",
       col=c("#1f78b4", "#b2df8a", "#a6cee3","black"),lwd=c(1,1,1,3), cex=0.8)

#plot prevalence and vacancy difference

plot(t/365, diff.prev, type="l", col="red", lty=1, lwd=3, ylim=c(-0.02,0.08), 
     ylab="Difference in %", xlab="Year") 
lines(t/365, diff.vac, col="blue", lty=3, lwd=3) 
legend("topright", legend=c("Infested","Vacant"), bty="n",
       col=c("red","blue"), lty=c(1,3),lwd=c(3,3), cex=0.8)

