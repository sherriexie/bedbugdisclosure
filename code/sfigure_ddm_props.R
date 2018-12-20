library(deSolve)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

source("code/functions.R")
source("code/functions_extra.R")

# 1. Set baseline prevalence (p), renter selectivity (s), # of disclosed compartments (Dnum), and years of simulation
#p <- 0.05
p <- 0.01
s <- 0.5
nyears <- 20
Dnum <- 100

# 2. Set parameter values manually
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

# 3. Solve for beta that will give the desired baseline prevalence
beta <- GetBeta(preparam, p)

# 4. Get initial conditions "at equilibrium"...
#...for base model
param <- c(preparam, beta, base.prev=p)
init <- GetInit(param)
y0 <- c(init, 0, 0, 0, 0)
names(y0)[5:8] <- c("Sv2","C_treat","C_turnover","C_vacancy")

#...for DDM
y0_DDM <- c(init, rep(0,Dnum), 0, 0,0)
names(y0_DDM)[5:(5+Dnum-1)] <- rep('Sv2',Dnum)
names(y0_DDM)[(5+Dnum):(5+Dnum+2)] <- c("C_treat","C_turnover","C_vacancy")

# 5. Format parameters and time to input into ode function
pp <- list(beta = beta, gamma = preparam[1], k = preparam[2], b = preparam[3],
           m = preparam[4], n = preparam[5], N = preparam[6], D = preparam[7],
           d = s)
pp_DDM <- list(beta = beta, gamma = preparam[1], k = preparam[2], b = preparam[3],
           m = preparam[4], n = preparam[5], N = preparam[6], D = preparam[7],
           d = s, Dnum=Dnum)
t <- seq(from=0, to=365*nyears+1, by=1)

# 6. Run the ode solver function for base and DDM models
out <- ode(y=y0, times = t, func=SetODEs, parms=pp)
outDDM <- ode(y=y0_DDM, times = t, func=SetODEsDDM, parms=pp_DDM)

# 7. Plot the difference in percent infested and percent vacant output by the 
#    two models

# Proportion infested and vacant according to base model
Iprop <- (out[,3] + out[,5]) / pp$N
Vprop <- (out[,4] + out[,5] + out[,6]) / pp$N

# Proportion of infested and vacant according to DDM
Sv2tot_DDM <- rowSums(outDDM[, 6 : (6 + Dnum - 1)])          
Iprop_DDM <- (outDDM[,3] + outDDM[,5]) / pp_DDM$N
Vprop_DDM <- (outDDM[,4] + outDDM[,5] + Sv2tot_DDM) / pp_DDM$N

# Calculate the difference in percentages between the two models
I_diff <- (Iprop_DDM - Iprop)*100
V_diff <- (Vprop_DDM - Vprop)*100

ymin <- min(c(I_diff, V_diff))
ymax <- max(c(I_diff, V_diff))

#pdf("figures_supplement/Routput/sfig_ddm_props_prev5.pdf", width = 3.5, height = 4)
pdf("figures_supplement/Routput/sfig_ddm_props_prev1.pdf", width = 3.5, height = 4)

plot(t/365, I_diff, xlab = "Year", ylab = "Difference in %", type = "l", 
     col = "red", ylim = c(ymin, ymax), lwd = 2)
lines(t/365, V_diff, col = "blue", lty = 2, lwd = 2)
plot.new()
legend("right", legend=c("Infested","Vacant"), bty="n",
       col=c("red","blue"), lty=c(1,2),lwd=c(2, 2), cex=0.8)
dev.off()
