library(deSolve)
library(ggplot2)
library(reshape2)
library(ggplot2)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

# Source the functions we will need for our analysis.
source("code/functions.R")

# Set renter selectivity (s)
s <- 0.5

# Vary baseline prevalence 
p <- 0.01
#p <- 0.02
#p <- 0.03
#p <- 0.04
#p <- 0.05
#p <- 0.06
#p <- 0.07
#p <- 0.08
#p <- 0.09
#p <- 0.10
#p <- 0.11
#p <- 0.12
#p <- 0.13
#p <- 0.14
#p <- 0.15
#p <- 0.16
#p <- 0.163
#p <- 0.17


# Set parameter values and bed bug-related costs.  
preparam <- SetParameters()
bbcosts <- SetCost()

# Solve for beta to get the desired baseline prevalence in the absence of
# disclosure. Append these and bprev to our vector of parameter values.
beta <- GetBeta(preparam, p)
param <- c(preparam, beta, base.prev = p)

# Get initial conditions (Sr0, Ir0, etc.) and append these and d to our vector 
# of parameter values.
init <- GetInit(param)
param.init <- c(param, init, d = s)

# Set the years over which to run the simulation and get the cost of disclosure.
years <- c(1:200)

# Run the GetCost function which outputs a data frame of total and component
# costs, along with prevalence over the years of the disclosure simulation.
cost.df <- GetCost(param.init, bbcosts, years)


# The values were filled in manually by rerunning the code above for different
# values of p
# Note: for p ≥ 17, costs remain positive indefinitely
View(cost.df)  # Check the year that Total_Cost goes from positive to negative
p.vec <- c(1,2:8, 9:12, 13, 14, 15, 16, 16.3)
breakeven.year <- c(4, rep(5,7), rep(6,4), 7, 8, 9, 13, 17)

df <- data.frame(p=p.vec, yrs=breakeven.year)

pdf("figures_supplement/sfig_breakevenpoint.pdf", height=5, width=6)

ggplot() +
  geom_line(data=df, aes(x=p, y=yrs)) +
  geom_point(data=df, aes(x=p, y=yrs), size=3) +
  theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16)) +
  ylim(c(0,17)) +
  labs(x="Baseline prevalence (%)", y = "Break-even point (years)")

dev.off()
