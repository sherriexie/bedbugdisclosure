library(deSolve)
library(reshape2)
library(ggplot2)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("/Users/sxs/Dropbox/Levy Lab/bb_properties/bedbugdisclosure")

# Source the functions we will need for our analyses.
source("code/functions.R")
source("../functions_renters.R")

# Set baseline prevalence (p) and renter selectivity (s).
p <- 0.05
s <- 0.5

# Set parameter values and bed bug-related costs.  
preparam <- SetParameters()
#bbcosts <- SetCost()
renter.costs <- c(ancillary.cost.per.trt = 800, # cost of replacing a low to mid range queen mattress 
                  cost.per.move = 400, # lower range listed for 2 bedroom apt
                                       # https://www.homeadvisor.com/cost/storage-and-organization/hire-a-moving-service/#movers
                  cost.per.month.infested = 200) # national hourly wage for all private employees was about $26 for 2017 (x8 for a full work day)
                                                 # source: https://www.bls.gov/web/empsit/compaehes.txt

# Solve for beta to get the desired baseline prevalence in the absence of
# disclosure. Append beta and p to our vector of parameter values.
beta <- GetBeta(preparam, p)
param <- c(preparam, beta, base.prev = p)

# Get initial conditions (Sr0, Ir0, etc.) and append these and d to our vector 
# of parameter values.
init <- GetInit(param)
param.init <- c(param, init, d = s)

# Set the years over which to run the simulation and get the cost of disclosure.
years <- 1:20

# Run the GetCost function which outputs a data frame of total and component
# costs, along with prevalence over the years of the disclosure simulation.
cost.df <- GetCost_renters(param.init, renter.costs, years)

# Add Year 0
year0 <- data.frame(Year = 0, Total_Cost = 0, Treatment = 0, Moveout = 0, 
                    Untreated_Infestation = 0, Prevalence = p)
cost.df <- rbind(year0, cost.df)

# Separate data frame into total cost, component cost, and prevalence.
# Component costs need to be transformed to long data format.
totalcost.df <- cost.df[,1:2]
prev.df <- cost.df[,c(1,6)]
componentcost.df <- melt(cost.df[,c(1,3,4,5)], id.vars= "Year", value.name = "cost")

# All costs are NEGATIVE (i.e. all savings), so we will transform costs to be 
# expressed as savings by multipling values by -1
totalcost.df$Total_Savings <- totalcost.df$Total_Cost*-1
componentcost.df$savings <- componentcost.df$cost*-1

# Convert prevalence so it's expressed in percentages
prev.df$Prevalence <- prev.df$Prevalence*100
prev.df$Prevalence_Reduction <- 5 - prev.df$Prevalence

# The following chunk of code finds the best place to position the prevalence
# curve:
maxcc <- max(totalcost.df$Total_Savings) 
mincc <- min(totalcost.df$Total_Savings)
maxprev <- max(prev.df$Prevalence_Reduction)
minprev <- min(prev.df$Prevalence_Reduction)
range.cost <- maxcc - mincc 
range.prev <- maxprev - minprev
mid.cost <- (maxcc + mincc)/2
mid.prev <- (maxprev + minprev)/2
m_transform <- range.prev/range.cost
b_transform <- mid.prev - m_transform*mid.cost

# Plot Supplemental Figure xxx
pdf("sfig_barplot_renters.pdf", height=6, width=11)
ggplot() +
  geom_bar(data=componentcost.df, stat = "identity", 
           aes(x=Year, y=savings, fill= variable)) +
  scale_fill_manual(name="Savings component:", values=c("#1f78b4", "#b2df8a", 
                                                     "#a6cee3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_text(vjust=-2), axis.ticks.x=element_blank()) +
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16)) +
  labs(x="Years since implementation of disclosure", y = "Savings ($)") +
  scale_y_continuous(breaks=seq(0, maxcc, by=20),
                     sec.axis = sec_axis(~.*m_transform + b_transform, 
                                         name = "Prevalence reduction (%)",
                                         breaks = 0:3)) +
  theme(axis.line.y.right = element_line(color = "firebrick3"),
        axis.ticks.y.right = element_line(color = "firebrick3"),
        axis.title.y.right = element_text(color = "firebrick3"),
        axis.text.y.right = element_text(color = "firebrick3")) +
  geom_line(data=prev.df,aes(x=Year, y=(Prevalence_Reducation-b_transform)/m_transform, 
                             color = "firebrick1"), color="firebrick1", linetype=1, size=1.3)  
dev.off()




                  