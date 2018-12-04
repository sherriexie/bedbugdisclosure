library(reshape2)
library(ggplot2)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
#setwd("~/Dropbox/Research/SESYNC/Code")
#setwd("UPDATE_PATH/bedbugdisclosure")

# Source the functions we will need for our analyses.
source("code/functions.R")
source("code/functions_DDM.R")

# Set baseline prevalence (p) and renter selectivity (s).
p <- 0.05
s <- 0.5
Dnum <- 100 

# Set parameter values and bed bug-related costs.  
preparam <- SetParameters()
bbcosts <- SetCost()

# Solve for beta to get the desired baseline prevalence in the absence of
# disclosure. Append beta and p to our vector of parameter values.
beta <- GetBeta(preparam, p)
param <- c(preparam, beta, base.prev = p)

# Get initial conditions (Sr0, Ir0, etc.) and append these and d to our vector 
# of parameter values.
init <- GetInit(param)
param.init <- c(param, init, d = s, Dnum = Dnum)

# Set the years over which to run the simulation and get the cost of disclosure.
years <- 1:20

# Run the GetCost function which outputs a data frame of total and component
# costs, along with prevalence over the years of the disclosure simulation.
cost.df <- GetCostDDM(param.init, bbcosts, years)

# Add Year 0
year0 <- data.frame(Year = 0, Total_Cost = 0, Treatment = 0, Turnover = 0, 
                    Vacancy = 0, Prevalence = p, Prop_Vacant = (init[3]+init[4])/param[6])
cost.df <- rbind(year0, cost.df)

# Separate data frame into total cost, component cost, and prevalence.
# Component costs need to be transformed to long data format.
totalcost.df <- cost.df[,1:2]
prev.df <- cost.df[,c(1,6)]
componentcost.df <- melt(cost.df[,c(1,3,4,5)], id.vars= "Year")

# Convert prevalence so it's expressed in percentages
prev.df$Prevalence <- prev.df$Prevalence*100

# The following chunk of code finds the best place to position the prevalence
# curve:
maxcc <- max(componentcost.df$value) 
mincc <- min(componentcost.df$value)
maxprev <- max(prev.df$Prevalence)
minprev <- min(prev.df$Prevalence)
range.cost <- maxcc - mincc 
range.prev <- maxprev - minprev
mid.cost <- (maxcc + mincc)/2
mid.prev <- (maxprev + minprev)/2
m_transform <- range.prev/range.cost
b_transform <- mid.prev - m_transform*mid.cost

# Plot Figure 3
fig <- ggplot() +
  geom_bar(data=componentcost.df, stat = "identity", 
           aes(x=Year, y=value, fill= variable)) +
  scale_fill_manual(name="Cost component:", values=c("#1f78b4", "#b2df8a", 
                                                    "#a6cee3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_text(vjust=-2), axis.ticks.x=element_blank()) +
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16)) +
  labs(x="Years since implementation of disclosure", y = "Cost ($)") +
  geom_line(data=totalcost.df,aes(x=Year, y=Total_Cost, color = "black"), 
            color="black", linetype=2, size=1.3) +
  scale_color_discrete(name="", labels="Total Cost") +
  scale_y_continuous(breaks=seq(-80, 40, by=20),
                     sec.axis = sec_axis(~.*m_transform + b_transform, 
                                         name = "Prevalence (%)",
                                         breaks = 2:5)) +
  theme(axis.line.y.right = element_line(color = "firebrick3"),
        axis.ticks.y.right = element_line(color = "firebrick3"),
        axis.title.y.right = element_text(color = "firebrick3"),
        axis.text.y.right = element_text(color = "firebrick3")) +
  geom_line(data=prev.df,aes(x=Year, y=(Prevalence-b_transform)/m_transform, 
            color = "firebrick1"), color="firebrick1", linetype=1, size=1.3) 
plot(fig)
#ggsave("figures/DDMfigures/fig_barplotDDM.pdf", height=6, width=10)

