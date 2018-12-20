library(deSolve)
library(reshape2)
library(ggplot2)
library(dplyr)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

# Source the functions we will need for our analyses.
source("code/functions.R")

# Set baseline prevalence (p) and renter selectivity (s).
p <- 0.05
s <- 0.5

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
param.init <- c(param, init, d = s)

# Set the years over which to run the simulation and get the cost of disclosure.
years <- 1:20

# Run the GetCost function which outputs a data frame of total and component
# costs, along with prevalence over the years of the disclosure simulation.
cost.df <- GetCost(param.init, bbcosts, years)

dcost <- cost.df %>%
  select(Year, Total_Cost) %>%
  mutate(Total_Cost_2 = Total_Cost*(1/1.02^Year),  # 5% discount rate
         Total_Cost_4 = Total_Cost*(1/1.04^Year),  # 10% discount rate
         Total_Cost_6 = Total_Cost*(1/1.06^Year), # 15% discount rate
         Total_Cost_8 = Total_Cost*(1/1.08^Year), # 15% discount rate
         Total_Cost_10 = Total_Cost*(1/1.1^Year), # 15% discount rate
         Cum_Cost = cumsum(Total_Cost),        # Find cumulative costs
         Cum_Cost_2 = cumsum(Total_Cost_2),
         Cum_Cost_4 = cumsum(Total_Cost_4),
         Cum_Cost_6 = cumsum(Total_Cost_6),
         Cum_Cost_8 = cumsum(Total_Cost_8),
         Cum_Cost_10 = cumsum(Total_Cost_10))  

# Add Year 0
year0 <- data.frame(Year = 0, Total_Cost = 0, Total_Cost_2 = 0, 
                    Total_Cost_4 = 0, Total_Cost_6 = 0, Total_Cost_8 = 0,
                    Total_Cost_10 = 0, Cum_Cost = 0, Cum_Cost_2 = 0, 
                    Cum_Cost_4 = 0, Cum_Cost_6 = 0, Cum_Cost_8 = 0,
                    Cum_Cost_10 = 0)
dcost <- rbind(year0, dcost)

# Separate data frame into yearly costs and cumulative costs
yearly.nodiscount <- select(dcost, Year, Total_Cost)
cum.nodiscount <- select(dcost, Year, Cum_Cost)
yearly.cost.wide <- select(dcost, Year, Total_Cost_2, Total_Cost_4, 
                      Total_Cost_6, Total_Cost_8, Total_Cost_10)
cum.cost.wide <- select(dcost, Year, Cum_Cost_2, Cum_Cost_4, Cum_Cost_6,
                        Cum_Cost_8, Cum_Cost_10)

# Transform into long data format.
yearly.cost <- melt(yearly.cost.wide, id.vars = "Year")
cum.cost <- melt(cum.cost.wide, id.vars = "Year")


# Make plots
pdf("../cost_discounted.pdf", height=4, width=5)

# Plot yearly costs
ggplot() +
  geom_line(data=yearly.cost, aes(x=Year, y=value, color=variable)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  theme_bw() +
  geom_line(data=yearly.nodiscount, aes(x=Year, y=Total_Cost), linetype=2) +
  scale_color_discrete(name="Discount rate") +
  labs(x="Years since implementation of disclosure", y = "Cost ($)")

# Plot cumulative costs
ggplot() +
  geom_line(data=cum.cost, aes(x=Year, y=value, color=variable)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  theme_bw() + 
  geom_line(data=cum.nodiscount, aes(x=Year, y=Cum_Cost), linetype=2) +
  scale_color_discrete(name="Discount rate") +
  labs(x="Years since implementation of disclosure", y = "Cumulative cost ($)")
dev.off()
