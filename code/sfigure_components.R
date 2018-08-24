library(deSolve)
library(reshape2)
library(ggplot2)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

# Source the functions we will need for our analysis
source("code/functions.R")
source("code/functions_extra.R")

# Note below that:
#  - bprev = baseline prevalence (p)
#  - d     = renter selectivity (s)

b1d1 <- PlotCostComponents(bprev=0.01, d=0.1, "b1d1")
b1d3 <- PlotCostComponents(bprev=0.01, d=0.3, "b1d3")
b1d5 <- PlotCostComponents(bprev=0.01, d=0.5, "b1d5")
b1d7 <- PlotCostComponents(bprev=0.01, d=0.7, "b1d7")
b1d9 <- PlotCostComponents(bprev=0.01, d=0.9, "b1d9")

b3d1 <- PlotCostComponents(bprev=0.03, d=0.1, "b3d1")
b3d3 <- PlotCostComponents(bprev=0.03, d=0.3, "b3d3")
b3d5 <- PlotCostComponents(bprev=0.03, d=0.5, "b3d5")
b3d7 <- PlotCostComponents(bprev=0.03, d=0.7, "b3d7")
b3d9 <- PlotCostComponents(bprev=0.03, d=0.9, "b3d9")

b5d1 <- PlotCostComponents(bprev=0.05, d=0.1, "b5d1")
b5d3 <- PlotCostComponents(bprev=0.05, d=0.3, "b5d3")
b5d5 <- PlotCostComponents(bprev=0.05, d=0.5, "b5d5")
b5d7 <- PlotCostComponents(bprev=0.05, d=0.7, "b5d7")
b5d9 <- PlotCostComponents(bprev=0.05, d=0.9, "b5d9")

b7d1 <- PlotCostComponents(bprev=0.07, d=0.1, "b7d1")
b7d3 <- PlotCostComponents(bprev=0.07, d=0.3, "b7d3")
b7d5 <- PlotCostComponents(bprev=0.07, d=0.5, "b7d5")
b7d7 <- PlotCostComponents(bprev=0.07, d=0.7, "b7d7")
b7d9 <- PlotCostComponents(bprev=0.07, d=0.9, "b7d9")

b9d1 <- PlotCostComponents(bprev=0.09, d=0.1, "b9d1")
b9d3 <- PlotCostComponents(bprev=0.09, d=0.3, "b9d3")
b9d5 <- PlotCostComponents(bprev=0.09, d=0.5, "b9d5")
b9d7 <- PlotCostComponents(bprev=0.09, d=0.7, "b9d7")
b9d9 <- PlotCostComponents(bprev=0.09, d=0.9, "b9d9")

pdf("figures_supplement/Routput/costcomponents.pdf",height=3, width=5.5)
b1d1
b1d3
b1d5
b1d7
b1d9

b3d1
b3d3
b3d5
b3d7
b3d9

b5d1
b5d3
b5d5
b5d7
b5d9

b7d1
b7d3
b7d5
b7d7
b7d9

b9d1
b9d3
b9d5
b9d7
b9d9
dev.off()

