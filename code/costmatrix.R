library(deSolve)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

# Source the functions we will need for our analysis
source("code/functions.R")

# Set ranges for baseline prevalence (bprev) and disclosure index (d) values 
# -- these will form the x and y axes for the 3D plots in Figures 3 and 4
#    and the cost and prevalence animations in the shiny app
bprev.vec <- seq(0.001, 0.1, length.out = 100)
d.vec <- seq(0.01, 1, length.out = 100)

# Get cost matrices for years 1, 2, 3, 4, 5, and 20 to be used for Figures 3 
# and 4 and save output
costmat1 <- GetCostMatrix(bprev.vec, d.vec, nyears = 1)
costmat2 <- GetCostMatrix(bprev.vec, d.vec, nyears = 2)
costmat3 <- GetCostMatrix(bprev.vec, d.vec, nyears = 3)
costmat4 <- GetCostMatrix(bprev.vec, d.vec, nyears = 4)
costmat5 <- GetCostMatrix(bprev.vec, d.vec, nyears = 5)
costmat20 <- GetCostMatrix(bprev.vec, d.vec, nyears = 20)

saveRDS(costmat1, "output_costmatrix/costmat_yr1.rds")
saveRDS(costmat2, "output_costmatrix/costmat_yr2.rds")
saveRDS(costmat3, "output_costmatrix/costmat_yr3.rds")
saveRDS(costmat4, "output_costmatrix/costmat_yr4.rds")
saveRDS(costmat5, "output_costmatrix/costmat_yr5.rds")
saveRDS(costmat20, "output_costmatrix/costmat_yr20.rds")

# Additionally, get cost matrices for years 6-19 to be used for cost and 
# prevalence animations in the shiny app 
costmat6 <- GetCostMatrix(bprev.vec, d.vec, nyears = 6)
costmat7 <- GetCostMatrix(bprev.vec, d.vec, nyears = 7)
costmat8 <- GetCostMatrix(bprev.vec, d.vec, nyears = 8)
costmat9 <- GetCostMatrix(bprev.vec, d.vec, nyears = 9)
costmat10 <- GetCostMatrix(bprev.vec, d.vec, nyears = 10)
costmat11 <- GetCostMatrix(bprev.vec, d.vec, nyears = 11)
costmat12 <- GetCostMatrix(bprev.vec, d.vec, nyears = 12)
costmat13 <- GetCostMatrix(bprev.vec, d.vec, nyears = 13)
costmat14 <- GetCostMatrix(bprev.vec, d.vec, nyears = 14)
costmat15 <- GetCostMatrix(bprev.vec, d.vec, nyears = 15)
costmat16 <- GetCostMatrix(bprev.vec, d.vec, nyears = 16)
costmat17 <- GetCostMatrix(bprev.vec, d.vec, nyears = 17)
costmat18 <- GetCostMatrix(bprev.vec, d.vec, nyears = 18)
costmat19 <- GetCostMatrix(bprev.vec, d.vec, nyears = 19)

saveRDS(costmat6,  "output_costmatrix/costmat_yr6.rds")
saveRDS(costmat7,  "output_costmatrix/costmat_yr7.rds")
saveRDS(costmat8,  "output_costmatrix/costmat_yr8.rds")
saveRDS(costmat9,  "output_costmatrix/costmat_yr9.rds")
saveRDS(costmat10, "output_costmatrix/costmat_yr10.rds")
saveRDS(costmat11, "output_costmatrix/costmat_yr11.rds")
saveRDS(costmat12, "output_costmatrix/costmat_yr12.rds")
saveRDS(costmat13, "output_costmatrix/costmat_yr13.rds")
saveRDS(costmat14, "output_costmatrix/costmat_yr14.rds")
saveRDS(costmat15, "output_costmatrix/costmat_yr15.rds")
saveRDS(costmat16, "output_costmatrix/costmat_yr16.rds")
saveRDS(costmat17, "output_costmatrix/costmat_yr17.rds")
saveRDS(costmat18, "output_costmatrix/costmat_yr18.rds")
saveRDS(costmat19, "output_costmatrix/costmat_yr19.rds")
