library(deSolve)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("/Users/sxs/Dropbox/Levy Lab/bb_properties/bedbugdisclosure")

# Source the functions we will need for our analyses.
source("code/functions.R")
source("../functions_renters.R")


# Set ranges for baseline prevalence (bprev) and disclosure index (d) values 
# -- these will form the x and y axes for the 3D plots 
bprev.vec <- seq(0.001, 0.1, length.out = 20)
d.vec <- seq(0.01, 1, length.out = 20)

# Get cost matrices for years 1, 2, 3, 4, 5, and 20 to be used for Supp. Figure xx
costmat1 <- GetCostMatrix_renters(bprev.vec, d.vec, nyears = 1)
costmat2 <- GetCostMatrix_renters(bprev.vec, d.vec, nyears = 2)
costmat3 <- GetCostMatrix_renters(bprev.vec, d.vec, nyears = 3)
costmat4 <- GetCostMatrix_renters(bprev.vec, d.vec, nyears = 4)
costmat5 <- GetCostMatrix_renters(bprev.vec, d.vec, nyears = 5)
costmat20 <- GetCostMatrix_renters(bprev.vec, d.vec, nyears = 20)

# COMMENT THIS OUT LATER
setwd("..")
saveRDS(costmat1, "output_costmatrix_renters/costmat_yr1.rds")
saveRDS(costmat2, "output_costmatrix_renters/costmat_yr2.rds")
saveRDS(costmat3, "output_costmatrix_renters/costmat_yr3.rds")
saveRDS(costmat4, "output_costmatrix_renters/costmat_yr4.rds")
saveRDS(costmat5, "output_costmatrix_renters/costmat_yr5.rds")
saveRDS(costmat20, "output_costmatrix_renters/costmat_yr20.rds")

