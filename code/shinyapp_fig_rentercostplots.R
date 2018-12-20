library(RColorBrewer)
library(oce)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

# Set the x- and y- axis breaks, i.e. baseline prevalence and disclosure index,
# respectively
bprev.vec <- seq(0.001, 0.1, length.out = 20)
d.vec <- seq(0.01, 1, length.out = 20)

# Set min and max cost
mincost <- 0
maxcost <- 409

# Get colors
myPalette <- colorRampPalette(brewer.pal(9, "Blues"))
colors <- myPalette(101)

# Load data ----
outmat1  <- readRDS("output_costmatrix_renters/costmat_yr1.rds")
outmat2  <- readRDS("output_costmatrix_renters/costmat_yr2.rds")
outmat3  <- readRDS("output_costmatrix_renters/costmat_yr3.rds")
outmat4  <- readRDS("output_costmatrix_renters/costmat_yr4.rds")
outmat5  <- readRDS("output_costmatrix_renters/costmat_yr5.rds")
outmat6  <- readRDS("output_costmatrix_renters/costmat_yr6.rds")
outmat7  <- readRDS("output_costmatrix_renters/costmat_yr7.rds")
outmat8  <- readRDS("output_costmatrix_renters/costmat_yr8.rds")
outmat9  <- readRDS("output_costmatrix_renters/costmat_yr9.rds")
outmat10 <- readRDS("output_costmatrix_renters/costmat_yr10.rds")
outmat11 <- readRDS("output_costmatrix_renters/costmat_yr11.rds")
outmat12 <- readRDS("output_costmatrix_renters/costmat_yr12.rds")
outmat13 <- readRDS("output_costmatrix_renters/costmat_yr13.rds")
outmat14 <- readRDS("output_costmatrix_renters/costmat_yr14.rds")
outmat15 <- readRDS("output_costmatrix_renters/costmat_yr15.rds")
outmat16 <- readRDS("output_costmatrix_renters/costmat_yr16.rds")
outmat17 <- readRDS("output_costmatrix_renters/costmat_yr17.rds")
outmat18 <- readRDS("output_costmatrix_renters/costmat_yr18.rds")
outmat19 <- readRDS("output_costmatrix_renters/costmat_yr19.rds")
outmat20 <- readRDS("output_costmatrix_renters/costmat_yr20.rds")

MapColors <- function(mat){
  z <- mat  
  z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
  colmap <- colormap(z = as.vector(z.facet.center), 
                     breaks = seq(mincost, maxcost, length.out=101), col = colors)
  colmat <- matrix(colmap$zcol, nrow = 19, ncol = 19)
  return(colmat)
}

SavePNG <- function(df, year){
  c <- df[,1]*-1  # multiply by -1 to transform cost to cost savings
  cost <- matrix(c, nrow=length(bprev.vec), ncol=length(d.vec))
  colmat <- MapColors(cost)
  
  png(paste0("shinyApp/images/rentercostplot", year, ".png"), width = 7, height = 7, units="in", 
      res = 200)
  persp(bprev.vec, d.vec, cost, phi=0, theta=-35,
        xlab="\n Baseline prevalence (%)", ylab="\n Renter selectivity", 
        zlab = "\n Savings ($)", ticktype="detailed", border=NA, col=colmat,
        main = paste("Savings for Renters, Year", year), cex.main=1.8, 
        zlim=c(mincost, maxcost))
  dev.off()
}

SavePNG(outmat1, 1)
SavePNG(outmat2, 2)
SavePNG(outmat3, 3)
SavePNG(outmat4, 4)
SavePNG(outmat5, 5)
SavePNG(outmat6, 6)
SavePNG(outmat7, 7)
SavePNG(outmat8, 8)
SavePNG(outmat9, 9)
SavePNG(outmat10, 10)
SavePNG(outmat11, 11)
SavePNG(outmat12, 12)
SavePNG(outmat13, 13)
SavePNG(outmat14, 14)
SavePNG(outmat15, 15)
SavePNG(outmat16, 16)
SavePNG(outmat17, 17)
SavePNG(outmat18, 18)
SavePNG(outmat19, 19)
SavePNG(outmat20, 20)

