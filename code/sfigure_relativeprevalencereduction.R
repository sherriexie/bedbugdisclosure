library(RColorBrewer)
library(oce)
library(lattice)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

# Set the x- and y- axis breaks, i.e. baseline prevalence and disclosure index,
# respectively
bprev.vec <- seq(0.001, 0.1, length.out = 100)
d.vec <- seq(0.01, 1, length.out = 100)

# -----------------------------------------------------------------------------
# 1. Load and prep data: 
# ---------------------

# 1.1 Load costmatrix output ----
cm1  <- readRDS("output_costmatrix/costmat_yr1.rds")
cm2  <- readRDS("output_costmatrix/costmat_yr2.rds")
cm3  <- readRDS("output_costmatrix/costmat_yr3.rds")
cm4  <- readRDS("output_costmatrix/costmat_yr4.rds")
cm5  <- readRDS("output_costmatrix/costmat_yr5.rds")
cm20 <- readRDS("output_costmatrix/costmat_yr20.rds")

# 1.2 Solve for relative prevalence reduction
GetRPR <- function(mat){
  fprev <- mat[,c("prevalence")]
  bprev <- mat[,c("baseline.prevalence")]
  rel.prev.red <- ((bprev - fprev)/bprev)*100
  mat <- cbind(mat, rel.prev.red)
  colnames(mat)[length(colnames(mat))] <- "rel.prev.red"
  return(mat)
}

cm1  <- GetRPR(cm1)
cm2  <- GetRPR(cm2)
cm3  <- GetRPR(cm3)
cm4  <- GetRPR(cm4)
cm5  <- GetRPR(cm5)
cm20 <- GetRPR(cm20)

# 1.3 Extract relative prev reduction and format into matrix for plotting ----
#     Make a 100 x 100 matrix of prevalence where each cell is the cost for a 
#     given baseline prevalence and disclosure index
#     Note: prevalence makes up the first column of the cm matrix
p1    <- cm1[,9]
prev1 <- matrix(p1, nrow=length(bprev.vec), ncol=length(d.vec))

p2    <- cm2[,9]
prev2 <- matrix(p2, nrow=length(bprev.vec), ncol=length(d.vec))

p3    <- cm3[,9]
prev3 <- matrix(p3, nrow=length(bprev.vec), ncol=length(d.vec))

p4    <- cm4[,9]
prev4 <- matrix(p4, nrow=length(bprev.vec), ncol=length(d.vec))

p5    <- cm5[,9]
prev5 <- matrix(p5, nrow=length(bprev.vec), ncol=length(d.vec))

p20    <- cm20[,9]
prev20 <- matrix(p20, nrow=length(bprev.vec), ncol=length(d.vec))

# -----------------------------------------------------------------------------
# 2. Color prep for 3d plots:
# ----------------
#    -   Code adapted from:
#        https://stackoverflow.com/questions/39117827/colorful-plot-using-persp
#    -   Set our color palette so that high prevalence is red, and low 
#        prevalence is yellow
#    -   Map prevalence values to our color palette

myPalette <- colorRampPalette(rev(brewer.pal(7, "YlOrRd")))
colors <- myPalette(100)

MapColors <- function(mat){
  z <- mat
  z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
  colmap <- colormap(z = as.vector(z.facet.center), 
                     breaks = seq(0, 100, length.out=101), col = colors)
  colmat <- matrix(colmap$zcol, nrow = 99, ncol = 99)
  return(colmat)
}

colmat1  <- MapColors(prev1)
colmat2  <- MapColors(prev2)
colmat3  <- MapColors(prev3)
colmat4  <- MapColors(prev4)
colmat5  <- MapColors(prev5)
colmat20 <- MapColors(prev20)


# -----------------------------------------------------------------------------
# 3. Make 3D color plots:
# -----------------------
# Save plots as a pdf to be formatted in keynote to make Figure S

pdf("figures_supplement/Routput/3dprevreduction_fixedz.pdf", height=4.5, width=4.5)

persp(bprev.vec, d.vec, prev1, phi=25, theta=-35, 
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity",
      zlab = "\n RPR (%)", border=NA, zlim=c(0,100),
      main = "Prev. Reduction Year 1", col=colmat1)

persp(bprev.vec, d.vec, prev2, phi=25, theta=-35, 
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity",
      zlab = "\n RPR (%)", border=NA, zlim=c(0,100),
      main = "Prev. Reduction Year 2", col=colmat2)

persp(bprev.vec, d.vec, prev3, phi=25, theta=-35, 
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity",
      zlab = "\n RPR (%)", border=NA, zlim=c(0,100),
      main = "Prev. Reduction Year 3", col=colmat3)

persp(bprev.vec, d.vec, prev4, phi=25, theta=-35, 
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity",
      zlab = "\n RPR (%)", border=NA, zlim=c(0,100),
      main = "Prev. Reduction Year 4", col=colmat4)

persp(bprev.vec, d.vec, prev5, phi=25, theta=-35, 
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity",
      zlab = "\n RPR (%)", border=NA, zlim=c(0,100),
      main = "Prev. Reduction Year 5", col=colmat5)

persp(bprev.vec, d.vec, prev20, phi=25, theta=-35, 
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity",
      zlab = "\n RPR (%)", border=NA, zlim=c(0,100),
      main = "Prev. Reduction Year 20", col=colmat20)

dev.off()

# -----------------------------------------------------------------------------
# 4. Make color legend using levelplot from the lattice package:
# -------------------------------------------------------------
# Only the lengend for this plot will be used to make Figure S2

pdf("figures_supplement/Routput/sfigure_RPR_legend.pdf", width = 4.5, height = 4.5)

levelplot(prev2, row.values=bprev.vec, col.values=d.vec, aspect="fill",
          col.regions=colors, at=seq(0, 100, length.out=101))

dev.off()

