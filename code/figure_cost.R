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

# 1.2 Extract total cost and format into matrix for plotting ----
#     Make a 100 x 100 matrix of total cost where each cell is the cost for a 
#     given baseline prevalence and disclosure index
#     Note: total cost makes up the first column of the cm matrix
c1    <- cm1[,1]
cost1 <- matrix(c1, nrow=length(bprev.vec), ncol=length(d.vec))

c2    <- cm2[,1]
cost2 <- matrix(c2, nrow=length(bprev.vec), ncol=length(d.vec))

c3    <- cm3[,1]
cost3 <- matrix(c3, nrow=length(bprev.vec), ncol=length(d.vec))

c4    <- cm4[,1]
cost4 <- matrix(c4, nrow=length(bprev.vec), ncol=length(d.vec))

c5    <- cm5[,1]
cost5 <- matrix(c5, nrow=length(bprev.vec), ncol=length(d.vec))

c20 <- cm20[,1]
cost20 <- matrix(c20, nrow=length(bprev.vec), ncol=length(d.vec))

# -----------------------------------------------------------------------------
# 2. Color prep for 3d plots:
# ----------------
#    -   Code adapted from:
#        https://stackoverflow.com/questions/39117827/colorful-plot-using-persp


# 2.1 Set color palette ----

# Get the min and max total cost -- these will form the upper and lower bounds
# of the color palette
max(c(cost1, cost2, cost3, cost4))  # 366 --> 367
min(cost20)                         # -280.49 -- > -281
maxcost <- 367
mincost <- -281

# Set our color palette so that red colors represent positive costs, blue 
# colors represent negative costs (i.e. savings), and $0 fall approximately
# at neutral colors
preprePalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))
prePalette <- preprePalette(101)
myPalette <- colorRampPalette(prePalette[c(1:26, seq(28,50,by=2), 51:101)])
colors <- myPalette(100)

# 2.2 Map total cost to colors in our palette ----

MapColors <- function(mat){
  z <- mat
  z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
  colmap <- colormap(z = as.vector(z.facet.center), 
                     breaks = seq(mincost, maxcost, length.out=101), col = colors)
  colmat <- matrix(colmap$zcol, nrow = 99, ncol = 99)
  return(colmat)
}

colmat1  <- MapColors(cost1)
colmat2  <- MapColors(cost2)
colmat3  <- MapColors(cost3)
colmat4  <- MapColors(cost4)
colmat5  <- MapColors(cost5)
colmat20 <- MapColors(cost20)


# -----------------------------------------------------------------------------
# 3. Make 3D color plots:
# -----------------------
# Save plots as a pdf to be formatted in keynote to make Figure 3

pdf("figures/Routput/3dcost.pdf", height=4.5, width=4.5)
p <- persp(bprev.vec, d.vec, cost1, phi=25, theta=-35,
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity", zlab = "\n Cost ($)",
      main = "Total Cost of Disclosure Year 1", border=NA,
      col=colmat1)
persp(bprev.vec, d.vec, cost2, phi=25, theta=-35,
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity", zlab = "\n Cost ($)",
      main = "\n Total Cost of Disclosure Year 2", border=NA,
      col=colmat2)
persp(bprev.vec, d.vec, cost3, phi=25, theta=-35,
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity", zlab = "\n Cost ($)",
      main = "\n Total Cost of Disclosure Year 3", border=NA,
      col=colmat3)
persp(bprev.vec, d.vec, cost4, phi=25, theta=-35,
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity", zlab = "\n Cost ($)",
      main = "\n Total Cost of Disclosure Year 4", border=NA,
      col=colmat4)
persp(bprev.vec, d.vec, cost5, phi=25, theta=-35,
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity", zlab = "\n Cost ($)",
      main = "\n Total Cost of Disclosure Year 5", border=NA,
      col=colmat5)
persp(bprev.vec, d.vec, cost20, phi=25, theta=-35,
      xlab="\n Baseline prev. (%)", ylab="\n Renter selectivity", zlab = "\n Cost ($)",
      main = "\n Total Cost of Disclosure Year 20",  border=NA,
      col=colmat20)
dev.off()

# -----------------------------------------------------------------------------
# 4. Make color legend using levelplot from the lattice package:
# -------------------------------------------------------------
# Only the lengend for this plot will be used to make Figure 3

pdf("figures/Routput/3dcost_legend.pdf", width = 4.5, height = 4.5)

levelplot(cost2, row.values=bprev.vec, col.values=d.vec, aspect="fill",
          col.regions=colors, at=seq(mincost, maxcost, length.out=101))

dev.off()
