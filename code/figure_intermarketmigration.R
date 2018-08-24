library(deSolve)
library(reshape)
library(dplyr)
library(ggplot2)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

# Source the functions we will need for our analyses.
source("code/functions.R")
source("code/functions_extra.R")

# Colors used for immigrant fraction (Blues):
b1 <- "#92c5de"
b2 <- "#4393c3"
b3 <- "#2166ac"
b4 <- "#053061"
col.b <- c(b1, b2, b3, b4)

# Colors used for external prevalence (Reds):
a1 <- "#f4a582"
a2 <- "#d6604d"
a3 <- "#b2182b"
a4 <- "#67001f"
col.a <- c(a1, a2, a3, a4)


# -----------------------------------------------------------------------------
# 1. IMMIGRATION FRACTION: 
# -----------------------
# Setting e = 0.1 and ranging i from 0 to 0.5 (in incrments of 0.1)
immigration.effect <- GetCostVaryingi(i.range=seq(0.1, 0.4, length.out=4), 
                                      e.set=0.05)
immigration.ref <- GetCostVaryingi(i.range=0, e.set=0.05)

# 1.1 Effect of immigration on total cost ----
immigration.effect$i <- as.character(immigration.effect$i*100)

# -- determine what y-axis limit and labels should be
max(immigration.effect$Total_Cost)  # 11
min(immigration.ref$Total_Cost)  # -68

pdf("figures/Routput/IMM_varyingi_cost.pdf", height=4, width=4.9)
ggplot() +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_line(data=immigration.effect, aes(x=Year, y=Total_Cost, color=i), size=1.5) +
  geom_line(data=immigration.ref, aes(x=Year, y=Total_Cost), size=1.5, linetype=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_text(vjust=-2), axis.ticks.x=element_blank()) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16)) +
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  scale_color_manual(name="i (%)", values=col.b) +
  scale_y_continuous(breaks=seq(-70, 10, by=10)[c(1,3,5,7,8,9)]) +
  labs(x="Years Since Implementation of Disclosure", y = "Cost ($)")
dev.off()

# 1.2 Effect of immigration on final prevalence ----

# -- determine what y-axis limit and labels should be
max(immigration.effect$Prevalence)  # 0.05
min(immigration.ref$Prevalence)  # 0.0197

pdf("figures/Routput/IMM_varyingi_prev.pdf", height=4, width=4.64)
ggplot() +
  geom_line(data=immigration.effect, aes(x=Year, y=Prevalence*100, color=i), 
            size=1.5) +
  geom_line(data=immigration.ref, aes(x=Year, y=Prevalence*100), size=1.5, 
            linetype=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_text(vjust=-2), axis.ticks.x=element_blank()) +  
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16),
        legend.text = element_text(size=16), legend.title = element_text(size=16)) +
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  scale_color_manual(name="i (%)", values=col.b) +
  scale_y_continuous(breaks=2:5) +
  labs(x="Years Since Implementation of Disclosure", y = "Prevalence (%)")
dev.off()

# -----------------------------------------------------------------------------
# 2. EXTERNAL PREVALENCE: 
# ----------------------
# Set i = 0.05 and range e from 0 to 0.5 (in increments of 0.04)
externalprev.effect <- GetCostVaryinge(i.set=0.2, 
                                       e.range=seq(0.05,0.2, length.out=4))

# 2.1 Effect of external prevalence on total cost ----
externalprev.effect$e <- as.character(externalprev.effect$e*100)
externalprev.effect$e <- factor(externalprev.effect$e, 
                                levels=c("5", "10", "15", "20"))

# -- determine what y-axis limit and labels should be
summary(externalprev.effect$Total_Cost)
# >   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# > -51.50  -32.62  -24.19  -21.80  -11.00   11.18 


pdf("figures/Routput/IMM_varyinge_cost.pdf", height=4, width=4.9)
ggplot() +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_line(data=externalprev.effect, aes(x=Year, y=Total_Cost, color=e),
            size=1.5) +
  geom_line(data=immigration.ref, aes(x=Year, y=Total_Cost), size=1.5, 
            linetype=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_text(vjust=-2), axis.ticks.x=element_blank()) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16)) +
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  scale_color_manual(name="e (%)", values=col.a) +
  scale_y_continuous(breaks=seq(-70, 10, by=10)[c(1,3,5,7,8,9)]) +
  labs(x="Years Since Implementation of Disclosure", y = "Cost ($)")
dev.off()

# 2.2 Effect of external prevalence on final prevalence ----

pdf("figures/Routput/IMM_varyinge_prev.pdf", height=4, width=4.64)
ggplot() +
  geom_line(data=externalprev.effect, aes(x=Year, y=Prevalence*100, color=e),
            size=1.5) +
  geom_line(data=immigration.ref, aes(x=Year, y=Prevalence*100), size=1.5, 
            linetype=2) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_text(vjust=-2), axis.ticks.x=element_blank()) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16)) +
  theme(plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  scale_color_manual(name="e (%)", values=col.a) +
  labs(x="Years Since Implementation of Disclosure", y = "Prevalence (%)")
dev.off()


