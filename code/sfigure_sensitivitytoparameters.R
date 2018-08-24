library(deSolve)
library(reshape)
library(plyr)
library(dplyr)
library(ggplot2)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

# Source the functions we will need for our analysis
source("code/functions.R")
source("code/functions_extra.R")

# Set the range of parameter values we will evaluate
invgamma.range.in.months <- seq(2,12,by=2) 
invgamma.range <- invgamma.range.in.months*30
gamma.range <- 1/(invgamma.range)
k.range <- seq(0,1,by=0.2)
b.range <- seq(1,5,by=0.5)
D.range <- c(0.5, 2, 3, 4, 5, 100)*365

# -----------------------------------------------------------------------------
# 1. Average length of infestation: 
# --------------------------------

g.effect <- GetCostVaryinggamma(gamma.range)
# Match gamma to inverse gamma in months (i.e. avg months infested)
g.effect$length.infestation <- mapvalues(g.effect$gamma, from=gamma.range, 
                                       to=invgamma.range.in.months)
g.effect$length.infestation <- as.character(g.effect$length.infestation)
g.effect$length.infestation <- factor(g.effect$length.infestation, 
                                    levels=c("2", "4", "6", "8", "10", "12"))
# Separate the 1/gamma estiamte used in the primary analysis (i.e. 6 mo)
g.estimate <- filter(g.effect, length.infestation == 6)
g.effect <- filter(g.effect, length.infestation != 6)
g.estimate$length.infestation <- as.character(g.estimate$length.infestation)

# 1.1 Plot effect of uncertainty in avg length of infestation on total cost
pdf("figures_supplement/Routput/sensitivity_varyinggamma_cost.pdf", height=4, width=4.5)
ggplot() +
  geom_line(data=g.effect, aes(x=Year, y=Total_Cost, 
                                 color=length.infestation)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=g.estimate, aes(x=Year, y=Total_Cost), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Avg. length \n of infestation \n (months)") +
  labs(x="Years Since Implementation of Disclosure", y = "Cost ($)")
dev.off()

# 1.2 Plot effect of uncertainty in avg length of infestation on prevalence
pdf("figures_supplement/Routput/sensitivity_varyinggamma_prev.pdf", height=4, width=4.5)
ggplot() +
  geom_line(data=g.effect, aes(x=Year, y=Prevalence*100, 
                                 color=length.infestation)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=g.estimate, aes(x=Year, y=Prevalence*100), size=1.5, 
            linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Avg. length \n of infestation \n (months)") +
  labs(x="Years Since Implementation of Disclosure", y = "Prevalence (%)")
dev.off()


# -----------------------------------------------------------------------------
# 2. Probability of relocation transmission k:
# -------------------------------------------

k.effect <- GetCostVaryingk(k.range)
k.effect$k <- as.character(k.effect$k)

k.estimate <- GetCostVaryingk(k=0.3)
k.estimate$k <- as.character(k.estimate$k)

# 2.1 Plot effect of uncertainty in k on total cost
pdf("figures_supplement/Routput/sensitivity_varyingk_cost.pdf", height=4, width=4.5)
ggplot() +
  geom_line(data=k.effect, aes(x=Year, y=Total_Cost, color=k)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=k.estimate, aes(x=Year, y=Total_Cost), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Prob. of relocation \n transmission k") +
  labs(x="Years Since Implementation of Disclosure", y = "Cost ($)")
dev.off()

# 2.2 Plot effect of uncertainty in k on prevalence
pdf("figures_supplement/Routput/sensitivity_varyingk_prev.pdf", height=4, width=4.5)
ggplot() +
  geom_line(data=k.effect, aes(x=Year, y=Prevalence*100, color=k)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=k.estimate, aes(x=Year, y=Prevalence*100), size=1.5, 
            linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Prob. of relocation \n transmission k") +
  labs(x="Years Since Implementation of Disclosure", y = "Prevalence (%)")
dev.off()


# -----------------------------------------------------------------------------
# 3. : Vacancy multiplier b:
# -------------------------

b.effect <- GetCostVaryingb(b.range)
b.effect$b <- as.character(b.effect$b)
b.effect$b[which(b.effect$b == "1")] <- "1.0"
b.effect$b[which(b.effect$b == "2")] <- "2.0"
b.effect$b[which(b.effect$b == "3")] <- "3.0"
b.effect$b[which(b.effect$b == "4")] <- "4.0"
b.effect$b[which(b.effect$b == "5")] <- "5.0"

b.estimate <- GetCostVaryingb(1.3)
b.estimate$b <- as.character(b.estimate$b)
# 3.1 Plot effect of uncertainty in b on total cost
pdf("figures_supplement/Routput/sensitivity_varyingb_cost.pdf", height=4, width=4.5)
ggplot() +
  geom_line(data=b.effect, aes(x=Year, y=Total_Cost, color=b)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=b.estimate, aes(x=Year, y=Total_Cost), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Vacancy \n multiplier b") +
  labs(x="Years Since Implementation of Disclosure", y = "Cost ($)")
dev.off()

# 3.2 Plot effect of uncertainty in b on prevalence
pdf("figures_supplement/Routput/sensitivity_varyingb_prev.pdf", height=4, width=4.5)
ggplot() +
  geom_line(data=b.effect, aes(x=Year, y=Prevalence*100, color=b)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=b.estimate, aes(x=Year, y=Prevalence*100), size=1.5, 
            linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Vacancy \n multiplier b") +
  labs(x="Years Since Implementation of Disclosure", y = "Prevalence (%)")
dev.off()


# -----------------------------------------------------------------------------
# 4. Probability of relocation transmission k w/ b=3:
# --------------------------------------------------

k.effect <- GetCostVaryingkhighb(k.range, b.set=5)
k.effect$k <- as.character(k.effect$k)

k.estimate <- GetCostVaryingkhighb(0.3, b.set=5)
k.estimate$k <- as.character(k.estimate$k)

# 4.1 Plot effect of uncertainty in k on total cost
pdf("figures_supplement/Routput/sensitivity_varyingkHIGHb5_cost.pdf", height=4, width=5)
ggplot() +
  geom_line(data=k.effect, aes(x=Year, y=Total_Cost, color=k)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=k.estimate, aes(x=Year, y=Total_Cost), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Prob. of relocation \n transmission k") +
  labs(x="Years Since Implementation of Disclosure", y = "Cost ($)")
dev.off()

# 4.2 Plot effect of uncertainty in k on prevalence
pdf("figures_supplement/Routput/sensitivity_varyingkHIGHb5_prev.pdf", height=4, width=4.5)
ggplot() +
  geom_line(data=k.effect, aes(x=Year, y=Prevalence*100, color=k)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=k.estimate, aes(x=Year, y=Prevalence*100), size=1.5, 
            linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Prob. of relocation \n transmission k") +
  labs(x="Years Since Implementation of Disclosure", y = "Prevalence (%)")
dev.off()

# -----------------------------------------------------------------------------
# 5. Length of Disclosure D:
# -------------------------

D.effect <- GetCostVaryingD(D.range)
D.effect$D <- factor(as.character(D.effect$D/365), levels=c("0.5", "2", 
                                                            "3", "4", "5", 
                                                            "100"))

D.estimate <- GetCostVaryingD(365)
D.estimate$D <- as.character(D.estimate$D/365)

# 5.1 Plot effect of uncertainty in D on total cost
pdf("figures_supplement/Routput/sensitivity_varyingD_cost.pdf", height=4, width=5)
ggplot() +
  geom_line(data=D.effect, aes(x=Year, y=Total_Cost, color=D)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=D.estimate, aes(x=Year, y=Total_Cost), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Length of \n disclosure D (yrs)") +
  labs(x="Years Since Implementation of Disclosure", y = "Cost ($)")
dev.off()

# 5.2 Plot effect of uncertainty in D on prevalence
pdf("figures_supplement/Routput/sensitivity_varyingD_prev.pdf", height=4, width=4.5)
ggplot() +
  geom_line(data=D.effect, aes(x=Year, y=Prevalence*100, color=D)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=D.estimate, aes(x=Year, y=Prevalence*100), size=1.5, 
            linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Length of \n disclosure D (yrs)") +
  labs(x="Years Since Implementation of Disclosure", y = "Prevalence (%)")
dev.off()

# 5.3 Plot effect of uncertainty in D on VACANCY cost
pdf("figures_supplement/Routput/sensitivity_varyingD_VACANCYcost.pdf", height=4, width=5)
ggplot() +
  geom_line(data=D.effect, aes(x=Year, y=Vacancy, color=D)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=D.estimate, aes(x=Year, y=Vacancy), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Length of \n disclosure D (yrs)") +
  labs(x="Years Since Implementation of Disclosure", y = "Vacancy Cost ($)")
dev.off()

# 5.4 Plot effect of uncertainty in D on PROPORTION VACANT cost
pdf("figures_supplement/Routput/sensitivity_varyingD_PROPVACANT.pdf", height=4, width=5)
ggplot() +
  geom_line(data=D.effect, aes(x=Year, y=Prop_Vacant, color=D)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=D.estimate, aes(x=Year, y=Prop_Vacant), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Length of \n disclosure D (yrs)") +
  labs(x="Years Since Implementation of Disclosure", y = "Vacancy Cost ($)")
dev.off()

# -----------------------------------------------------------------------------
# 6. Length of Disclosure D with d=1:
# ----------------------------------

D.effect <- GetCostVaryingD(D.range, d=1)
D.effect$D <- factor(as.character(D.effect$D/365), levels=c("0.5", "2", 
                                                            "3", "4", "5", 
                                                            "100"))

D.estimate <- GetCostVaryingD(365, d=1)
D.estimate$D <- as.character(D.estimate$D/365)

# 6.1 Plot effect of uncertainty in D on total cost
pdf("figures_supplement/Routput/sensitivity_varyingD_d1_cost.pdf", height=4, width=5)
ggplot() +
  geom_line(data=D.effect, aes(x=Year, y=Total_Cost, color=D)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=D.estimate, aes(x=Year, y=Total_Cost), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Length of \n disclosure D (yrs)") +
  labs(x="Years Since Implementation of Disclosure", y = "Cost ($)")
dev.off()

# 6.2 Plot effect of uncertainty in D on prevalence
pdf("figures_supplement/Routput/sensitivity_varyingD_d1_prev.pdf", height=4, width=4.5)
ggplot() +
  geom_line(data=D.effect, aes(x=Year, y=Prevalence*100, color=D)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=D.estimate, aes(x=Year, y=Prevalence*100), size=1.5, 
            linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Length of \n disclosure D (yrs)") +
  labs(x="Years Since Implementation of Disclosure", y = "Prevalence (%)")
dev.off()

# 6.3 Plot effect of uncertainty in D on VACANCY cost
pdf("figures_supplement/Routput/sensitivity_varyingD_d1_VACANCYcost.pdf", height=4, width=5)
ggplot() +
  geom_line(data=D.effect, aes(x=Year, y=Vacancy, color=D)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=D.estimate, aes(x=Year, y=Vacancy), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Length of \n disclosure D (yrs)") +
  labs(x="Years Since Implementation of Disclosure", y = "Vacancy Cost ($)")
dev.off()

# 6.4 Plot effect of uncertainty in D on PROPORTION VACANT cost
pdf("figures_supplement/Routput/sensitivity_varyingD_d1_PROPVACANT.pdf", height=4, width=5)
ggplot() +
  geom_line(data=D.effect, aes(x=Year, y=Prop_Vacant, color=D)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24)) +
  geom_line(data=D.estimate, aes(x=Year, y=Prop_Vacant), size=1.5, linetype=2) +
  theme_bw() +
  scale_color_discrete(name="Length of \n disclosure D (yrs)") +
  labs(x="Years Since Implementation of Disclosure", y = "Vacancy Cost ($)")
dev.off()
