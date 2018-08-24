library(ggplot2)

# IMPORTANT: UPDATE THE WORKING DIRECTORY BELOW
setwd("UPDATE_PATH/bedbugdisclosure")

gt <- read.csv("data/bedbugs_googletrend.csv")
gt$x <- 1:dim(gt)[1]

pdf("figures_supplement/sfig_googletrends.pdf", height=5, width=7.5)
ggplot() +
  geom_line(data=gt, aes(x=x, y=Search_frequency)) +
  theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16)) +
        scale_x_continuous(breaks=seq(1, dim(gt)[1], by=24), 
                           labels=c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 
                                    2018)) +
  labs(x="Year", y = "Search frequency")
dev.off()
  

