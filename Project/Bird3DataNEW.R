#NEW DATA
library(phytools)
library(ape)
library(TreeTools)


Data3 <- read.csv("/Users/AVEREE/Desktop/Evolution/Tasks/Project/SuppDataFileS1.csv", header = TRUE, stringsAsFactors = FALSE)
Bird3 <- Data3[c(3,4,5)]
head (Bird3)

pdf('Bird3.pdf')

par(las=1, mar=c(4, 4, 2, 2), mgp=c(2, 0.5, 0), tck=-0.01, cex=1)
plot(log(Brain.mass..g.) ~ log(Body.mass..g.), data=Bird3, xlab="Log of body mass (g)", ylab="Log of brain mass (g)")
abline(lm(log(Brain.mass..g.) ~ log(Body.mass..g.), data=Data3), col='red')
title("Log of Brain Mass and Body Mass")
dev.off()

phylobird <- force.ultrametric(read.tree("/Users/AVEREE/Desktop/Evolution/SuppDataFileS2.tre"))


plot(phylobird, show.tip.label = FALSE)
?plot.phylo
