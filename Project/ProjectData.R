setwd("/Users/AVEREE/Desktop/Evolution/Tasks/Project/")
Data1 <- read.csv("Dataset1_BrainSize.csv", header=TRUE, stringsAsFactors=FALSE)
Data2 <- read.csv("/Users/AVEREE/Desktop/Evolution/Tasks/Project/SuppDataFileS1.csv", header=TRUE, stringsAsFactors=FALSE)

library(phytools)
library(ape)
library(TreeTools)
tree <- read.nexus("hackett.tre")

#head(Data1)
#tail(Data1)
#Data1[1:3,]

head(Data1)
Bird1 <- Data1[c(1,3,4)]
head(Bird1)


Bird2 <- Data2[3:5]
head(Bird2)

?merge
colnames(Bird2)[2] = "brain.g"
colnames(Bird2)[3] = "body.g"

MasterBird <- rbind(Bird1, Bird2)

head(MasterBird)


pdf('brain_body_size_logscatterplot.pdf')
par(las=1, mar=c(4, 4, 2, 2), mgp=c(2, 0.5, 0), tck=-0.01, cex=1)
plot(log(brain.g) ~ log(body.g), data=MasterBird, xlab="body mass (g)", ylab="brain mass (g)")
abline(lm(log(brain.g) ~ log(body.g), data=MasterBird), col='red')
dev.off()



#Pearson's Correlation Coefficient = 0.7948022 
#There is a positive linear relation between body mass (g) and brain mass (g)


#Hypothesis: Among species of birds, brain size will increase as body size increases as a function of pleitropy. 

Data3 <- read.csv("/Users/AVEREE/Desktop/Evolution/Tasks/Project/Data3Birds.csv", header = TRUE, stringsAsFactors = FALSE)
Bird3 <- Data3[c(6,12,14)]
Bird4 <- Data3[c(12, 14)]
write.csv(Data3[4:5], "birdnames.csv", quote=F)
Bird3x <- Bird3
#NEW DATA

Data3 <- read.csv("/Users/AVEREE/Desktop/Evolution/Tasks/Project/SuppDataFileS1.csv", header = TRUE, stringsAsFactors = FALSE)
Bird3 <- Data3[c(3,4,5)]
head (Bird3)
pdf('Bird3.pdf')

par(las=1, mar=c(4, 4, 2, 2), mgp=c(2, 0.5, 0), tck=-0.01, cex=1)
plot(log(brain.mass) ~ log(body.mass), data=Bird3, xlab="Log of body mass (g)", ylab="Log of brain mass (g)")
abline(lm(log(brain.mass) ~ log(body.mass), data=Data3), col='red')
title("Log of Brain Mass and Body Mass")
dev.off()
Cols <- c('#f7fcfd','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6','#8c6bb1','#88419d','#810f7c','#4d004b', '#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac',
          '#084081', '#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a', '#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c',
          '#bd0026','#800026', '#ffffff','#f0f0f0','#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525','#000000', '#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0',
          '#225ea8','#253494','#081d58')
names(Cols) <- unique(Bird3$species)



phylobird <- ReadTntTree("/Users/AVEREE/Desktop/Evolution/SuppDataFileS2.tre")
kms <- ape::read.tree(text = 'Bird3$species')

brainx <- c(Bird3$brain.mass)

corr.brainbody <- cor.test(x=Bird3$brain.mass, y=Bird3$body.mass)


meanbrain <- 4.618291
meanbody <- 719.9688
stdbrain <- 5.405244
stdbody <- 1751.98

Tminbrain <- meanbrain-(3*stdbrain)
Tmaxbrain <- meanbrain +(3*stdbrain)

Bird3$species[which(Bird3$brain.mass < Tminbrain | Bird3$brain.mass > Tmaxbrain)]

removedbrain <- Bird3$brain.mass[which(Bird3$brain.mass > Tminbrain & Bird3$brain.mass < Tmaxbrain)]

Tminbody <- meanbody-(3*stdbody)
Tmaxbody <- meanbody+(3*stdbody)

Bird3$species[which(Bird3$body.mass < Tminbody | Bird3$body.mass > Tmaxbody)]

removedbody <- [which(Bird3$body.mass > Tminbody & Bird3$body.mass) < Tmaxbody]
                               
finaldata <- removedbody[which(Bird4$brain.mass > Tminbrain & Bird4$brain.mass < Tmaxbrain)]
head(Bird3)
which.max(Bird3$brain.mass)
Bird3$species[62]

which.min(Bird3$brain.mass)
Bird3$species[72]

which(log(Bird3$body.mass) < 1.5)
?cor.test


