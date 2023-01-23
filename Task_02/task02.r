#Part A
setwd('~/Desktop/Evolution/Tasks/Task_02')
Data1 <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
Data2 <- read.csv('http://jonsmitchell.com/data/cyrus.csv', stringsAsFactors=F)
write.csv(Data2, 'rawdata.csv', quote=F)
Data1
head(Data1)
GlargleBrgle <- Data1
head(Data1)
length(Data1)
nrow(Data1)
ncol(Data1)
colnames(Data1)
head(Data1)
Data1[1,]
Data1[2,]
Data1[1:3,]
Data1[1:3, 4]
Data1[1:5, 1:3]
Data1[257, ]
Feeds <- which(Data1[,9] == 'bottle')
berenMilk <- Data1[Feeds,]
head(berenMilk)
Feeds <- which(Data1[,'event'] == 'bottle')
Feeds <- which(Data1$event == 'bottle')
dayID <- apply(Data1, 1, function(x) paste (x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data1$age <- dateID - dateID[which(Data1$event == 'birth')]
head(Data1)
beren2 <- Data1
beren3 <- beren2[order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)                
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
?cor
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
berenANOVA
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz")
?par
par(las = 1, mar = c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf('r02b-totalMilkByDay.pdf', height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()






#Part B
source("http://jonsmitchell.com/code/plotFxn02b.R")
# summarize the day count
Days <- unique(beren3$age[Feeds])
ndays <- length(Days)

# create a set of custom colors
Alpha <- 0.75
divCols <- c(rgb(158/255,1/255,66/255, Alpha),rgb(213/255,62/255,79/255, Alpha),rgb(244/255,109/255,67/255, Alpha),rgb(253/255,174/255,97/255, Alpha),rgb(254/255,224/255,139/255, Alpha),rgb(255/255,255/255,191/255, Alpha),rgb(230/255,245/255,152/255, Alpha),rgb(171/255,221/255,164/255, Alpha),rgb(102/255,194/255,165/255, Alpha),rgb(50/255,136/255,189/255, Alpha),rgb(94/255,79/255,162/255, Alpha))
Pal <- colorRampPalette(divCols, interpolate = "spline", alpha = T)

# subset the custom colors
Cols <- Pal(ndays)
names(Cols) <- Days

par(mar=c(4,4,1,1), las=1, mfrow=c(1, 2), mgp=c(2, 0.5, 0), tck=-0.01, cex.axis=1, cex.lab=1.2)

# make a blank plot
plot(1, 1, xlim=c(7, 16), ylim=c(0, 20), type="n", xlab="time of day", ylab="total milk (oz)")

# add data to the plot
for (Day in Days)	{
  FeedsOnDay <- which(beren3$age[Feeds] == Day)
  Times <- beren3$start_hour[Feeds[FeedsOnDay]] + (beren3$start_minute[Feeds[FeedsOnDay]] / 60)
  Amounts <- cumsum(beren3$value[Feeds[FeedsOnDay]])
  points(Times, Amounts, col=Cols[as.character(Day)], type="b", pch=16)
}

# add a legend to the plot
dayVec <- c(min(Days), Days[floor(0.25*ndays)], median(Days), Days[ceiling(0.75*ndays)], max(Days))
legend("top", legend=dayVec, pch=21, pt.bg=Cols[as.character(dayVec)], horiz = T, bty = "n", pt.cex=2, title = "age in days")


# make a nap plot!
Naps <- which(beren2$event == "nap")

startT <- beren2$start_hour[Naps] + ( beren2$start_minute[Naps] / 60 )
endT <- beren2$end_hour[Naps] + ( beren2$end_minute[Naps] / 60 )
napTs <- endT - startT

plot(1,1,type="n", xlim=c(125, 700), ylim=c(7, 16), xlab="age (days)", ylab="nap time")
x <- tapply(1:length(Naps), beren2$age[Naps], function(x) segments(beren2$age[Naps][x], startT[x], beren2$age[Naps][x], endT[x]))
head(Data1)
Mass <- which(Data1[,9] == 'trait_mass')
berenMass <- Data1[Mass,]
Mass<-which(Data1[,'event'] == 'trait_mass')
head(berenMass)
dayID <- apply(Data1, 1, function(x)paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin="2019-04-18")
Data1$age <- dateID - dateID[which(Data1$event == 'birth')]
head(Data1)
head(berenMass)
beren4 <- berenMass
Mass<-which(beren3$event == "trait_mass")
avgMass <- mean(beren3$value[Mass])
avgMass <- tapply(beren3$value[Mass], beren3$age[Mass],mean)
varMass <- tapply(beren3$value[Mass], beren3$age[Mass], var)
totalMass <- tapply(beren3$value[Mass], beren3$age[Mass], sum)
numMass <- tapply(beren3$value[Mass], beren3$age[Mass], length)
cor(beren3$value[Mass], beren3$age[Mass])
cor.test(beren3$value[Mass], beren3$age[Mass])
berenCor <- cor.test(beren3$value[Mass], beren3$age[Mass])
summary(berenCor)
berenANOVA <- aov(beren3$value[Mass]~beren3$caregiver[Mass])
boxplot(beren3$value[Mass]~beren3$age[Mass], xlab="Age (days)", ylab="Mass (kg)")
par(las=1,mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab="Age(days)", ylab="Mass(kg)")
abline(h=mean(totalMass), lty=2, col='red')
setwd('')
head(Data2)
Mass<-which(Data2[,9] == 'trait_mass')
cyrusMass <- Data2[Mass,]
Mass <- which(Data2[,'event'] == 'trait_mass')
Mass <- which(Data2$event == 'trait_mass')
head(cyrusMass)
dayID <- apply(Data2, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2")
Data2$age <- dateID - dateID[which(Data2$event == 'birth')]
head(Data2)
cyrus2<-Data2
cyrus3<-cyrus2[order(cyrus2$age),]
cyrus3
write.csv(cyrus2, 'cyrus_new.csv', quote=F, row.names=FALSE)
Mass<-which(cyrus3$event == "trait_mass")
avgMass <- mean(cyrus3$value[Mass])
avgMass
avgMass <- tapply(cyrus3$value[Mass], cyrus3$age[Mass],mean)
varMass <- tapply(cyrus3$value[Mass], cyrus3$age[Mass], var)
totalMass <- tapply(cyrus3$value[Mass], cyrus3$age[Mass], sum)
numMass <- tapply(cyrus3$value[Mass], cyrus3$age[Mass], length)
cor(cyrus3$value[Mass], cyrus3$age[Mass])
cor.test(cyrus3$value[Mass], cyrus3$age[Mass])
cyrusCor <- cor.test(cyrus3$value[Mass], cyrus3$age[Mass])
summary(cyrusCor)
cyrusANOVA <- aov(cyrus3$value[Mass]~cyrus3$caregiver[Mass])
boxplot(cyrus3$value[Mass]~cyrus3$age[Mass], xlab="Age (days)", ylab="Mass (kg)")
par(las=1,mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab="Age(days)", ylab="Mass(kg)")
abline(h=mean(totalMass), lty=2, col='red')
head(cyrus2)
head(cyrusMass)
?points
plot(berenMass$value[Mass]~berenMass$age[Mass],xlab="Age (days)", ylab="Mass (kg)")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", col="blue", pch=16,xlab="Age (days)", ylab="Mass (kg)")
points(cyrusMass$value[Mass]/1000~cyrusMass$age[Mass], col="green", pch=16, xlab="Age in days", ylab="Mass in kg")
title(("Cyrus and Beren Age vs Mass"), xlab = "Age (days)", ylab = "Mass (kg)")
?plot
abline(h=mean(totalMass), lty=2, col="red")
cyrusMass
?mar

#Question 01: Hypothesis I is too vague, there is no description of the amount Beren eats daily. 
#Hypothesis II is vague, too. There is no specificity to the correlation between 
#how much Beren naps and how much he drinks daily. 

#Question 02: I believe the x-axis would require smaller units for slightly more accurate interpretation. Overall, the 
#graph has the data too close together, with lots of points of overlapping making it difficult to read.

#EXTRA CREDIT
unique(cyrusMass$event)
which(cyrusMass$event == "trait_mass")
cmass <- which(cyrusMass$event == "trait_mass")
avgMass <- mean(cyrusMass$value[Mass])
avgGrowth <- tapply(cyrusMass$value[Mass], cyrusMass$age[Mass], mean)
varGrowth <- tapply(cyrusMass$value[Mass], cyrusMass$age[Mass], var)
totalGrowth <- tapply(cyrusMass$value[Mass], cyrusMass$age[Mass], sum)
numGrowth <- tapply(cyrusMass$value[Mass], cyrusMass$age[Mass], length)
cor(cyrusMass$value[Mass], cyrusMass$age[Mass])
cor.test(cyrusMass$value[Mass], cyrusMass$age[Mass])
cyrusreg <- lm(cyrusMass$value[Mass] ~ cyrusMass$age[Mass])
summary(cyrusreg)
attributes(cyrusreg)
plot(cyrusMass$value[Mass]/1000~cyrusMass$age[Mass], col="green", pch=16, xlab="Age in days", ylab="Mass in kg")
cyrusMass
(10999-10003)
996/2
(11170-10999)
(498+171)/2
11170+334.5+334.5+334.5
12173.5/1000
(4710+4450+4422+5103+6095+8730+10003+10999+11170)/9
#I predict Cyrus will weigh 12.17kg +/- 0.574kg

