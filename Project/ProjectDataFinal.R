Data3 <- read.csv("/Users/AVEREE/Desktop/Evolution/Tasks/Project/Data3Birds.csv", header = TRUE, stringsAsFactors = FALSE)
Bird3 <- Data3[c(6,12,14)]
birdorigin <- Data3[c(6, 12, 14, 15)]

head (Bird3)

#Log-log Regression 

pdf('Bird3.pdf')
par(las=1, mar=c(4, 4, 2, 2), mgp=c(2, 0.5, 0), tck=-0.01, cex=1)
plot(log(brain.mass) ~ log(body.mass), data=Bird3, xlab="Log of body mass (g)", ylab="Log of brain mass (g)")
abline(lm(log(brain.mass) ~ log(body.mass), data=Bird3), col='red')
title("Log of Brain Mass and Body Mass")
dev.off()

#Pearson's Correlation Test
cor.test(Bird3$brain.mass, Bird3$body.mass)

#Extracting those from wild and captivity to compare 
wild <- subset(birdorigin, birdorigin$specimen.origin %in% c("wild"))

par(las=1, mar=c(4, 4, 2, 2), mgp=c(2, 0.5, 0), tck=-0.01, cex=1)
plot(log(wild$brain.mass) ~ log(wild$body.mass), xlab="Log of body mass (g)", ylab="Log of brain mass (g)")
abline(lm(log(wild$brain.mass) ~ log(wild$body.mass)), col='red')
title("Log of Brain Mass and Body Mass")
cor.test(wild$brain.mass, wild$body.mass)

captive <- subset(birdorigin, birdorigin$specimen.origin %in% c("captivity"))
par(las=1, mar=c(4, 4, 2, 2), mgp=c(2, 0.5, 0), tck=-0.01, cex=1)
plot(log(captive$brain.mass) ~ log(captive$body.mass), xlab="Log of body mass (g)", ylab="Log of brain mass (g)")
abline(lm(log(captive$brain.mass) ~ log(captive$body.mass)), col='red')
title("Log of Brain Mass and Body Mass")

cor.test(captive$brain.mass, captive$body.mass)

nrow(captive)
nrow(wild)
