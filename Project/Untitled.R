Data1 <- read.csv("/Users/AVEREE/Desktop/Evolution/Tasks/Project/Dataset1_BrainSize.csv", header=TRUE, stringsAsFactors=FALSE)
Data2 <- read.csv("/Users/AVEREE/Desktop/Evolution/Tasks/Project/SuppDataFileS1.csv", header=TRUE, stringsAsFactors=FALSE)

#head(Data1)
#tail(Data1)
#Data1[1:3,]

Data2

Bird1 <- Data1[c(1,3,4)]
head(Bird1)


Bird2 <- Data2[3:5]
head(Bird2)

?merge
MasterBird <- rbind(Bird1, Bird2)

colnames(Bird2)[2] = "brain.g"
colnames(Bird2)[3] = "body.g"

head(MasterBird)
pdf('brain_body_size_logscatterplot.pdf')
par(las=1, mar=c(4, 4, 2, 2), mgp=c(2, 0.5, 0), tck=-0.01, cex=1)
plot(log(brain.g) ~ log(body.g), data=MasterBird, xlab="body mass (g)", ylab="brain mass (g)")
abline(lm(log(brain.g) ~ log(body.g), data=MasterBird), col='red')
dev.off()

corr.brainbody <- cor.test(x=MasterBird$brain.g, y=MasterBird$body.g)

#Pearson's Correlation Coefficient = 0.7948022 
#Proves there is a positive linear relation between body mass (g) and brain mass (g)


#Hypothesis: Among species of birds, brain size will increase as body size increases as a function of pleitropy. 

