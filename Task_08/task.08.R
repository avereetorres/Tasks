library("phytools")

#Questions 1-3
trees <- list()
births <- vector()
Fractions <- vector()

?pbtree

for (i in 1:100){
  births[[i]] <- runif(1)
  Fractions[[i]] <- runif(1)
  trees[[i]] <- pbtree(n=100, b=births[[i]], d=(births[[i]]*Fractions[i]))
}

trees[[i]]

?tiplabels
?Ntip.multiPhylo
?Ntip
?Ntip.phylo

extrate <- births * Fractions

#Question 4
logs <- log(sapply(trees, Ntip))
netdiv <- (births - extrate)
plot(netdiv, logs, xlab="net diversification", ylab="log of tips")
abline(lm(logs~netdiv), col="red")

#There is a positive correlation between the log of the number of tips and net diversification rate

?length
?averageTree

#Question 5
avgbranchlength <- vector()
for(i in 1:100){
  births[[i]] <- runif(1)
  Fractions[[i]] <- runif(1)
  trees[[i]] <- pbtree(n=100, b=births[[i]], d=(births[[i]]*Fractions[i]))
  avgbranchlength[i] <- mean(trees[[i]]$edge.length)
}

plot(births, avgbranchlength, xlab="speciation rate", ylab="average branch length")

#Question 6
cor(births, avgbranchlength)
#There is a negative correlation between speciation rate and average branch length 

?which

#Question 7
MTree <- which.max(sapply(trees, Ntip))
Tree <- trees[[MTree]]
plot(Tree)
rates <- vector()
traits <- list()
Tree
for(i in 1:100){
  rates[i] <- runif(1)
  traits[[i]] <- fastBM(tree=Tree, sig2=rates[i])
}

?fastBM

#Question 8
meantraits <- sapply(traits, mean)
cor(meantraits, rates)
plot(meantraits, rates, xlab="Mean of Traits", ylab="Rates")
abline(lm(meantraits~rates), col="red")
#There is a negative correlation between the mean of traits and rates

#Question 9 
vartraits <- sapply(traits, var)
cor(vartraits, rates)
#There is a positive correlation between the variance of traits and rates 

#Question 10
pdf("08_plot.pdf")
traits1 <- traits[[1]]
traits2 <- traits[[2]]
cor(traits2, traits1)
traitMat <- cbind(traits[[24]], traits[[50]])
plot(traitMat)
dev.off()

cor.test(traitMat[,1], traitMat[,2])$p.value

