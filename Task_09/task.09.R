

setwd('/Users/AVEREE/Desktop/Evolution/Tasks/Task_09')

install.packages("diversitree")
library(diversitree)

transition_0to1 <- 0.1
transition_1to0 <- 0.1 

speciation_0 <- 0.2 
extinction_0 <- 0.1

speciation_1 <- 0.2 
extinction_1 <- 0.1 

maxN <- 1e3
maxT <- 50 

Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)

simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(simTree)

?tree.bisse

stateTable <- table(simTree$tip.state)
stateTable / sum(stateTable)



library("ape")
library("phytools")

pdf("Tree1.pdf")
plotTree(simTree)

pdf("Graph1.pdf")
plot(stateTable)
dev.off()

#1
transition_0to1 <- 0.1
transition_1to0 <- 0.1 

speciation_2 <- 0.2 
extinction_2 <- 0.1

speciation_3 <- 0.5 
extinction_3 <- 0.1 

maxN <- 1e3
maxT <- 50 

Pars2 <- c(speciation_2, speciation_3, extinction_2, extinction_3, transition_0to1, transition_1to0)

simTree2 <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(simTree2)

stateTable2 <- table(simTree2$tip.state)
stateTable2 / sum(stateTable2)

pdf("Question.1.pdf")
plot(stateTable2)
dev.off()
#Net diversification rate was larger for speciation/extinction 3 (representing state 1) than 2 (representing state 0). It appears
#this did have an effect as state 1 was less than state 0 in this graph. In the previous graph, state 1 was larger. This means that 
#more lineages have the trait than not in state 1 when the diversification rate is higher for the trait. 

#2
transition_0to1 <- 2
transition_1to0 <- -100

speciation_2 <- 0.2 
extinction_2 <- 0.1

speciation_3 <- 0.2 
extinction_3 <- 0.1 

maxN <- 1e3
maxT <- 1

Pars2 <- c(speciation_2, speciation_3, extinction_2, extinction_3, transition_0to1, transition_1to0)

simTree3 <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(simTree3)

stateTable3 <- table(simTree3$tip.state)
stateTable3 / sum(stateTable3)

pdf("Question.2.pdf")
plot(stateTable3)
dev.off()
#A range of values was used from -100 to 100 for both transition rates and I could never get the frequency of state 1 to 0. Even when I set the transition rates
#to zero state values would not become 0.  


#3
transition_0to1 <- 0.1
transition_1to0 <- 0.1

speciation_2 <- 0.1 
extinction_2 <- 0.1

speciation_3 <- 0.1 
extinction_3 <- 0.1 

maxN <- 1e3
maxT <- 50

Pars2 <- c(speciation_2, speciation_3, extinction_2, extinction_3, transition_0to1, transition_1to0)

simTree3 <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(simTree3)

stateTable3 <- table(simTree3$tip.state)
stateTable3 / sum(stateTable3)

pdf("Question.3.pdf")
plot(stateTable3)
dev.off()

var(stateTable3)
#When all of the parameters are the same the variation is equal to 0. This is because 
#state 1 and state 0 both equal 0.5. 

#4
transition_0to1 <- 0.1
transition_1to0 <- 0.1

speciation_2 <- 0.2 
extinction_2 <- 0.1

speciation_3 <- 0.2 
extinction_3 <- 0.1 

maxN <- 1e3
maxT <- 3

Pars2 <- c(speciation_2, speciation_3, extinction_2, extinction_3, transition_0to1, transition_1to0)

simTree3 <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(simTree3)

stateTable3 <- table(simTree3$tip.state)
stateTable3 / sum(stateTable3)


#When maxT = 1, all lineages are state 1, there is not even a state 0. When maxT = 3, state 0 and state 1 are equal. 
#Therefore, maxT has an effect on the frequency of state 1. 


#Crazy Interesting Plot 
transition_021 <- 0.1 
transition_120 <- 0.1 

sp0 <- 0.5
ex0 <- 0

sp1 <- 0.5
ex1 <- 0 

maxN <- 1e3
maxT <- 50

Pars2 <- c(sp0, sp1, ex0, ex1, transition_021, transition_120)

interesting <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(interesting)

interestingtable <- table(interesting$tip.state)
interestingtable / sum(interestingtable)

rel0 <- c(0.5173979, 0.425, 0.4457831, 0.4646465, 0.495098, 0.5165794, 0.4651639, 0.6458333, 0.7619048, 0.4929006, 0.53125)
rel1 <- c(0.4826021, 0.575, 0.5542169, 0.5353535, 0.504902, 0.4834206, 0.5348361, 0.3541667, 0.2380952, 0.5070994, 0.46875)

merged <- rbind(rel0, rel1)
netdivers <- c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5)

pdf("09_plot.pdf")
plot(rel0, netdivers, xlab ="Relative Frequency", ylab = "Net Diversification Rate", xlim=c(0.2, 0.8))
points(rel1, netdivers, col='red')
title("Relative Frequency and Net Diversification Rate")
legend(x="bottomright", legend=c("State 0", "State 1"), col=c(1,'red'), lwd=2)
dev.off()

#This is a graph of the net diversification rate and relative frequency. This graph was interesting
#to me because I created another graph where I changed the net diversification rate in increments of 0.1
#for state 0, but not state 1. The graph created previously yielded the same, where points for state 1 and state 0 
#were always opposite of one another in frequency. Therefore, from this we can determine that for this binary trait
#if an evolutionary force favors the speciation or extinction of having the trait, it will inversely effect the speciation
#and extinction of those who do not have the trait. It was also interesting because it looks like the cancer awareness 
#symbol (upside down). 


