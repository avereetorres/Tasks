set(wd'~/Desktop/Evolution/Tasks/Task_04')
source("http://jonsmitchell.com/code/fxn05.R")

Pop1<-simPop(Popsize=50, nGenerations=100,initial_p=0.5, h=1, s=0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0,1), type="l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend=c("a", "b"), col=c("black", "red"), lwd=2, bty="n")

plotFit(nruns=10, n=50, ngens=100, init_p=0.5, h=1, s=0)

Expectation <- c(10,10,10,10)
Observed <- c(15, 15, 5, 5)

Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2~"="~.(Chisq)), legend.text=c("expected", "observed"))

Observed <- c(5, 0, 0, 35)
Observed <- c(2, 3, 10, 30)

Observed <- c(40)

?simDraws 

