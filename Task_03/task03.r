5
A <- 5
A * 2
B <- c(A, 5, 5)
B 
B * 2
'B'*2
B <- c(A, 5, 5)
B * 2 
C <- c(B, 6, 10)
C * 6
D <- c(1, 5, 10, 15, 20)
sum(D)
mean(D)
min(D)
E <- c(6, 7, 8,  9, 10)

which(E == 8)
which(E > 8)
E[which(E>8)]

trueMean1<-5
trueSD1<-5
population1<-rnorm(1e6, trueMean1, trueSD1)

trueMean2<-4
trueSD2<-5
population2<-rnorm(1e6, trueMean2, trueSD2)

Size<-50
Sample1<-sample(population1, Size)
Sample2<-sample(population2, Size)

boxplot(Sample1, Sample2)

individual1<-c("B", "A")
individual2<-c("A", "A")

Gam1<-sample(individual1, 1)
Gam2<-sample(individual2, 1)
newBaby<-c(Gam1, Gam2)
newBaby

isHet<-c()

for(i in 1:100) {
  Gam1<- sample(individual1, 1)  
  Gam2<-sample(individual2, 1)
  newBaby<-c(Gam1, Gam2)
  isHet[i]<-Gam1 == Gam2
}  
sum(isHet)/length(isHet)

source("http://jonsmitchell.com/code/simFxn04.R")

MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")

Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)

Focus <- makeBaby(Brenda, Alan)

ToMom <- length(grep("mom", Focus))/length(Focus)
ToMom

ToMomMom <- length(grep("grandma_mom", Focus))/length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus))/length(Focus)
ToMomMom
ToMomDad
ToMomMom+ToMomDad

ToDadMom <- length(grep("grandma_da", Focus))/length(Focus)
ToDadDad <- length(grep("grandpa_da", Focus))/length(Focus)
ToDadMom
ToDadDad
ToDadMom+ToDadDad

Sibling_01 <- makeBaby(Brenda, Alan)

ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)
ToSib

ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))

quantile(ManySiblings)
mean(ManySiblings)

hist(ManySiblings, col="blue", border="white", xlab="Relation to Focus", ylab="Number of Siblings")



     
?hist


#The distribution of values in this analysis are explainable because while each 
#child has 50% of moms genetics and 50% of dads genetics, it is completely random
#what genes you will actually get from mom and dad as a product of recombination.
#Therefore, each child will not have the same DNA, and their relation to Focus will
#vary. 

