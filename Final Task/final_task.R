x <- rnorm(100, mean=0, sd = 2) 
w <- runif(100, min=0, max=0.1)
y <- (x*5+2+w)

reg <- lm(y~x)
cf <- coef(reg)

plot1 <- plot(x,y, xlim=c(-5, 6), ylim=c(-100,30))
abline(reg, col='red')

#Y-intercept = 2
#Slope = 5
x <- c()
w <- c()
z <- c()
v <- c()
y <- c()
slope1 <- c()

for (i in 1:100){
  x[i] <- rnorm(100, mean=0, sd = 2)
  w[i] <- runif(100, min=0, max=0.1)
  z[i] <- rnorm(1)
  v[i] <- x*z
  y[i] <- (v[i]*5+2+w[i])
  reg <- lm(y~x)
  slope <- coef(reg)
  slope1[i] <- slope["x"]
}

pdf("plot10.pdf")
plot(z, slope1)
abline(lm(slope1 ~ z), col='red')
reg1 <- lm(slope1 ~ z)
coef(reg1)
dev.off()

#Slope and y-int are 0 

#Monty Hall Bonus 
door <- c("A", "B", "C")
data = c()
for (i in 1:10000){
  prize <- sample(door)[1]
  pick <- sample(door)[1]
  open <- sample(door[which(door!=pick&door!=prize)])[1]
  switches <- door[which(door!=pick&door!=open)]
  if(pick==prize){data=c(data, "noswitching")}
  if(switches==prize){data=c(data, "switching")}
}
y<-length(which(data=="switching"))
x<-length(which(data=="noswitching"))

height <- c(x,y)
pdf("plot10.ec01.pdf")
barplot(height, col=c("pink", "green"), ylab="Chances of Winning")
title("Chances of Winning a Prize")
legend(x="topleft", c("Switching", "No Switching"), col=c("pink","green"),
       border="black", lwd =2, cex=1)
dev.off()
?legend

#Meme Bonus 
install.packages("meme")
install.packages("magick")
install.packages("memery")
library("meme")
?meme

jpeg("meme3.jpeg")
meme(("https://i.imgflip.com/7jloxl.jpg"), "When you spend 5 hours trying to 
            fix one issue on R","and Dr. Mitchell fixes it in 5 seconds", size=2.0)
dev.off()

#Nicole Kester and I worked together to finish this R assignment 

