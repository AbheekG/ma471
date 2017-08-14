rm(list = ls())
d = read.table("d-csp0108.txt", header=TRUE)

k = 2
plot(d[,1], d[,k], main="C return vs Time", xlab="Time", ylab="Return")
dev.copy(png, "plots/plota1.png")
dev.off ()
k = 3
plot(d[,1], d[,k], main="SP return vs Time", xlab="Time", ylab="Return")
dev.copy(png, "plots/plota2.png")
dev.off ()

k = 2
hist(d[,k], freq=FALSE, breaks=100, col='green', main='Probability Distribution of C returns', xlab='C returns')
x <- seq(-0.5, 0.5, length=1000)
lines(x, dnorm(x, mean(d[,k]), sd(d[,k])))
dev.copy(png, "plots/plota3.png")
dev.off ()

k = 3
hist(d[,k], freq=FALSE, breaks=100, col='green', main='Probability Distribution of SP returns', xlab='SP returns')
x <- seq(-0.5, 0.5, length=1000)
lines(x, dnorm(x, mean(d[,k]), sd(d[,k])))
dev.copy(png, "plots/plota4.png")
dev.off ()
