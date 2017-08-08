d = read.table("d-csp0108.txt", header=TRUE)

k = 2
plot(d[,1], d[,k], main="C return vs Time", xlab="Time", ylab="Return")
k = 3
plot(d[,1], d[,k], main="SP return vs Time", xlab="Time", ylab="Return")

k = 2
hist(d[,k], freq=FALSE, breaks=100, col='green', main='Probability Distribution of C returns', xlab='C returns')
x <- seq(-4, 4, length=1000)
lines(x, dnorm(x, mean(d[,k]), sd(d[,k])))

k = 3
hist(d[,k], freq=FALSE, breaks=100, col='green', main='Probability Distribution of SP returns', xlab='SP returns')
x <- seq(-4, 4, length=1000)
lines(x, dnorm(x, mean(d[,k]), sd(d[,k])))
