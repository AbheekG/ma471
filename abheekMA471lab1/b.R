rm(list = ls())
library(MASS)

ddoublex = function(x, mu = 0, lambda = 1) {
	a = abs(x - mu)
	return (dexp(a, lambda)/2)
}

dmixnorm = function(x, p = 0.5, mu1 = 0, mu2 = 0, sig1 = 1, sig2 = 100) {
	return (p*dnorm(x, mu1, sig1) + (1-p)*dnorm(x, mu2, sig2))
}

d = read.table("d-csp0108.txt", header=TRUE)

names = c('C', 'SP')
cols = c('red', 'blue', 'green', 'black')
dists = c('t-distribution', 'double-exponential', 'cauchy', 'mixture of normal')

for (k in 2:3) {
	x <- seq(-0.5, 0.5, length=1000)
	mu_d = mean(d[,k])
	sd_d = sd(d[,k])

	hist(d[,k], prob=T, breaks=100, main = sprintf("Probability Distribution Function (PDF) of %s returns", names[k-1]), 
		xlab = sprintf("%s returns", names[k-1]), ylab = "Probability Density")
	
	
	# t-dist	
	param = fitdistr(d[,k], "t", start = list(m = mu_d, s = sd_d, df=3))#, lower=c(-1, 0.001,1))
	param = param$estimate
	lines(x, dt((x-mu_d)/sd_d, df=param[3])/sd_d, col = cols[1])

	param = fitdistr(d[,k], ddoublex, start = list(mu = 0, lambda = 1))
	param = param$estimate
	# print(param)
	lines(x, ddoublex((x), mu = param[1], lambda = param[2]),  col = cols[2])

	param = fitdistr(d[,k], "cauchy", start = list(location = 0, scale = 1))
	param = param$estimate
	lines(x, dcauchy((x-mu_d), location = param[1], scale = param[2]),  col = cols[3])

	sd1 = sd_d/2
	p = 0.7
	param = c(p, mu_d, mu_d, sd1, ( (sd_d^2 - p*sd1^2) / (1-p) )^0.5)
	# print(param)
	lines(x, dmixnorm(x, p = param[1], mu1 = param[2], mu2 = param[3], sig1 = param[4], sig2 = param[5]),  col = cols[4])

	legend("topright", legend = dists, fill = cols)
	dev.copy(png, sprintf("plots/plotb%d.png", k-1))
	dev.off ()
}