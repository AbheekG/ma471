rm(list = ls())
library(MASS)

ddoublex = function(x, mu = 0, lambda = 1) {
	a = abs(x - mu)
	return (dexp(a, lambda)/2)
}

dmixnorm = function(x, p = 0.5, mu1 = 0, mu2 = 0, sig1 = 1, sig2 = 100) {
	return (p*dnorm(x, mu1, sig1) + (1-p)*dnorm(x, mu2, sig2))
}

rdoublex = function(n, mu = 0, lambda = 1) {
	D = rexp(n, lambda)
	temp = runif(n)
	D[temp > 0.5] = -D[temp > 0.5]
	D = D + mu
	return(D)
}

rmixnorm = function(n, p = 0.5, mu1 = 0, mu2 = 0, sig1 = 1, sig2 = 100) {
	n1 = as.integer(n*p)
	D1 = rnorm(n1, mu1, sig1)
	D2 = rnorm(n - n1, mu2, sig2)
	D = c(D1, D2)
	return(D)
}

d = read.table("d-csp0108.txt", header=TRUE)

names = c('C', 'SP')
cols = c('red', 'blue', 'green', 'brown')
dists = c('t-distribution', 'double-exponential', 'cauchy', 'mixture of normal')

for (k in 2:3) {
	p = seq(0, 1, 0.01)
	Q_data = quantile(d[,k], probs = p)
	mu_d = mean(d[,k])
	sd_d = sd(d[,k])

	refLine = seq(min(Q_data), max(Q_data), 0.01)
	plot(refLine, refLine, main = sprintf("QQ-plot of %s and Normal", names[k-1]), 
		xlab = "Base distribution Quantile", ylab = sprintf("%s Quantile", names[k-1]), col = 'black', type = 'l')	
	
	# t-dist	
	param = fitdistr(d[,k], "t", start = list(m = mu_d, s = sd_d, df=3))#, lower=c(-1, 0.001,1))
	param = param$estimate
	# lines(x, dt((x-mu_d)/sd_d, df=param[3])/sd_d, col = cols[1])
	Q_t = qt(p, df=param[3])*sd_d + mu_d
	lines(Q_t, Q_data, col = cols[1])

	param = fitdistr(d[,k], ddoublex, start = list(mu = 0, lambda = 1))
	param = param$estimate
	# print(param)
	Q_doublex = quantile(rdoublex(length(p), mu = param[1], lambda = param[2]), probs = p)
	lines(Q_doublex, Q_data, col = cols[2])

	param = fitdistr(d[,k], "cauchy", start = list(location = 0, scale = 1))
	param = param$estimate
	Q_cauchy = quantile(rcauchy(length(p), location = param[1], scale = param[2]), probs = p)
	lines(Q_cauchy, Q_data, col = cols[3])

	sd1 = sd_d/2
	pp = 0.7
	param = c(pp, mu_d, mu_d, sd1, ( (sd_d^2 - pp*sd1^2) / (1-pp) )^0.5)
	Q_mixnorm = quantile(rmixnorm(length(p), p = param[1], mu1 = param[2], mu2 = param[3], sig1 = param[4], sig2 = param[5]), probs = p)
	lines(Q_mixnorm, Q_data, col = cols[4])

	legend("bottomright", legend = dists, fill = cols)
	dev.copy(png, sprintf("plots/plotc%d.png", k-1))
	dev.off ()
}