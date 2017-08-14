rm(list = ls())
library(MASS)

rdoublex = function(n, mu = 0, lambda = 1) {
	D = rexp(n, lambda)
	temp = runif(n)
	D[temp > 0.5] = -D[temp > 0.5]
	D = D + mu
	return(D)
}

rmixnorm = function(n, p = 0.5, mu1 = 0, mu2 = 0, sig1 = 1, sig2 = 100) {
	n1 = n*p
	D1 = rnorm(n1, mu1, sig1)
	D2 = rnorm(n - n1, mu2, sig2)
	D = c(D1, D2)
	return(D)
}

d = read.table("d-csp0108.txt", header=TRUE)

names = c('C', 'SP')
cols = c('red', 'blue', 'green', 'black')
dists = c('t-distribution', 'double-exponential', 'cauchy', 'mixture of normal')

for (k in 2:3) {
	p = seq(0, 1, 0.01)
	Q_data = quantile(d[,k], probs = p)
	mu_d = mean(d[,k])
	sd_d = sd(d[,k])

	refLine = seq(min(Q_data), max(Q_data), 0.01)
	plot(refLine, refLine, main = sprintf("QQ-plot of %s and Normal", names[k-1]), 
		xlab = "Normal Quantile", ylab = sprintf("%s Quantile", names[k-1]), col = 'black', type = 'l')	
	
	# t-dist	
	param = fitdistr(d[,k], "t", start = list(m = mu_d, s = sd_d, df=3))#, lower=c(-1, 0.001,1))
	param = param$estimate
	# lines(x, dt((x-mu_d)/sd_d, df=param[3])/sd_d, col = cols[1])
	Q_t = quantile(rt(length(d[,k]), df=param[3]), probs = p)
	lines(Q_t, Q_data, col = cols[1])

	# param = fitdistr(d[,k], ddoublex, start = list(mu = 0, lambda = 1))
	# param = param$estimate
	# # print(param)
	# lines(x, ddoublex((x), mu = param[1], lambda = param[2]),  col = cols[2])
	# lines(Q_norm, Q_data, col = col[2])

	# param = fitdistr(d[,k], "cauchy", start = list(location = 0, scale = 1))
	# param = param$estimate
	# lines(x, dcauchy((x-mu_d), location = param[1], scale = param[2]),  col = cols[3])
	# lines(Q_norm, Q_data, col = col[3])

	# sd1 = sd_d/2
	# p = 0.7
	# param = c(p, mu_d, mu_d, sd1, ( (sd^2 - p*sd1^2) / (1-p) )^0.5)
	# # print(sd(dmixnorm(x, p = param[1], mu1 = param[2], mu2 = param[3], sig1 = param[4], sig2 = param[5])))
	# # print(param)
	# lines(x, dmixnorm(x, p = param[1], mu1 = param[2], mu2 = param[3], sig1 = param[4], sig2 = param[5]),  col = cols[4])
	# lines(Q_norm, Q_data, col = col[4])

	legend("bottomright", legend = dists, fill = cols)
	dev.copy(png, sprintf("plots/plotb%d.png", k-1))
	dev.off ()
}