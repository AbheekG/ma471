rm(list = ls())

n = 100
X = rnorm(n, mean = 0, sd = sqrt(5))
sum_X2 = sum(X^2)

prior <- function (sig2) {
	return ((sig2^-3.5) * exp(-1/(2*sig2)))
}

likelihood <- function (sig) {
	return (exp(-sum_X2/(2*sig^2)) / (sqrt(2*pi)*sig)^n)
}

posteriorDensity <- function (sig) {
	return (likelihood(sig)*prior(sig^2))
}

# Getting the Gamma distribution
k = 1/integrate(posteriorDensity, 0, Inf)$value
t = seq(0, 5, length=1000)
dens = k*posteriorDensity(t)

png("posteriorDensity.png")
plot(t, dens, xlab="x", ylab="Density", type='l')
title("Density of Posterior Inverse-Gamma distribution")
dev.off()

# Bayes Estimate
bayesEstimate <- function (t) {
	return (t*k*posteriorDensity(t))
}

be = integrate(bayesEstimate, 0, Inf)$value
print(sprintf("Bayes estimate = %f", be))

# MAP estimator
map = optimize(posteriorDensity, interval=c(0, 30), maximum=TRUE)$maximum
print(sprintf("MAP estimate = %f", map))