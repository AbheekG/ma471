rm(list = ls())

n = 100;
alpha = 0.05;
sig = sqrt(5);

X = rnorm(n, mean = 0, sd = sig);
sum_X2 = sum(X^2);

prior <- function(sig2) {
	return (sig2^(-7/2) * exp(-0.5/sig2));
}

likelihood <- function (sig2) {
	return (exp(-sum_X2/(2*sig2)) / sqrt(2*pi*sig2)^n)
}

posterior1 <- function(sig2) {
	y = likelihood(sig2)*prior(sig2);
	return (y);
}

coeff = 1/integrate(posterior1, 0, Inf)$value;
posterior <- function(x) {
	return (coeff * posterior1(x^2));
}

rpost = function(n) {
	a = seq(0.01, 100, 0.01);
	b = max(posterior(a)/dexp(a, rate = 1/2));
}

m = 200;
S = rpost(m);