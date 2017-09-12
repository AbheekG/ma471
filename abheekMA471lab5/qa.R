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
posterior <- function(sig2) {
	return (coeff * posterior1(sig2));
}

m = 1000;
Y = sort(rexp(m));
W = posterior(Y)/dexp(Y);
W = W / sum(W);

clb = 0; cub = Inf;

s = 0; k = 0;
for (i in 1:m) {
	s = s + W[i];
	if ((s >= alpha/2) && (k == 0)) {
		k = 1;
		clb = Y[i];
	}
	if ((s >= 1 - alpha/2) && (k == 1)) {
		k = 2;
		cub = Y[i];
	}
}

cat(sprintf('\nThe %d%% confidence interval for sig^2 = [%f, %f]\n\n', 100*(1-alpha), clb, cub));