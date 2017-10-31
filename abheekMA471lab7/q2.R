contGumbel <- function(theta, mu, sig, df) {
	copula <- function(u, v) {
		y = exp(-((-log(u))^theta + (-log(v))^theta)^(1/theta));
		return (y);
	}
	pdf_copula <- function(u, v) {
		y = (dnorm(qnorm(u, mean = mu, sd = sig), mean = mu, sd = sig) * 
			dt(qt(v, df = df), df = df) *
			(exp(-((-log(u))^theta + (-log(v))^theta)^(1/theta))*(-log(u))^(theta - 1)*(-log(v))^(theta - 1)*((-log(u))^theta + (-log(v))^theta)^(2/theta - 2))/(u*v) - (theta*exp(-((-log(u))^theta + (-log(v))^theta)^(1/theta))*(-log(u))^(theta - 1)*(-log(v))^(theta - 1)*(1/theta - 1)*((-log(u))^theta + (-log(v))^theta)^(1/theta - 2))/(u*v));
	}
	n = 100;
	X_ = seq(1/n, 1 - 1/n, length.out = n);
	Y_ = X_;
	Z = numeric();
	for (i in 1:n) {
		for (j in 1:n) {
			Z = c(Z, pdf_copula(X_[i], Y_[j]));
		}
	}

	X = qnorm(X_, mean = mu, sd = sig);
	Y = qt(Y_, df = df);
	# print(X); print(Y); print(Z);
	Z = matrix(Z, nrow = n, ncol = n, byrow = TRUE);
	persp(X, Y, Z, xlab = "Quantile from N(3,4)", ylab = "Quantile from T(3)", zlab = "Density",
	 col = 'red', main = "Density of bivariate gumbel copula with parameter 1.4");
	dev.copy(png, "plots/plot_q2_1.png"); dev.off ();
	contour(X, Y, Z, main = "Density of bivariate gumbel copula with parameter 1.4");
	dev.copy(png, "plots/plot_q2_2.png"); dev.off ();

	Z = numeric();
	for (i in 1:n) {
		for (j in 1:n) {
			Z = c(Z, copula(X_[i], Y_[j]));
		}
	}
	Z = matrix(Z, nrow = n, ncol = n, byrow = TRUE);
	persp(X, Y, Z, xlab = "Quantile from N(3,4)", ylab = "Quantile from T(3)", zlab = "CDF",
	 col = 'red', main = "CDF of bivariate gumbel copula with parameter 1.4");
	dev.copy(png, "plots/plot_q2_3.png"); dev.off ();
	contour(X, Y, Z, main = "CDF of bivariate gumbel copula with parameter 1.4");
	dev.copy(png, "plots/plot_q2_4.png"); dev.off ();
}


theta = 1.4;
mu = 3;
sig = 2;
df = 3;
contGumbel(theta, mu, sig, df);