contGumbel <- function(theta, mu, sig, df) {
	copula <- function(u, v) {
		y = exp(-((-log(u))^theta + (-log(v))^theta)^(1/theta));
		return (y);
	}
	n = 100;
	X = seq(1/n, 1 - 1/n, length.out = n);
	Y = X;
	Z = numeric();
	for (i in 1:n) {
		for (j in 1:n) {
			Z = c(Z, copula(X[i], Y[j]));
		}
	}

	X = qnorm(X, mean = mu, sd = sig);
	Y = qt(Y, df = df);
	# print(X); print(Y); print(Z);
	Z = matrix(Z, nrow = n, ncol = n, byrow = TRUE);
	persp(X, Y, Z, xlab = "Quantile from N(3,4)", ylab = "Quantile from T(3)", zlab = "Density",
	 main = "Density of bivariate gumbel copula with parameter 1.4");
	dev.copy(png, "plots/plot_q2_1.png"); dev.off ();
	contour(X, Y, Z, main = "Density of bivariate gumbel copula with parameter 1.4");
	dev.copy(png, "plots/plot_q2_2.png"); dev.off ();

	for (i in 2:n) {
		for (j in 2:n) {
			Z[i, j] = Z[i-1, j] + Z[i, j-1] - Z[i-1, j-1];
		}
	}
	persp(X, Y, Z, xlab = "Quantile from N(3,4)", ylab = "Quantile from T(3)", zlab = "CDF",
	 main = "CDF of bivariate gumbel copula with parameter 1.4");
	dev.copy(png, "plots/plot_q2_3.png"); dev.off ();
	contour(X, Y, Z, main = "CDF of bivariate gumbel copula with parameter 1.4");
	dev.copy(png, "plots/plot_q2_4.png"); dev.off ();
}


theta = 1.4;
mu = 3;
sig = 2;
df = 3;
contGumbel(theta, mu, sig, df);