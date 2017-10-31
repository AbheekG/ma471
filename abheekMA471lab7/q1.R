library(mvtnorm)
surfGumbel <- function(theta) {
	copula <- function(u, v) {
		y = exp(-((-log(u))^theta + (-log(v))^theta)^(1/theta));
		return (y);
	}
	n = 100;
	X = seq(0, 1, length.out = n);
	Y = X;
	Z = numeric();
	for (i in 1:n) {
		for (j in 1:n) {
			Z = c(Z, copula(X[i], Y[j]));
		}
	}

	Z = matrix(Z, nrow = n, ncol = n);
	persp(X, Y, Z, xlab = "u", ylab = "v", zlab = "Copula",  col = "red",
	 main = "Surface plot of bivariate gumbel copula with parameter 1.5");
	dev.copy(png, "plots/plot_q1_1.png"); dev.off ();
}

surfNormal <- function(theta) {
	copula <- function(u, v) {
		x_up = qnorm(u); y_up = qnorm(v);
		Sig = matrix(c(1, theta, theta, 1), nrow=2, ncol=2);
		pmvnorm(lower = c(-Inf, -Inf), upper = c(x_up, y_up), mean = c(0, 0), corr = Sig);
		# pdf <- function(x, y) {			
		# 	X = matrix(X, nrow=2, ncol=1);
		# 	ans = det(2*pi*Sig)^(-1/2) * exp(-t(X) %*% Sig^(-1) %*% X / 2);
		# 	return (ans)
		# }
		# integrate(function(y) {
		# 	sapply(y, function(y) {
		# 		integrate(function(x) {
		# 			sapply(x, function(x) pdf(x, y))}, -10, x_up)$value
		# 		})
		# 	}, -10, y_up)
	}

	n = 100;
	X = seq(0, 1, length.out = n);
	Y = X;
	Z = numeric();
	for (i in 1:n) {
		for (j in 1:n) {
			Z = c(Z, copula(X[i], Y[j]));
		}
	}

	Z = matrix(Z, nrow = n, ncol = n);
	persp(X, Y, Z, xlab = "u", ylab = "v", zlab = "Copula", col = "red",
	 main = "Surface plot of bivariate normal copula with parameter 0.7");
	dev.copy(png, "plots/plot_q1_2.png"); dev.off ();
}

surfGumbel(1.5);
surfNormal(0.7);