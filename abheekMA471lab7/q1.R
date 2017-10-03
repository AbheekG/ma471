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
	persp(X, Y, Z, xlab = "u", ylab = "v", zlab = "Copula",
	 main = "Surface plot of bivariate gumbel copula with parameter 1.5");
	dev.copy(png, "plots/plot_q1_1.png"); dev.off ();
}

surfNormal <- function(theta) {
	copula <- function(u, v) {
		X = matrix(c(qnorm(u), qnorm(v)), nrow=2, ncol=1);
		Sig = matrix(c(1, theta, theta, 1), nrow=2, ncol=2);
		y = det(2*pi*Sig)^(-1/2) * exp(-t(X) %*% Sig^(-1) %*% X / 2);
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
	persp(X, Y, Z, xlab = "u", ylab = "v", zlab = "Copula",
	 main = "Surface plot of bivariate normal copula with parameter 0.7");
	dev.copy(png, "plots/plot_q1_2.png"); dev.off ();
}

surfGumbel(1.5);
surfNormal(0.7);