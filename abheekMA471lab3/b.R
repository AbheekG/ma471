library(mixtools)
rm(list=ls())

expectationMaximization <- function(X, lm, mu1, mu2, sig1, sig2) {
	tol = 10^-5; lim = 100; i = 0

	n = length(X)
	temp1 = dnorm(X, mean = mu1, sd = sig1)
	temp2 = dnorm(X, mean = mu2, sd = sig2)
	M1 = (lm*temp1) / (lm*temp1 + (1-lm)*temp2)
	M2 = 1 - M1
	Q = -1

	repeat {
		lmt = lm; mu1t = mu1; mu2t = mu2; sig1t = sig1; sig2t = sig2;
		i = i + 1
		Q0 = Q

		lm = sum(M1)/n
		mu1 = sum(M1 * X) / sum(M1)
		mu2 = sum(M2 * X) / sum(M2)
		sig1 = sqrt(sum(M1 * (X - mu1)^2 ) / sum(M1))
		sig2 = sqrt(sum(M2 * (X - mu2)^2 ) / sum(M2))

		temp1 = dnorm(X, mean = mu1, sd = sig1)
		temp2 = dnorm(X, mean = mu2, sd = sig2)
		M1 = (lm*temp1) / (lm*temp1 + (1-lm)*temp2)
		M2 = 1 - M1

		Q = sum(M1*log(lm*temp1) + M2*log((1-lm)*temp2))

		if (abs(Q-Q0) < tol || i > lim) {
		# if ((abs(lm-lmt)+abs(mu1t-mu1)+abs(mu2t-mu2)+abs(sig1t-sig1)+abs(sig2t-sig2) < tol)) {
			# print(i)
			break
		}
	}

	return (list(lm = lm, mu1 = mu1, mu2 = mu2, sig1 = sig1, sig2 = sig2))
}

n = 200

lm = 0.4
mu1 = mu2 = 0
sig1 = 1
sig2 = 5

temp_U = runif(n)
temp_X1 = rnorm(n, mean = mu1, sd = sig1)
temp_X2 = rnorm(n, mean = mu2, sd = sig2)

X = temp_X1*(temp_U <= 0.4) + temp_X2*(temp_U > 0.4)

print("From manual implementation of EM")
param = expectationMaximization(X, 0.5, 0, 0, 1, 10);
print( sprintf("lambda = %f, mu1 = %f, mu2 = %f, sig1 = %f, sig2 = %f", 
	param$lm, param$mu1, param$mu2, param$sig1, param$sig2))

print("From built in function normalmixEM")
param = normalmixEM(X)
print( sprintf("lambda = %f, mu1 = %f, mu2 = %f, sig1 = %f, sig2 = %f", 
	param$lambda[1], param$mu[1], param$mu[2], param$sigma[1], param$sigma[2]))