rm(list=ls())

shape = 2
scale = 1
count = c(20, 40, 100, 200)

log_likelihood <- function(beta, theta, X)
{
	n = length(X)
	temp1 = sum(log(X))
	temp3 = X^beta
	temp3 = vector(,n)
	for(i in 1:n)
		temp3[i] = X[i]^beta
	temp3 = sum(temp3)
	return(n*log(beta) + n*beta*log(theta) + (beta-1)*temp1 - (theta^beta)*temp3 )
}

log_likelihood_beta <- function(beta, X)
{
	n = length(X)
	temp1 = log(X)
	temp3 = X^beta
	temp2 = sum(temp1*temp3)
	temp1 = sum(temp1)
	temp3 = sum(temp3)
	return(n/beta - n*temp2/temp3 + temp1)
}

log_likelihood_beta_prime <- function(beta, X)
{
	n = length(X)
	temp1 = log(X)
	temp3 = X^beta
	temp2 = temp1*temp3
	temp4 = (temp1*temp1) %*% temp3 # scalar
	temp2 = sum(temp2)
	temp3 = sum(temp3)
	return( -n/(beta^2) -n*( (temp2^2 - temp3*temp4)/(temp3^2) ) )
}

newton_raphson <- function(f, f_prime, X, tol=1e-5, x0=1, N=100)
{
	i=1
	x1 = x0
	p = numeric(N)
	while (i<=N)
	{
		df.dx = (f(x0 + tol,X) - f(x0,X))/tol
		x1 = x0 - (f(x0,X)/df.dx)
		p[i] = x1
		i = i + 1
		if(abs(x1 - x0) < tol)
			break
		x0 = x1
	}
	return(x0)
}

estimateParam <- function(X)
{
	beta0 = newton_raphson(log_likelihood_beta,log_likelihood_beta_prime, X)
	temp3 = sum(X^beta0)
	theta0 = (length(X)/temp3)^(1/beta0)
	return(c(beta0, theta0))
}

for (k in count) {
	data = rweibull(k, shape, scale)
	# LL <- function(beta, theta) {
	# 	z = matrix(0, length(beta), length(theta))
	# 	for (i in 1:length(beta)) {
	# 		for (j in 1:length(theta)) {
	# 			z[i, j] = log_likelihood(beta[i], 1/theta[j], data)
	# 		}
	# 	}

	# 	z = log_likelihood(beta, 1/theta, data)

	# 	return(z)
	# }

	# y = x = seq(0.1, 10, 0.1)
	# z <- outer(x, y, LL)
	# persp(x, y, z, main = "Log likelihood vs shape, scale", xlab = 'shape', ylab = 'scale', zlab = 'log-likelihood')

	# dev.copy(png, sprintf("plots/plotd%d_1.png", k))
	# dev.off ()

	# for (i in 1:length(x)) {
	# 	y[i] = log_likelihood_beta(x[i], data)
	# }
	# plot(x, y, main = sprintf("Profile likelihood for %d samples", k), 
	# 	xlab = "beta", ylab = "Log likelihood", col = 'black', type = 'l')

	# dev.copy(png, sprintf("plots/plotd%d_2.png", k))
	# dev.off ()

	param = estimateParam(data)
	cat(sprintf("For %d samples, estimates of shape = %f, scale = %f, mean square error = %f\n", k, param[1], 1/param[2], 
		(((param[1] - shape)^2 + (1/param[2] - scale)^2)/2)^0.5 ))

}

