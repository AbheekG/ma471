rm(list = ls())
d = read.table("d-csp0108.txt", header=TRUE)
names = c('C', 'SP')
T = length(d[,1]);

# Calculating log returns
for (k in 2:3) {
	d[,k] = log(1 + d[,k]);
}

skew <- function(X) {
	T = length(X);
	mu = mean(X);
	sig = sqrt( sum((X-mu)^2)/(T-1) );
	sk = sum((X-mu)^3)/(T-1) / sig^3;
	return (sk);
}

kurt <- function(X) {
	T = length(X);
	mu = mean(X);
	sig = sqrt( sum((X-mu)^2)/(T-1) );
	kt = sum((X-mu)^4)/(T-1) / sig^4;
	return (kt);
}

alpha = 0.05;
type = c('Skewness', 'Excess Kurtosis');

for (k in 2:3) {
	for (ty in type) {
		X = d[,k];

		if (ty == 'Skewness') {
			theta = skew(X);
			sig = sqrt(6/T);
			clb = sig*qnorm(alpha/2); cub = sig*qnorm(1 - alpha/2);
		} else {
			theta = kurt(X) - 3;
			sig = sqrt(24/T);
			clb = sig*qnorm(alpha/2); cub = sig*qnorm(1 - alpha/2);
		}
		

		cat(sprintf('\n%s Stock\\\\\n', names[k-1]));
		cat(sprintf('The %d\\%% confidence interval for %s = [%f, %f]\\\\\n', 100*(1-alpha), ty, clb, cub));
		if ((clb <= theta) && (theta <= cub)) {
			cat(sprintf('The %s = %f is inside the confidence interval. Hypothesis True\\\\\n\n', ty, theta));
		} else {
			cat(sprintf('The %s = %f is not inside the confidence interval. Hypothesis False\\\\\n\n', ty, theta));
		}
	}
}
