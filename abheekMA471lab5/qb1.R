rm(list = ls())
d = read.table("d-csp0108.txt", header=TRUE)
names = c('C', 'SP')
n = length(d[,1]);

# Calculating log returns
for (k in 2:3) {
	d[,k] = log(1 + d[,k]);
}

N = c(50, n);
alpha = 0.05;

for (k in 2:3) {
	X = d[,k];
	mu_total = mean(X);
	for (n in N) {
		mu = mean(X[1:n]);
		sig = sd(X[1:n]);

		clb = sig*qnorm(alpha/2)/sqrt(n); cub = sig*qnorm(1 - alpha/2)/sqrt(n);

		cat(sprintf('\n%s Stock, %d samples\\\\\n', names[k-1], n));
		cat(sprintf('The %d%% confidence interval for mean = [%f, %f]\\\\\n', 100*(1-alpha), clb, cub));
		if ((clb <= mu) && (mu <= cub)) {
			cat(sprintf('The mean = %f is inside the confidence interval.\\\\\n\n', mu));
		} else {
			cat(sprintf('The mean = %f is not inside the confidence interval.\\\\\n\n', mu));
		}
	}	
}
