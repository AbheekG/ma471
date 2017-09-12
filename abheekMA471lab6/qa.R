rm(list = ls())
d = read.table("d-csp0108.txt", header=TRUE)
names = c('C', 'SP')
n = length(d[,1]);

# Calculating log returns
for (k in 2:3) {
	d[,k] = log(1 + d[,k]);
}

N = c(50, 100, 500, 1000, n);
NN = seq(50, n, 50);
alpha = 0.05;

for (k in 2:3) {
	X = d[,k];
	mu_total = mean(X);
	for (n in N) {
		mu = mean(X[1:n]);
		sig = sd(X[1:n]);

		clb = sig*qnorm(alpha/2)/sqrt(n); cub = sig*qnorm(1 - alpha/2)/sqrt(n);

		cc = exp(-qnorm(1 - alpha/2)^2/2);

		cat(sprintf('\n%s Stock, interval constructed using %d samples\\\\\n', names[k-1], n));
		cat(sprintf('Using Likelihood Ratio Test to find confidence interval.\\\\\nHypothesis rejected if ratio less than %f\\\\\n', cc));
		cat(sprintf('The %d\\%% confidence interval for mean = [%f, %f]\\\\\n', 100*(1-alpha), clb, cub));
		if ((clb <= mu) && (mu <= cub)) {
			cat(sprintf('The mean = %f is inside the confidence interval.\\\\\n', mu));
		} else {
			cat(sprintf('The mean = %f is not inside the confidence interval.\\\\\n', mu));
		}

		for (nn in NN) {
			conf = 0;
			for (i in 1:nn) {
				s = sample(n, n, replace=T);
				mu_test = mean(X[s]);
				if ((clb+mu <= mu_test) && (mu_test <= cub+mu)) {
					conf = conf + 1;
				}
			}
			cat(sprintf('For %d samples coverage probability = %f\\\\\n', nn, conf/nn));
		}
	}	
}
