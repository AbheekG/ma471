rm(list = ls())
d = read.table("d-csp0108.txt", header=TRUE)
names = c('C', 'SP')
methods = c('boot-t', 'boot percentile');
n = length(d[,1]);

# Calculating log returns
for (k in 2:3) {
	d[,k] = log(1 + d[,k]);
}

N = c(50, n);
alpha = 0.05;
B = 1000;
m = 100;

for (method in methods) {
	for (k in 2:3) {
		X = d[,k];
		mu_total = mean(X);
		for (n in N) {
			mu = mean(X[1:n]);
			sig = sd(X[1:n]);

			if (method == 'boot-t') {
				T = numeric(B);
				for (b in 1:B) {
					s = sample(n, m, replace = TRUE);
					Y = X[s];
					T[b] = sqrt(m)*(mean(Y) - mu)/sd(Y);
				}
				CI = quantile(T, c(alpha/2, 1- alpha/2));
				CI = CI*sig/sqrt(n);
			} else {
				M = numeric(B);
				for (b in 1:B) {
					s = sample(n, m, replace = TRUE);
					Y = X[s];
					M[b] = mean(Y);
				}
				CI = quantile(M, c(alpha/2, 1- alpha/2));
			}

			# print(CI);
			clb = CI[1]; cub = CI[2];
			cat(sprintf('\n%s Stock, %d samples, Method = %s\\\\\n', names[k-1], n, method));
			cat(sprintf('The %d%% confidence interval for mean = [%f, %f]\\\\\n', 100*(1-alpha), clb, cub));
			if ((clb <= mu) && (mu <= cub)) {
				cat(sprintf('The mean = %f is inside the confidence interval.\\\\\n\n', mu));
			} else {
				cat(sprintf('The mean = %f is not inside the confidence interval.\\\\\n\n', mu));
			}
		}	
	}
}