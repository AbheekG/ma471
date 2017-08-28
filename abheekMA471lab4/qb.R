rm(list = ls())
p = 0.3;
alpha = 0.05;

n = 1000;
M = c(20, 50, 100);

for (m in M) {
	Y = rbinom(m, size = 1, prob = p);
	X = Y[ sample(1:m, n, replace = TRUE) ];

	p_mle = sum(X)/n;
	err = -qnorm(alpha/2)*sqrt(p_mle*(1-p_mle)/n);
	cil = p_mle - err;
	ciu = p_mle + err;

	if (cil < 0) cil = 0;
	if (ciu > 1) ciu = 1;

	cat(sprintf("\nBernoulli sample count = %d\np_mle = %f\n", m, p_mle));
	cat(sprintf("Confidence interval = [%f, %f]\n", cil, ciu));

	# covProb = sum((qnorm(alpha/2) <= X) & (X <= qnorm(1 - alpha/2)));
	# cat("Coverage Probability =", covProb/n, "\n")
}