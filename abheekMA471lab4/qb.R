rm(list = ls())
p = 0.3;
alpha = 0.05;

n = 1000;
M = c(20, 50, 100);

for (m in M) {
	covProb = 0;
	crossL = 0;
	crossU = 0;

	X = rbinom(m*n, size = 1, prob = p);
	p_mle = sum(X)/(n*m);
	# print(p_mle); print(length(X))

	for (i in 1:n) {
		Y = X[((i-1)*m + 1) : (i*m)];

		p_mean = mean(Y);
		# print((i-1)*m); print(p_mean)
		err = -qnorm(alpha/2)*sqrt(p_mean*(1-p_mean)/m);
		cil = p_mean - err;
		ciu = p_mean + err;

		if (cil < 0) {
			cil = 0;
			crossL = crossL + 1;
		}

		if (ciu > 1) {
			ciu = 1;
			crossU = crossU + 1;
		}

		if ((cil <= p_mle) && (p_mle <= ciu)) {
			covProb = covProb + 1;
		}
	}
	err = -qnorm(alpha/2)*sqrt(p_mle*(1-p_mle)/m);

	cat(sprintf("\nBernoulli sample count = %d\\\\\np_mle = %f\\\\\n", m, p_mle));
	cat(sprintf("Confidence Interval = [%f, %f]\\\\\n", p_mle-err, p_mle+err));
	cat(sprintf("Coverage Probability = %f\\\\\n", covProb/n));
	cat(sprintf("Number of instances of interval LOWER bound going outside parameter space = %d\\\\\n", crossL));
	cat(sprintf("Number of instances of interval UPPER bound going outside parameter space = %d\\\\\n", crossU));
}