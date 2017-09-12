rm(list = ls())
p = 0.3;
alpha = 0.05;

n = 1000;
M = c(20, 50, 100, 500, 1000);

Ps = seq(0.001, 1-0.001, 0.001);
z = qchisq(1-alpha, 1);

for (m in M) {
	covProb = 0;
	crossL = 0;
	crossU = 0;

	X = rbinom(m*n, size = 1, prob = p);
	p_0 = sum(X)/(n*m);
	# print(p_mle); print(length(X))

	for (i in 1:n) {
		Y = X[((i-1)*m + 1) : (i*m)];

		p_mle = mean(Y);
		LogL_mle = m*p_mle*log(p_mle) + m*(1-p_mle)*log(1-p_mle);
		LogL_Ps = m*p_mle*log(Ps) + m*(1-p_mle)*log(1-Ps);
		temp = 2*(LogL_mle - LogL_Ps) < z;

		cil = min(Ps[temp]);
		ciu = max(Ps[temp]);

		if (!is.na(cil) || !is.na(ciu)) if ((cil <= p_0) && (p_0 <= ciu)) {
			covProb = covProb + 1;
		}
	}

	LogL_0 = m*p_0*log(p_0) + m*(1-p_0)*log(1-p_0);
	LogL_Ps = m*p_0*log(Ps) + m*(1-p_0)*log(1-Ps);
	temp = 2*(LogL_0 - LogL_Ps) < z;
	cil = min(Ps[temp]);
	ciu = max(Ps[temp]);

	cat(sprintf("\nBernoulli sample count = %d\\\\\np_0 = %f\\\\\n", m, p_0));
	cat(sprintf("Using Likelihood Ratio Test Confidence Interval = [%f, %f]\\\\\n", cil, ciu));
	cat(sprintf("Coverage Probability = %f\\\\\n", covProb/n));
}
