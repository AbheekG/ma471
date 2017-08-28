rm(list = ls())
d = read.table("d-csp0108.txt", header=TRUE)

# Calculating log returns
for (k in 2:3) {
	d[,k] = log(1 + d[,k]);
	cat("Mean =", mean(d[,k]), "\n");
}

cat("Yes, mean different from 0.\n")

alpha = 0.05;
n = length(d[,1]);

for (k in 2:3) {
	mu = mean(d[,k]);
	sig = sd(d[,k]);

	# X = (d[,k] - mu)/sig;
	X = d[,k];
	# covProb = sum((qnorm(alpha/2) <= X) & (X <= qnorm(1 - alpha/2)));
	covProb = sum( ((X + sig*qnorm(alpha/2))/(1+sig) <= mu) & (mu <= (X + sig*qnorm(1 - alpha/2))/(1+sig)) );
	cat("Coverage Probability =", covProb/n, "\n")
}