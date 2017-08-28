rm(list = ls())
d = read.table("d-csp0108.txt", header=TRUE)

# Calculating log returns
for (k in 2:3) {
	d[,k] = log(1 + d[,k]);
	# print(mean(d[,k]));
}
