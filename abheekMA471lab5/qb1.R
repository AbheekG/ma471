rm(list = ls())
d = read.table("d-csp0108.txt", header=TRUE)
names = c('C', 'SP')
# Calculating log returns
for (k in 2:3) {
	d[,k] = log(1 + d[,k]);
}