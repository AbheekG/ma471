rm(list=ls())
d = read.table("d-csp0108.txt", header=TRUE)

names = c('C', 'SP')

for (k in 2:3) {
	mean_d = mean(d[,k])
	sd_d = sd(d[,k])
	kurtosis_d = mean( (d[,k] - mean_d)^4 / sd_d^4 )
	print(sprintf("Kurtosis of %s = %f", names[k-1], kurtosis_d))
}
