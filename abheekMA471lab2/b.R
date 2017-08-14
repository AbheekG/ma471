rm(list=ls())
d = read.table("d-csp0108.txt", header=TRUE)

names = c('C', 'SP')

for (k in 2:3) {
	p = seq(0, 1, 0.01)
	Q_data = quantile(d[,k], probs = p)
	Q_norm = quantile(rnorm(length(d[,k]), mean = mean(d[,k]), sd = sd(d[,k])), probs = p)
	# Q_norm = qnorm(p, mean = mean(d[,k]), sd = sd(d[,k]))
	
	refLine = seq(min(Q_data), max(Q_data), 0.01)
	plot(refLine, refLine, main = sprintf("QQ-plot of %s and Normal", names[k-1]), 
		xlab = "Normal Quantile", ylab = sprintf("%s Quantile", names[k-1]), col = 'black', type = 'l')
	lines(Q_norm, Q_data, col = 'red')
	legend("bottomright", legend = c('reference line', 'QQ-plot'), fill = c('black', 'red'))
	
	dev.copy(png, sprintf("plots/plotb%d.png", k-1))
	dev.off ()
}