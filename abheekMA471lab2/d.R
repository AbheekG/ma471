rm(list=ls())

shape = 2
scale = 1
count = c(20, 40, 100, 200)


for (k in count) {
	d = rweibull(k, shape = shape, scale = scale)
	# print(sprintf("For %d samples, maximum = %f", k, max(d)))
}