n = 200

la = 0.4
mu1 = mu2 = 0
sig1 = 1
sig2 = 5

temp_U = runif(n)
temp_X1 = rnorm(n, mean = mu1, sd= sig1)
temp_X2 = rnorm(n, mean = mu2, sd= sig2)

X = temp_X1*(temp_U <= 0.4) + temp_X2*(temp_U > 0.4)