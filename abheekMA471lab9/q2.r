rm(list = ls())

sig = 0.025;
alp = 0.2;

# a
n = 100;
A = rnorm(n = n+1, mean = 0, sd = sig);
X = A[-1] + alp*A[-(n+1)];
plot(ts(X));
dev.copy(png, "plots/plot2_a.png"); dev.off ();

# b
acf(X);
dev.copy(png, "plots/plot2_b.png"); dev.off ();

# c
plot(0:4, c(1, 0.2/1.04, 0, 0, 0), type="h", col = "blue");
dev.copy(png, "plots/plot2_c.png"); dev.off ();