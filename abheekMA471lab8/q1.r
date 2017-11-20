rm(list = ls())
d = read.table("FRWRD.txt", header=FALSE);

cvd_error1 <- function(X, Y, n, r) {
	s = 0;

	for (i in 1:n) {
		X1 = X[-i,];
		Y1 = as.matrix(Y[-i,]);
		# print(X1);
		# print(Y1);
		fn <- function(beta) {
			return (sum((Y-X %*% beta)^2));
		}
		# beta = solve(t(X1) %*% X1) %*% (t(X1) %*% Y1);
		beta = optim(runif(r), fn)$par;
		# print(beta);
		s = s + sum((Y-X %*% beta)^2);
	}
	return (s/n);
}

cvd_error2 <- function(X, Y, n, r) {
	s = 0;
	k = 3;
	m = 2*n;

	for (i in 1:m) {
		leave = -sample.int(n, k);
		X1 = X[leave,];
		Y1 = as.matrix(Y[leave,]);
		# print(X1);
		# print(Y1);
		fn <- function(beta) {
			return (sum((Y-X %*% beta)^2));
		}
		# beta = solve(t(X1) %*% X1) %*% (t(X1) %*% Y1);
		beta = optim(runif(r), fn)$par;
		# print(beta);
		s = s + sum((Y-X %*% beta)^2);
	}
	return (s/m);
}

Y = d[,1];
n = length(Y);
Y = matrix(Y, nrow = n, ncol = 1);
# print(Y);

R = c(3, 6, 8);
mincv1 = numeric();
mincv2 = numeric();

for (r in R) {
	a = numeric();
	for (i in 0:r) {
		a = c(a, (1:n)^i);
	}
	X = matrix(a, nrow = n, ncol = r+1);
	mincv1 = c(mincv1, cvd_error1(X, Y, n, r+1));
	mincv2 = c(mincv2, cvd_error2(X, Y, n, r+1));
	# print(a);
}

print('Error LOOCV then k-fold');
print(mincv1); print(mincv2);
print('degree with minimum error by LOOCV')
print(R[which.min(mincv1)]);

print('degree with minimum error by k-fold')
print(R[which.min(mincv2)]);