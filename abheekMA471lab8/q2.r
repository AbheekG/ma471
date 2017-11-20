rm(list = ls())

r = 7;
n = 16;

Y = as.matrix(longley[1:n, r]);
X = as.matrix(longley[1:n, 1:(r-1)]);

X = cbind(rep(1, n), X);

beta = solve(t(X) %*% X) %*% (t(X) %*% Y);
print('Best fit');
print(beta[,1]);

# Outliers
xm = colSums(X)/n;

print('Most Outlier');
print(X[which.max(colSums((t(X) - xm)^2) + (Y - X %*% beta)^2), ])

# Leverage point
print('Most Influential Point');
print(X[which.max((Y - X %*% beta)^2), ])


print('Most Leverage point');
print(X[which.max(colSums((t(X) - xm)^2)), ])
