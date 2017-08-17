syms a b x n;
F(x, a, b) = 1 - exp(-(x/b)^a)

f(x, a, b) = diff(F, x)

L(x, a, b) = log(f(x, a, b)^n)

db(x, a, b) = diff(L, b)

da(x, a, b) = diff(L, a)
