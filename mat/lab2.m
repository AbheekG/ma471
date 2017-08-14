syms a b x e;
F(x, a, b) = 1 - exp(-(x/b)^a)

f(x, a, b) = diff(F, x)

d(x, a, b) = diff(f, b)


c(a, x) = d(x, a, x^((a+1)/a))