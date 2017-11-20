rm(list = ls())

d = read.csv('Data_gold.txt', header = FALSE);
d = d[, 'V4'];
# print(d)

# a
d = ts(d);
plot.ts(d, ylab = 'Gold Price', main = 'Gold price vs time');
dev.copy(png, "plots/plot1_a.png")
dev.off ()

# b
X = log(d[-1]) - log(d[-length(d)]);
X_ts = ts(X);
plot.ts(X_ts, ylab="Log returns of Gold", main = "Log returns of Gold vs Time");
dev.copy(png, "plots/plot1_b.png");
dev.off ();

# c
acf(X);
dev.copy(png, "plots/plot1_c.png"); dev.off ();
pacf(X)
dev.copy(png, "plots/plot1_d.png"); dev.off ();

# d
X_ar = ar(X);
print(X_ar);

# e
pred = predict(X_ar, n.ahead = 3);
print(pred);
plot(pred$pred, ylab = "Log returns", main = "3-step Prediction of Log returns");
dev.copy(png, "plots/plot1_e.png"); dev.off ();