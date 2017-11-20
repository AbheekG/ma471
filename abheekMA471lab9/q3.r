rm(list = ls())

d = read.table('deciles08.txt');
# print(d)
d2 = d[,3];
d10 = d[,5];

# a
acf(d2, main = "Decile 2 ACF vs Lag");
dev.copy(png, "plots/plot3_a.png"); dev.off ();
acf(d10, main = "Decile 10 ACF vs Lag");
dev.copy(png, "plots/plot3_b.png"); dev.off ();

# b
d2_arma = arima(d2, order = c(5, 1, 5));
print(d2_arma)

#3c
arma_forecast = predict(d2_arma, n.ahead = 12);
print(arma_forecast)
plot(arma_forecast$pred, ylab = "Forecasted values")
dev.copy(png, "plots/plot3_c.png"); dev.off ();