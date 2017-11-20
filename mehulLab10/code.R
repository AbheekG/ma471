##################
# Question1
##################

gold = read.csv("Data_gold.txt", header = FALSE)
gold = gold[,"V2"];

#1a
gold_ts = ts(gold)
plot.ts(gold_ts, ylab="Gold Prices")

#1b
gold_lret = log(gold[-1]) - log(gold[-length(gold)])
gold_lret_ts = ts(gold_lret)
plot.ts(gold_lret_ts, ylab="Log returns of Gold")

#1c
acf(gold_lret, na.action = na.pass)
pacf(gold_lret, na.action = na.pass)

#1d
ar_gold_lret = ar(unlist(gold_lret), aic = TRUE, na.action = na.exclude)

#1e
pred1 = predict(ar_gold_lret, newdata = unlist(gold_lret), n.ahead = 1)
pred3 = predict(ar_gold_lret, newdata = unlist(gold_lret), n.ahead = 3)
plot(pred3$se, ylab = "log returns", main = "3-step Prediction of Log returns");

##############
# Question 2
##############

#2a
a_t = rnorm(n = 101, mean = 0, sd = sqrt(0.025))
r_t = a_t[-1] + 0.2*a_t[-101];

#2b
acf(r_t)

#2c
plot(x = c(0,1,2,3,4), y=c(1,0.2/1.04, 0, 0, 0), type="h", col = "blue")

###################
# Question 3
###################

#3a
deciles = read.table("deciles08.txt")
acf(deciles[,"V3"], main = "decile 2 ACF")
acf(deciles[,"V5"], main = "decile 10 ACF")

#3b
deciles2_arma = arima(deciles[,"V3"], order = c(1,1,1))

#3c
arma_forecast = forecast(deciles2_arma)
plot(arma_forecast$mean, ylab = "Forecasted values")

