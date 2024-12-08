library(ggfortify)
library(FitAR)
library(forecast)
library(tseries)
library(Metrics)
library(lmtest)
library(fpp2)
library(readr)
Kurs_Transaksi <- read_csv("~/My Folder/Tugas Akhir/Sidang Skripsi/Data Bersih/Kurs Transaksi.csv")

usd <- Kurs_Transaksi$USD
len_usd <- length(usd)
len_usd_train <- as.integer(len_usd*0.85)

usd_train <- head(usd,len_usd_train)

lambda <- BoxCox.lambda(usd_train)
lambda

adf.test(BoxCox(usd_train, lambda))
adf.test(diff(BoxCox(usd_train, lambda),1))
usd_diff <- diff(BoxCox(usd_train, lambda),1)

Acf(usd_diff)
Pacf(usd_diff)

best_model <- auto.arima(BoxCox(usd_train, lambda), max.p=5, max.q=5,
                         stepwise=FALSE, approximation=FALSE)
summary(best_model)

coeftest(best_model)

upper_quantile_loss <- c()
lower_quantile_loss <- c()

mape <- c()
mse <- c()


for (i in 1:799) {
  print(i)
  
  rho_upper <- 0.975
  rho_lower <- 0.025
  
  y_true <- head(tail(usd, i+30-1),30)
  
  sum_y_true <- sum(y_true)
  
  x <- head(usd, len_usd-i-30+1)

  model <- arima(BoxCox(x, lambda), order=c(5L, 1L, 0L))
  
  fc <- forecast(model, h=30, level=95)
  
  y_upper <- InvBoxCox(fc$upper[1:30,1], lambda)
  
  y_lower <- InvBoxCox(fc$lower[1:30,1], lambda)
  
  y_median <- InvBoxCox(fc$mean[1:30], lambda)
  
  sum_y_upper <- sum(y_upper)
                  
  sum_y_lower <- sum(y_lower)   
  
  upper_quantile_loss[i] <- 2*(sum_y_true-sum_y_upper)*(rho_upper*(sum_y_true>sum_y_upper)-(1-rho_upper)*(sum_y_true<sum_y_upper))
  lower_quantile_loss[i] <- 2*(sum_y_true-sum_y_lower)*(rho_lower*(sum_y_true>sum_y_lower)-(1-rho_lower)*(sum_y_true<sum_y_lower))
  
  mape[i] <- mean(abs((y_median-y_true)/y_true))
  mse[i] <- mean((y_median-y_true)^2)
  
  
}

mean(mape)*100

sqrt(mean(mse))

mean(upper_quantile_loss)

mean(lower_quantile_loss)


