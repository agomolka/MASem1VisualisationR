rm(list=ls())
options(width=200)

library(ggplot2)
library(forecast)


mVec <- as.numeric(USAccDeaths)
x11();print(ggAcf(mVec))
x11();print(ggPacf(mVec))

mTs <- ts(mVec,freq=12)
x11(); print(autoplot(mTs))
x11();print(autoplot(decompose(mTs)))

model_1 <- ets(mTs)
x11();print(autoplot(model_1))
pred_1 <- forecast(model_1,h=12)
img_pred_1 <- autoplot(pred_1)
x11(); print(img_pred_1)

model_2 <- auto.arima(mTs)
x11();print(autoplot(model_2))
pred_2 <- forecast(model_2,h=12)
img_pred_2 <- autoplot(pred_2)
x11(); print(img_pred_2)



