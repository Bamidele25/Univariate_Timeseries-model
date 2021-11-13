#import data set from excel to R 
fol<-read.csv("C:\\Users\\LENOVO PC\\Documents\\folo2_data.csv")
f.ts <- ts (fol, frequency = 4,start= c(1981,1))
f.ts
head(fol)
#Time series plot of the oil prices at level
plot.ts(f.ts, pch= 16, main = "Oil Price Series ", xlab = "Year", ylab = "Oil price", type = "o", col ="red")
#Autocorrelation Function and PACF for the series at level
acf(f.ts, lag.max = 25,type = "correlation", main = "ACF of Oil prices")
acf(f.ts, lag.max = 25,type = "partial", main = "PACF of Oil prices")
dfst = diff(f.ts)
ldfst = log(dfst)
#Time series plot of the oil prices at 1st diff
plot.ts(dfst, pch= 16, main = "Oil Price Series ", xlab = "Year", ylab = "Oil price", type = "o", col ="red")
#Autocorrelation Function and PACF for the series at 1st diff
acf(dfst, lag.max = 15,type = "correlation", main = "ACF of GDP")
acf(dfst, lag.max = 25,type = "partial", main = "PACF of GDP")
#Stationary Test at level
adf.test(f.ts, k = 1)
pp.test(f.ts,lshort=TRUE)
#Stationary Test at 1st Diff
adf.test(dfst, k = 1)
pp.test(dfst,lshort=TRUE)
#ARIMA model fitting 
fit1<-auto.arima (f.ts, trace =TRUE, test="kpss", ic = "aic")
fit1
md<-arima(f.ts, order = c(1,1,1))
md
md2 <- arima(f.ts, order = c(1,1,0))
md2
md3<- arima(f.ts, order = c(0,1,1))
md3
#diagonastics test 
Box.test(md$residuals,lag = 10, type = "Ljung-Box")
Box.test(md$residuals^2,lag = 10, type = "Ljung-Box")
#ACF and PACF of the Diagonastic model 
acf(md$residuals, lag.max = 25,type = "correlation",main = "ACF of\nARIMA (1,1,1) Residual")
acf(md$residuals, lag.max = 25,type = "partial",main = "PACF of\nARIMA (1,1,1) Residual")
#sarima model for the data sets 
sma1<- arima (f.ts, order = c(1,0,0), seasonal = list (order=c(0,1,1), period = 4), method = "ML")
sma2<- arima(f.ts, order=c(1,1,0),seasonal = list(order= c(1,1,1),period = 4), method = "ML")
sma3<- arima(f.ts, order=c(1,1,1),seasonal = list(order= c(1,1,0),period = 4), method = "ML")
sma4<- arima(f.ts, order=c(2,1,1),seasonal = list(order= c(0,1,0),period = 4), method = "ML")
sma5<- arima(f.ts, order=c(3,1,1),seasonal = list(order= c(1,1,1),period = 4), method = "ML")
sma1
sma2
sma3
sma4
sma5
acf(sma5$residuals, lag.max = 25,type = "correlation",main = "ACF of\nSARIMA (1,1,1)x(1,1,0)[4] Residual")
acf(sma5$residuals, lag.max = 25,type = "partial",main = "PACF of\nSARIMA (1,1,1)x(1,1,0)[4] Residual")
#GARCH effect model
x = ugarchspec(variance.model = list(garchOrder = c(0,1)),mean.model = list(armaOrder = c(1,1)))
bfit = ugarchfit(x,data = f.ts)
bfit
?rugarch
#diagonastic check up for the GARCH model selected to be best 
re_d = bfit@fit$residuals
plot.ts(re_d, type = "l")
acf(re_d, lag.max = 25, type = "correlation", main = "ACF of \nARCH(1) Residual")
acf(re_d^2, lag.max = 25, type = "correlation", main = "ACF of \nGARCH(1,1) Residual")
acf(re_d, lag.max = 25, type = "partial", main = "PACF of \nARCH(1) Residual")
Box.test(re_d, lag = 10, type = "Ljung-Box")
Box.test(re_d^2, lag = 10, type = "Ljung-Box")
jarque.bera.test(re_d)
