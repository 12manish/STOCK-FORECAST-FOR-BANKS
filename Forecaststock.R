###########>>>>Time series<<<<<<<<<<<########
#install.packages('forecast', dependencies = TRUE)
library(forecast)
library(forecast)
library(tseries)
library(fpp)
library(lubridate)
library(psych)
library(lattice)
library(car)
library(corrgram)
library(dplyr)
library(rugarch)
#if(!require(tsfknn)) install.packages("tsfknn")

setwd("C:\\Users\\SPEED TAIL\\Desktop\\Time series analysis")
bank <- read_excel("AllData_BANKS.xlsx")
describe(bank)
histogram(~bank$OPEN) 
histogram(~bank$CLOSE) 
df <- data.frame(bank)
df$Day<- as.factor(df$Day)

df$Day<-strptime(df$Day,format="%Y-%m-%d") #defining what is the original format of your date
df$Day<-as.Date(df$Day,format="%Y-%m-%d") #defining what is the desired format of your date
table(df$Company)
hist(df$HIGH, breaks = 60, col = "orange",main = "stock high")
hist(df$CLOSE, main = "Closing Prices of Banks",breaks = 60, col = "cyan")

df$Year<-as.Date(df$Year,format="%Y")
summary(df)
str(df)



##T TEST
t.test(bank$OPEN,bank$CLOSE)

df<-select(df,(4))
#Visualizing Stock Data

summary(df)

dim(df)
sum(is.na(df))

class(df)

# Convert the data frame into a time series object, using ts() function.
# ts() function belongs to forecast package

?ts()
ml<-ts(df,start=c(2014,10),end=c(2019,10),frequency=12)  ##months wise
class(ml)
ml
start(ml)
end(ml)
frequency(ml)
cycle(ml)
plot(ml)
plot.ts(ml)


#Data has both trend and seasonality

#Decomposing using MA
dec<-decompose(ml,type="additive")
str(dec)
dec$x #original series
dec$seasonal #seasonal indices (guessed based on frequency of ts() object)
dec$trend #trend component
dec$random #random component
dec$figure #seasonal components
dec$type # type of seasonality
plot(dec)

decM<-decompose(ml,type="multiplicative")
decM$x #original series
decM$seasonal #seasonal indices (guessed based on frequency of ts() object)
decM$trend #trend component
decM$random #random component
decM$figure #seasonal components
decM$type # type of seasonality
plot(decM)


dec$x-dec$seasonal #deseasonalise
decM$x/decM$seasonal

#Forecasting
#ses  
#holt's model
#Arima models


# Explore the time series

# Aggregating the time series

# Yearly totals

?aggregate
aggregate(ml)
plot(aggregate(ml))

# Increasing trend

?aggregate()
aggregate(ml, FUN = mean)
plot(aggregate(ml, FUN = mean))

# Boxplot creates one boxplor for each month
?boxplot
b1<-boxplot(ml~cycle(ml))
b1


#Subsetting the time series using window function
# We shall create two windows (2014 to 2018) and (2018 to 2019)
# Creating Training and Testing Window datasets
# Frequency is the number of time periods in a year

mlTr<-window(ml,start=c(2014,10),end=c(2018,10),frequency=12)

mlT<-window(ml,start=c(2018,10),end=c(2019,10),frequency=12)

plot(mlTr)
plot(mlT)

# We shall create a time series model on (2014 to 2017)
# and forecast for the period (2017 to 2019)
# We shall then compare the forecasts with the actual time series data

#Data has both trend and seasonality

#Decomposing the Time Series into seasonal, trend and irregular
# components with additive seasonality
?decompose
dec<-decompose(mlTr)
plot(dec)

# Top panel contains the original time series
# Second panel contains the trend
# Third panel contains the seasonality component
# Last panel contains random fluctuations

#exponential smoothing on training window dataset

?ses

# To forecast share for the next 24 months

es<-ses(mlTr,h=24)
plot(es)
summary(es)

es$x

es$fitted

es$residuals


100*es$residuals/es$x # PE

mean(abs(100*es$residuals/es$x))

# Finding the accuracy of exponential smoothing model

accuracy(es,mlT)

# Checking residuals

checkresiduals(es)
# There is a pattern in the resuduals. Therefore this forecast is 
# not sufficiently an accurate forecast.
# Hence, we use the Holt's method.

#Fitting holts's model
hol<-holt(mlTr,h=24) #forecasting for two years periods
plot(hol)
summary(hol)
accuracy(hol,mlT)
?checkresiduals
checkresiduals(hol)
?Box.test
Box.test(hol$residuals, lag = 20, type = "Ljung-Bo")

# Blue line is slightly elevated, meaning that this forecast 
# is betterthan the previous one.
# The Ljung-Box test reveals that p-value is low.
# Ho: Data is identically and independently distributed (white noise)
# Ha:  Data is not identically and independently 
# distributed (exhibits serial correlation)
# Therefore we reject Ho and conclude that data exhibits serial correlation.

#Clearly the series is seasonal so seasonal component is required

# Finally trying the holt winter's method 

### Holt Winter's Model for Trend & Seasonality 

?hw

hwTS<-hw(mlTr,h=24,seasonal = "additive") ##for two years
summary(hwTS)

plot(hwTS)

accuracy(hwTS,mlT)


checkresiduals(hwTS)

# p-value is high, indicating that data is identically and 
#independently distributed 

#	All spikes are below blue line.
# There's no pattern in the residuals
# Histogram is also closer to bell-shaped

#Automating model building using ets()
?ets()
auto<-ets(mlTr)
summary(auto)


# The MAPE is less than 7 and it seems like a good MAPE.

foc<-forecast(auto,h=24)      ##for two years
summary(foc)
plot(foc)

checkresiduals(foc)


# ARIMA

# Integration Part

plot.ts(mlTr)

# Stationarizing the series by differencing it

mlTrdiff1 <- diff(mlTr, differences = 1)
mlTrdiff1

# Checking for stationarity

plot.ts(mlTrdiff1)

# statistical way of checking stationarity

?adf.test
adf.test(mlTrdiff1)


mlTrdiff2 <- diff(mlTr, differences = 2)
mlTrdiff2
plot(mlTrdiff2)
adf.test(mlTrdiff2)

# Optimal d is 1


# Sometimes we might require taking the log of the time series 

# We can use auto.arima() to find optimal values of p.d and q.

auto.arima(mlTr)

# Running the final ARIMA model
?arima
mlTrarima <- arima(mlTr, c(0,1,0), seasonal = list(order = c(0,1,1), period =12))
mlTrarima
mlTrarimaF <- forecast(mlTrarima, h = 24)# forecast for 24 months

plot(mlTrarimaF)
# Validating the model
checkresiduals(mlTrarimaF)
summary(mlTrarimaF)
#Dataset  STOCK forecasting for the next 2 years
pSHARE_forecast <- forecast(mlTrarima, h=24)
pSHARE_forecast
plot(pSHARE_forecast)
#As we can see, we have a blue line that represents the mean of our prediction:
#Dataset forecast mean first 5 values
head(pSHARE_forecast$mean)
#Dataset forecast lower first 5 values
head(pSHARE_forecast$lower)
#Dataset forecast upper first 5 values
head(pSHARE_forecast$upper)

trainarimafit <- auto.arima(mlTr, lambda = "auto")

predlen=length(bank.test)
trainarimafit <- forecast(trainarimafit, h=predlen)
trainarimafit
#Once we have our prediction applied over the train set we plot the mean tendency
#of our forecasting over the test set close price move.
#Plotting mean predicted values vs real data
meanvalues <- as.vector(trainarimafit$mean)


################################



#Clearly the series is seasonal so seasonal component is required
hw<-hw(mlTr,h=24,seasonal = "additive")
accuracy(hw,x =window(ml,start=c(2014,10),end=c(2019,10)))
plot(hw)

summary(es)

#Automating model building using ets()
auto<-ets(mlTr)
summary(auto)

foc<-forecast(auto,h=24)
foc
accuracy(foc,x =window(ml,start=c(2014,10),end=c(2019,10)))
Box.test(auto$residuals)
checkresiduals(foc)
plot(foc)


###Garch modelk

#Once we have our package loaded we proceed to apply the model previously defined to the close price dataset. 
#For finding ARFIMA parameters we run auto arfima function.
##Dataset forecast upper first 5 values
fitarfima = autoarfima(data = mlTr, ar.max = 2, ma.max = 2, 
                       criterion = "AIC", method = "full")
#With the parameters collected we choose ARFIMA (1,0,2) and incorporate the parameters to a garch model.
#define the model
garch11closeprice=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)))
#estimate model 
garch11closepricefit=ugarchfit(spec=garch11closeprice, data=mlTr)
#Having our model fitted, we can make a volatility plot.
#conditional volatility plot
plot.ts(sigma(garch11closepricefit), ylab="sigma(t)", col="blue")
#Model akike
infocriteria(garch11closepricefit)
##With this information now we proceed to plot the residuals. 
#We first plot the normal residuals:
#Normal residuals
garchres <- data.frame(residuals(garch11closepricefit))  
plot(garchres$residuals.garch11closepricefit.)
#Now we proceed to calculate and plot the standardized residuals.
#Standardized residuals
garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE)) 
#Normal Q plot
qqnorm(garchres$residuals.garch11closepricefit..standardize...TRUE.)
qqline(garchres$residuals.garch11closepricefit..standardize...TRUE.)
#Squared standardized residuals Ljung Box
garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE)^2) 
Box.test(garchres$residuals.garch11closepricefit..standardize...TRUE..2, type="Ljung-Box")
##With Ljung Box test we can see that our standardized squared residuals does not reject 
#the null hypothesis, confirming that we are not having autocorrelation between them.
#As we explained before with our model volatility, we can see that our model residuals 
#are bigger in the last years data. This can be caused by higher data volatility
#in 2018 and 2019. As we found our volatility and residuals behavior we can proceed 
#forecasting our next 365 days and compare to the other models.
#GARCH Forecasting
garchforecast <- ugarchforecast(garch11closepricefit, n.ahead = 30 )
garchforecast
#With the model fitted and the values forecasted we plot our data prediction.
plot(garchforecast)




