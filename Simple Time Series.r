if(!require(pacman)){install.packages("pacman")}
pacman::p_load("lubridate","tseries","tidyverse","forecast","prophet","TSA")

# astsa has the Johnson & Johnson dataset
# library(astsa)

set.seed(123)
nifty_data <- read_csv("../Time Series//nifty50.csv") %>% 
    sample_n(1000) %>% 
    mutate(rtrn = close -  open,
           date = ymd(date)) %>% 
    arrange(date)

dim(nifty_data)
sum(nifty_data$close)
head(nifty_data)

nifty_data %>% 
ggplot(aes(date,close))+
geom_line() +
labs(title = "Non-stationary dataset",
    subtitle = "Changes with time")

nifty_data %>% 
ggplot(aes(date,rtrn))+
geom_point() +
labs(title = "Stationary dataset",
    subtitle = "Does not change with time")

adf.test(x = nifty_data$rtrn)

acf(nifty_data$rtrn)

pacf(nifty_data$rtrn)

arima(nifty_data$rtrn, order = c(1,0,0))


#order = c(p,d,q)

#d = differencing, is applicable if you're converting from non-stationary to stationary
#q = Moving Average is applicable if there is an element of auto-correlation
#p = Auto Regression, this will be important.

arima(x = nifty_data$rtrn, order = c(2,0,0))

arima(x = nifty_data$rtrn, order = c(1,0,1))

arima.final <- arima(x = nifty_data$rtrn, order = c(1,0,0))

tsdiag(object = arima.final)

# pacf(arima.final$residuals) will show the PACF diagnosis plot

# Standardised residuals should not have a pattern and should be around zero
# ACF should be within the blue lines 
# P-values should be >0.05. Ljung-Box test null hypothesis is that it is stationary data

predict(object = arima.final, n.ahead = 10)

data.frame(predict(object = arima.final, n.ahead = 10)$pred) %>% 
    setNames("pred") %>%
    mutate(nxt = c(pred[2:10],NA),
           sm = pred==nxt,
           seq = row_number()) %>% 
    relocate(seq, .before = everything())

adf.test(nifty_data$close)

d.close <- diff(nifty_data$close)

head(d.close)

adf.test(d.close)

pacf(d.close)

arima(x = d.close, order = c(1,0,0))
arima(x = d.close, order = c(2,0,0)) # Model decreasing with additional AR terms
arima(x = d.close, order = c(1,0,1)) # Model decreasing with additional MA terms
arima(x = d.close, order = c(1,1,0)) # Model decreasing with additional differencing
arima(x = nifty_data$close, order = c(1,2,0)) # Same as the model above

arima.final.st <- arima(d.close, c(1,0,0))
tsdiag(arima.final.st)

#data()  #datasets in all packages

astsa::jj

plot(jj, main = "Quarterly earnings of Johnson & Johnson shared", ylab = "Earnings", xlab = "Year")

trans.jj <- diff(log(jj))

plot(trans.jj)
acf(trans.jj)
pacf(trans.jj)

trans.seas.jj <- diff(diff(log(jj)),4)

# 4 represents the lag since this is seasonal data. It represents the number of seasons in a year
# The first differencing is for removing the trend
# The second differencing is for removing the seasonal trend

trans.seas.jj

Box.test(x = trans.seas.jj, lag = log(length(trans.seas.jj)))

# log is the typical way of choosing a lag

length(trans.seas.jj)
# Length is the number of observations

log(79)

pacf(trans.seas.jj)

acf(trans.seas.jj)

arima(x = log(jj), order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 4))
arima(x = log(jj), order = c(0,1,0), seasonal = list(order = c(0,1,1), period = 4))
arima(x = log(jj), order = c(0,1,0), seasonal = list(order = c(1,1,0), period = 4))
arima(x = log(jj), order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 4))

# etc, etc

# This will be the model with the lowest AIC

seas.arima.final <- arima(x = log(jj), order = c(0,1,1), seasonal = list(order = c(1,1,0), period = 4))

seas.arima.final

tsdiag(seas.arima.final)

sarima(xdata = log(jj), 0,1,1,1,1,0,4)

predict(object = seas.arima.final, n.ahead = 20)

2.910254-0.08895758
2.910254+0.08895758

forecast(object = seas.arima.final, h = 20)

plot(forecast(object = seas.arima.final, h = 20))

# Best ARIMA model was c(1,0,0)

auto.arima(y = nifty_data$rtrn, seasonal = FALSE)

# Best ARIMA model was c(0,1,0)

auto.arima(y = nifty_data$close, seasonal = FALSE)

# Best ARIMA model was (0,1,1)(1,1,0)[4]

seas.arima <- auto.arima(y = jj, seasonal = TRUE)

seas.arima

plot(forecast(object = seas.arima, h = 20))

tsdiag(seas.arima)

pacf(seas.arima$residuals)

qqnorm(seas.arima$residuals)

?qqnorm

summary(seas.arima)

accuracy(seas.arima)

# accuracy = 100 - MAPE (Mean Absolute Percentage Error)

icecream <- read_csv("../Time Series//Icecream.csv")

head(icecream)

plot(icecream$cons, type = "b", lty = 2)

arima.ice <- auto.arima(y = icecream$cons)

arima.ice

forecast(arima.ice, h = 6)

plot(forecast(arima.ice, h = 6))

accuracy(arima.ice)

arimax.ice <- auto.arima(y = icecream$cons, xreg = icecream$temp)

arimax.ice

temp_model <- auto.arima(y = icecream$temp, seasonal = TRUE)

temp_model

plot(forecast(temp_model, 6))

fore_temp <- forecast(object = temp_model, h = 6)

fore_temp

plot(forecast(object = arimax.ice, h = 6, xreg = fore_temp$mean))

forecast(object = arimax.ice, xreg = fore_temp$mean, h = 6)

lmtest::coeftest(x = arimax.ice)

lag(x = icecream$income, n = 2)

vars_matrix <- cbind(icecream$temp,
     icecream$income,
     lag(x = icecream$income, n = 1),
     lag(x = icecream$income, n = 2))

head(vars_matrix)

aic_vals <- c(temp_inc = auto.arima(y = icecream$cons, xreg = vars_matrix[,1:2])$aic,
              temp_inc_L1_L2 = auto.arima(y = icecream$cons, xreg = vars_matrix[,1:3])$aic,
              all_vars = auto.arima(y = icecream$cons, xreg = vars_matrix)$aic,
              temp_inc_L1 = auto.arima(y = icecream$cons, xreg = vars_matrix[,c(1,3)])$aic,
              temp_inc_L2 = auto.arima(y = icecream$cons, xreg = vars_matrix[,c(1,4)])$aic,
              temp = arimax.ice$aic,
              simple_arima = arima.ice$aic)

aic_vals

sort(aic_vals)

best.arimax <- auto.arima(y = icecream$cons, xreg = vars_matrix[,c(1,4)])

tail(icecream$income)

fore_income <- c(tail(icecream$income,2), 93, 96, 96, 96) # Lagged income by 2 periods

fore_matrix <- cbind(fore_temp$mean,fore_income)

fore_matrix

class(fore_matrix)

forecast(object = best.arimax, h = 6, xreg = fore_matrix)

plot(forecast(object = best.arimax, h = 6, xreg = fore_matrix))

results <- rbind(accuracy(arima.ice),
     accuracy(arimax.ice),
     accuracy(best.arimax)) %>%  
as.data.frame(.)

rownames(results) <- c("arima","arimax.ice","best.arimax")

results %>% arrange(MAPE)

sort(aic_vals)

# Randomly generated data with a seasonal element and two explanatory variables

ran_data <- data.frame(y = ts(rnorm(120,0,3) + 1:120 + 20*sin(2*pi*(1:120)/12), frequency=12),
                       x1 = rnorm(length(y), 20, 30),
                       x2 = rnorm(length(y), 5, 30))

dim(ran_data)
head(ran_data)

par(mfrow = c(2,2))
plot(ran_data$y)
plot(ran_data$x1)
plot(ran_data$x2)

auto.arima(ran_data$y)

seas_mat <- matrix(ran_data$x1, ran_data$x2, ncol = 2, nrow = nrow(ran_data))

auto.arima(y = ran_data$y, xreg = seas_mat)

tom_brady <- wikipediatrend::wp_trend(page = "Tom_Brady", from = "2013-01-01", to = "2015-12-31")

head(tom_brady)

qplot(x = date, y = views, data = tom_brady)
summary(tom_brady$views)

tom_brady <- tom_brady %>% 
    mutate(views = na_if(x = views, y = 0),
           log_views = log(views))

head(tom_brady)

qplot(x = date, y = log_views, data = tom_brady)

prophet_model <- prophet(df = tom_brady %>% select(ds = date,y = log_views))

prophet_future <-  make_future_dataframe(m = prophet_model, periods = 365)

tail(prophet_future)

prophet_forecast <- predict(object = prophet_model, prophet_future)

tail(prophet_forecast)

prophet_forecast %>% 
select(ds,yhat,yhat_lower,yhat_upper) %>% 
mutate(view = exp(yhat)) %>% 
tail()

plot(prophet_model, prophet_forecast)

prophet_plot_components(m = prophet_model, fcst = prophet_forecast)

df.cv <- cross_validation(model = prophet_model, horizon = 160, units = "days")

tail(df.cv)

prophet_perf <- performance_metrics(df = df.cv)

dim(prophet_perf)
head(prophet_perf)

prophet_perf %>% 
summarise_at(.vars = c(2:6), .funs = mean)

AirPassengers

par(mfrow = c(2,2))
plot(decompose(AirPassengers)$x, type="l", lty=1, main = "Original data")
plot(decompose(AirPassengers)$figure, type="l", lty=1, main = "Seasonal index")
plot(decompose(AirPassengers)$trend, type="l", lty=1, main = "Trend")

autoplot(stl(x = AirPassengers, s.window = "periodic", robust = TRUE))

frequency(x = AirPassengers)

stl(x = AirPassengers, s.window = "periodic", robust = TRUE)[]

freq <- frequency(AirPassengers)
time_p <- stl(x = AirPassengers, s.window = "periodic", robust = TRUE)$time.series[1:freq,1]
peak_s <- which.max(time_p)

plot(x = time_p, 
     type = "b", 
     lty = 1,
     main = "Seasonality Index",
     ylab = "Seasonality Index",
     xlab = "Time period")
abline(v = peak_s, lty = 2, col = "red")

plot(nifty_data$close, type = "l")

ets(y = nifty_data$close, model = "ZZZ")

plot(forecast(ets(y = nifty_data$close), h = 100))

AirPassengers

TSA::periodogram(y = AirPassengers)

periodogram <- data.frame(
TSA::periodogram(y = AirPassengers, plot = F)$freq,
TSA::periodogram(y = AirPassengers, plot = F)$spec) %>% 
setNames(c("freq","spec")) %>% 
arrange(desc(spec)) %>% 
mutate(cycle.months = 1/freq/12) # Each entry is a month, each row is a year

head(periodogram)

periodogram %>%
head(10) %>% 
mutate(row_num = row_number()) %>% 
ggplot(aes(x = row_num, y = spec))+
geom_line()+
scale_y_continuous(labels = scales::comma_format())


