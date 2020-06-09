#Libraries
library(lubridate)
library(prophet)
library(ggplot2)

#Bike Sharing Data
data <- read.csv(file.choose(),header = T)
str(data)
View(data)
data$dteday <- ymd(data$dteday)

#Time Series Plot
qplot(dteday,cnt,data=data,
       main='Bike Rentals in Washington DC')

##Data has seasonality and trend

#Data
ds <- data$dteday
y <- data$cnt
#Add Temperature and Humidity as Regressor
df <- data.frame(ds,y)
df$temp <- data$temp
df$hum <- data$hum




#Forecasting Model
m <- prophet()
m <- add_country_holidays(m,country_name = 'US')
m <- add_regressor(m,'temp')
m <- add_regressor(m,'hum')
m <- fit.prophet(m,df)
 
  
#Predictions
future <- make_future_dataframe(m,periods = 10)
x <- data.frame(df$temp)
colnames(x) <- 'temp'
y <- data.frame(runif(10,0.1,0.3))
colnames(y) <- 'temp'
future$temp <- rbind(x,y)


x <- data.frame(df$hum)
colnames(x) <- 'hum'
y <- data.frame(runif(10,0.4,0.8))
colnames(y) <- 'hum'
future$hum <- rbind(x,y)


future <- as.matrix(future)
colnames(future) <- NULL
colnames(future) <- c('ds','temp','hum')
                      
future <- data.frame(future)                   
future$temp <- as.numeric(future$temp)  
future$hum <- as.numeric(future$hum)
future$ds <- ymd(future$ds)
                      
forecast <- predict(m,future)

#Plot Forecast
plot(m,forecast)
dyplot.prophet(m,forecast)

#Forecast Components
prophet_plot_components(m,forecast)

#Model Performance
pred <- forecast$yhat[1:731]
actual <- df[,2]
plot(actual,pred)
abline(lm(pred~actual),col='red')
summary(lm(pred~actual))

 
 