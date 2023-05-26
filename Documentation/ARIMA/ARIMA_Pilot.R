library(tidyverse) # group_by and summarise
library(forecast) # arima

# load dataset
df <- read.csv("Data/CleanData/aviationYear_ARIMA.csv")

### Pilot
# filter the distribution of women by year
yr_gender <- df %>% 
  filter(Gender == 'Women')

# convert time series 
tsdata <- ts(yr_gender$Pilot, start = 2005, frequency=1)

# check best model
auto.arima(tsdata, trace=TRUE)
# set best model for predict
model <- arima(tsdata, order = c(1,2,0))
# Comparison before and after difference
par(mfrow = c(1,2))
plot(tsdata,
     main = "Female Pilot Distribution Annual Time Series Plot", col="red")
plot(model$residuals,
     main= "Female Pilot Distribution Annual Time Series Plot", col="red")

# Verify Normal Distribution
qqnorm(model$residuals, pch=21, col="black",
       bg="gold")
qqline(model$residuals, col="royalblue", lwd=2)

# check white noise
# acf and pacf
par(mfrow=c(1,2))
acf(model$residuals, main="ACF")
pacf(model$residuals, main="PACF")
# Ljung-Box text
Box.test(model$residuals, type="Ljung-Box")

# p-value is 0.7464, indicating that there is not enough evidence to reject the null hypothesis that there is no autocorrelation.
# this means that, the residuals have independence and are consistent with the white noise process.
# If the residuals of the time series model match the white noise process, 
# the model is said to be appropriately specified and is likely to make good predictions, so we use this model.*/

# forcasting
predict <- forecast(model, h=10) # 10 years

# save plot as jpeg
jpeg(file="Documentation/ARIMA/resultPilot.jpeg", width=600, height=600)
plot(predict, col="darkgreen", lwd=2,
     flty=1, flwd=3, fcol="royalblue", 
     shadecols=c("mistyrose", "salmon"),
     xlab="Year", ylab="Flow",
     main="Women in the Aerospace over the next 10 years\nPilot")
dev.off()

# show numeric values of predict
predict


