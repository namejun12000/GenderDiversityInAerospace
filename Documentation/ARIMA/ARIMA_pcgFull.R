### I think we can not use this

library(tidyverse) # group_by and summarise
library(forecast)

# load dataset
pcgFull <- read.csv("Data/CleanData/pcgFull.csv")

# filter the distribution of gender in stem job by year 
yr_gender <- pcgFull %>% 
  group_by(Year, GENDER) %>% 
  filter(StemJob == "Stem" & GENDER == 'F') %>% 
  tally()

# convert time series
tsdata <- ts(yr_gender$n, frequency = 1)

# fit the model
arima <- auto.arima(tsdata, trace=TRUE)
model <- arima(tsdata, order=c(0,0,0))

# Time series not normalized
acf(model$residuals)
pacf(model$residuals)

forecast <- forecast(model, h=10)

# save plot as jpeg
jpeg(file="Documentation/ARIMA/resultPcgFull.jpeg", width=600, height=600)
plot(forecast, col="darkgreen", lwd=2,
     flty=1, flwd=3, fcol="royalblue", 
     shadecols=c("mistyrose", "salmon"),
     xlab="", ylab="Flow",
     main="Women in the STEM over the next 10 years ")
dev.off()

# show numeric values of predict
predict



