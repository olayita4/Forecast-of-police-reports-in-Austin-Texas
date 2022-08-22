# Packages
library(data.table)
library(lubridate)
library(dplyr) 
library(astsa)

# Load data (2,430,435 obs. of 27 variables) and calculate the monthly police reports in Austin from January 2003 to March 2022 

rm(list=ls())
data<-as.data.frame(fread('C:\\Users\\oscar\\Desktop\\Time series\\Time Series proyect\\Crime_Reports.tsv'))
colnames(data)[6]="date"
data$date <- as.Date(data$date, format="%m/%d/%Y")

data_new <- data                                   
data_new$year_month <- floor_date(data_new$date,  
                                  "month")

data_new$Number_of_police_reports = rep(1,2430435)

data1 <- data_new %>%                         
  group_by(year_month) %>% 
  dplyr::summarize(Number_of_police_reports = sum(Number_of_police_reports)) %>% 
  as.data.frame()

time_series = data1[1:231,]

#Plot of the time series (Monthly Police Reports in Austin - from January 2003 to March 2022) and analysis of cycle and trend
#Kernel smoothing was performed on the original time series to investigate the cyclic behavior and trend. The plot shows a normal kernel smoother with a bandwidth of 5 (blue line) and 30 (green line). 
#As shown in the plot, there is a cycle every 12 months (See blue line) and the number of monthly reports increase constantly from 2003 to 2008 and then decrease gradually (See green line).

ts.plot(time_series$Number_of_police_reports)
lines(ksmooth(time(time_series$Number_of_police_reports), time_series$Number_of_police_reports, 'normal', b=5), col=4, lwd=2)
lines(ksmooth(time(time_series$Number_of_police_reports), time_series$Number_of_police_reports, 'normal', b=30), col=3, lwd=2)

# Take the first difference to make the data stationary 
time_series_diff = diff(time_series$Number_of_police_reports)
ts.plot(time_series_diff)
# ACF and PACF after having taken the difference
acf(time_series_diff, lag=100)
pacf(time_series_diff, lag=100)
# Testing different SARIMA models
sarima(time_series_diff, p=1, d=0, q=1, P=1, D=1, Q=0, S=12, details = TRUE)  
sarima(time_series_diff, p=1, d=0, q=1, P=1, D=1, Q=1, S=12, details = TRUE)  
sarima(time_series_diff, p=1, d=0, q=0, P=1, D=1, Q=1, S=12, details = TRUE)  
sarima(time_series_diff, p=0, d=0, q=0, P=1, D=1, Q=1, S=12, details = TRUE)  
sarima(time_series_diff, p=1, d=0, q=0, P=1, D=1, Q=0, S=12, details = TRUE)  
sarima(time_series_diff, p=1, d=0, q=1, P=0, D=1, Q=1, S=12, details = TRUE)   
sarima(time_series_diff, p=2, d=0, q=2, P=2, D=1, Q=1, S=12, details = TRUE)  
sarima(time_series_diff, p=2, d=0, q=0, P=1, D=1, Q=1, S=12, details = TRUE) 
sarima(time_series_diff, p=2, d=0, q=1, P=2, D=1, Q=1, S=12, details = TRUE)  
# 24-Month Forecast 
# The model (1,1,1)x(0,1,1)_12 was selected to make predictions since it has the smallest AIC
forecast <- sarima.for(time_series$Number_of_police_reports, p=1, d=1, q=1, P=0, D=1, Q=1, S=12, n.ahead = 24)
