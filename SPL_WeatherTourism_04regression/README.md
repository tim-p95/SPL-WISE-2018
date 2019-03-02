![banner](../images/banner.png)

# SPL_WeatherTourism_04regression ![qloqo](../images/qloqo.png) 

```
Name of Quantlet:  SPL_WeatherTourism_04regression

Published in:      'Statistical programming languages - Student Project on ''Impact of Meteorological Factors on Regional Tourism'' '

Description:       'Several multiple regression models to measure the impact of weather factors on tourism'

Keywords:          regression model, multiple regression, autocorrelation, multicollinearity, heteroskedasticity

Author:            Tim Peschenz

Submitted:         So, Mar 17 2019
```

## R Code
```R
# Name of Quantlet:  SPL_WeatherTourism_04regression
# Published in:      'Statistical programming languages - Student Project on ''Impact of Meteorological Factors on Regional Tourism'' '
# Description:       'Several multiple regression models to measure the impact of weather factors on tourism'
# Keywords:          regression model, multiple regression, autocorrelation, multicollinearity, heteroskedasticity
# Author:            Tim Peschenz
# Submitted:         So, Mar 17 2019


############################################################
## Build Regression Models
############################################################

### extract the relevant data for the analysis ###
lr_data = data.frame("rel_guest_dev_basis" = tourism$rel_guest_dev_basis,
                     "rel_night_dev_basis" = tourism$rel_night_dev_basis,
                     "avg_time"            = tourism$avg_time,
                     "MO_TT"               = tourism$MO_TT,
                     "MO_SD_S"             = tourism$MO_SD_S,
                     "MO_RR"               = tourism$MO_RR)


### Stage 1: run linear regression with absolute values for weather variables ###
#1 average nights per guest
lm_avg_nights = lm(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
summary(lm_avg_nights)

#2 relative deviation of guests
lm_guest_dev = lm(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
summary(lm_guest_dev)

#3 relative deviation of nights
lm_night_dev = lm(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)
summary(lm_night_dev)


### create output tables for latex ###
stargazer(lm_avg_nights, lm_guest_dev, lm_night_dev, title="Regression Results with Original Weather Measures", 
          align=TRUE, 
          dep.var.labels=c("Average Nights","Deviation of Guests", "Deviation of Nights"),
          covariate.labels=c("Temperature (°C)","Sun Hours (h)", "Rainfall (mm)"))


### Durbin-Watson Test for autocorrelation ###
dwtest(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
dwtest(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
dwtest(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)

#Since the p-values for the Durbin-Watson tests are far from 0.05, we cannot reject the Null-
#Hypothesis that there is no autocorrelation.


### check for multicollinearity ###
#correlations
cor(lr_data[, 4:6])

#variance inflation factor
car::vif(lm_avg_nights)
car::vif(lm_guest_dev)
car::vif(lm_night_dev)

#The VIF-values are all below 5. Therefore there should be no multicollinearity among the independent
#variables.


### check for heteroskedasticity ###
bptest(lm_avg_nights)
bptest(lm_guest_dev)
bptest(lm_night_dev)

#Since the p-values for the Breusch-Pagan tests are not close to 0.05, we cannot reject the Null-
#Hypothesis that there is no heteroskedasticity.


### Stage 2: run linear regression with deviations of weather variables ###
#adjust data for linear regression
lr_data[, 4:6]   = NULL
lr_data$temp_dev = weather_deviation$MO_TT
lr_data$sun_dev  = weather_deviation$MO_SD_S
lr_data$rain_dev = weather_deviation$MO_RR


#1 average nights per guest
lm_avg_nights = lm(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
summary(lm_avg_nights)

#2 relative deviation of guests
lm_guest_dev = lm(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
summary(lm_guest_dev)

#3 relative deviation of nights
lm_night_dev = lm(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)
summary(lm_night_dev)


### create output tables for latex ###
stargazer(lm_avg_nights, lm_guest_dev, lm_night_dev, title="Regression Results with deviations weather measures", 
          align=TRUE, 
          dep.var.labels=c("Average Nights","Deviation of Guests", "Deviation of Nights"),
          covariate.labels=c("Deviation of Temperature","Deviation of Sun Hours", "Deviation of Rainfall"))


### Durbin-Watson Test for autocorrelation ###
dwtest(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
dwtest(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
dwtest(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)

#The p-values for the Durbin-Watson tests are far from 0.05 for the model 2 and 3 (i.e. the relative 
#deviation for guests and nights). The Null-Hypothesis cannot be rejected in these cases.Therefore,
#there is probably no autocorrelation in these models. The Durbin-Watson test for the model with 
#the average number of nights per guest has a much lower p-Value (< 0.0005), meaning that we definitely
#have to reject the Null-Hypothesis of no autocorrelation.


### check for multicollineraity ###
#correlations
cor(lr_data[, 4:6])

#variance infation factor
car::vif(lm_avg_nights)
car::vif(lm_guest_dev)
car::vif(lm_night_dev)

#The VIF-values are all below 5. Therefore there should be no multicollinearity among the independent
#variables.


### check for heteroskedasticity ###
bptest(lm_avg_nights)
bptest(lm_guest_dev)
bptest(lm_night_dev)

#Since the p-values for the Breusch-Pagan tests are not close to 0.05, we cannot reject the Null-
#Hypothesis that there is no heteroskedasticity.


### Stage 3: regression with adding day counts of weather categories ###
#adjust data for regression
lr_data$rainy  = days$rainy
lr_data$sunny  = days$sunny
lr_data$cloudy = days$cloudy
lr_data$windy  = days$windy
lr_data$hot    = days$hot

#1 average nights per guest
lm_avg_nights = lm(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
summary(lm_avg_nights)

#2 relative deviation of guests
lm_guest_dev = lm(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
summary(lm_guest_dev)

#3 relative deviation of nights
lm_night_dev = lm(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)
summary(lm_night_dev)

### create output tables for latex ###
stargazer(lm_avg_nights, lm_guest_dev, lm_night_dev, title="Regression Results with deviations weather measures and day counts for weather categories", 
          align=TRUE, 
          dep.var.labels=c("Average Nights","Deviation of Guests", "Deviation of Nights"),
          covariate.labels=c("Deviation of Temperature","Deviation of Sun Hours", 
                             "Deviation of Rainfall", "Rainy Days", "Sunny Days", "Cloudy Days", "Windy Days", "Hot Days"))


### Durbin-Watson Test for autocorrelation ###
dwtest(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
dwtest(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
dwtest(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)

#Since the p-values for the Breusch-Pagan tests are not close to 0.05, we cannot reject the Null-
#Hypothesis that there is no heteroskedasticity.


### check for multicollinearity ###
#correlations
cor(lr_data[, 4:11])

#variance infation factor
car::vif(lm_avg_nights)
car::vif(lm_guest_dev)
car::vif(lm_night_dev)

#The VIF-values are all below 5. Therefore there should be no multicollinearity among the independent
#variables.


### check for heteroskedasticity with ###
bptest(lm_avg_nights)
bptest(lm_guest_dev)
bptest(lm_night_dev)

#The p-values for the Breusch-Pagan tests are not above 0.05 for the first and the third model. For these,
#we cannot reject the Null-Hypothesis that there is no heteroskedasticity. However for the second model 
#with the relative deviation of guests, there is a significant result (p-Value < 0.02), meaning that
#we have to reject the Null-Hypothesis of no heteroskedasticity.

