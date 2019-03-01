![banner](/images/banner.png)

![qloqo](/images/qloqo.png) 
# SPL_WeatherTourism_01preparation ![qloqo](/images/qloqo.png) 

```
Name of Quantlet: SPL_WeatherTourism_01preparation

Published in:     'Statistical programming languages - Student Project on ''Impact of Meteorological Factors on Regional Tourism'' '

Description:      'Loading Packages, Import and first preparation of data, Description of Weather Variables'

Keywords:         packages, import, csv, date format, matching data sources

Author:           Tim Peschenz

Submitted:        So, Mar 17 2019
```


## R Code
```R
# Name of Quantlet: SPL_WeatherTourism_01preparation
# Published in:     'Statistical programming languages - Student Project on ''Impact of Meteorological Factors on Regional Tourism'' '
# Description:      'Loading Packages, Import and first preparation of data, Description of Weather Variables'
# Keywords:         packages, import, csv, date format, matching data sources
# Author:           Tim Peschenz
# Submitted:        So, Mar 17 2019


## set the working directory
wd = "C:/Users/timpe_000/Desktop/SPL-WISE-2018/SPL_WeatherTourism_01preparation"
setwd(wd)


## loading packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(psych)
library(data.table)
library(ggcorrplot)
library(stargazer)
library(lmtest)
library(caret)


############################################################
## Data Import
############################################################

### import monthly weather data ###
#read monthly weather data and transform it into a data frame
weather_all = data.frame(read.csv("weather_data.csv", header = TRUE, sep = ";", na.strings = "-999"))

#Missing values are represented by the value "-999" (replaced with NA).

#add dates in date format to replace original date structure
weather_all$date_beg = as.Date(sapply(weather_all$MESS_DATUM_BEGINN, toString), format = "%Y%m%d")
weather_all$date_end = as.Date(sapply(weather_all$MESS_DATUM_ENDE, toString), format = "%Y%m%d")

#replace former date variables
weather_all$MESS_DATUM_BEGINN = NULL
weather_all$MESS_DATUM_ENDE   = NULL

#get relevant time period of the weather data to match tourism observations
weather           = subset(weather_all, as.numeric(format(date_beg, format="%Y")) >= 2010)
rownames(weather) = seq(1:nrow(weather))

#Observations for tourism start in January 2010 until December 2017 (96 observations) and the
#weather data has to adapted to this time frame.


### import daily weather data ###
#read daily weather data and transform into data frame
weather_all_daily = data.frame(read.csv("weather_data_daily.csv", header = TRUE, sep = ";", na.strings = "-999"))

#converting date into R Date format and replace former format
weather_all_daily$date = as.Date(sapply(weather_all_daily$MESS_DATUM, toString), format = "%Y%m%d")

#delete former date variable
weather_all_daily$MESS_DATUM = NULL

#extract relevant period of time
weather_daily = subset(weather_all_daily, as.numeric(format(date, format="%Y")) >= 2010)

#deleting columns that are not relevant for the analysis, i.e. certain weather measures
weather_daily[, c(1, 2, 5, 9, 11, 12, 14, 18)] = NULL

#Deleted columns: stations id (STATIONS_ID), quality levels (QN_3, QN_4), snow height (SHK_TAG), air
#pressure (VPM, PM), humidity (UPM), end of record (eor)

#adding month-year combination for matching daily weather data with monthly tourism_data
weather_daily$date_id = as.character(format(weather_daily$date, format = "%m-%Y"))
#example: January 2010 = "01-2010"

### description for weather variables ###
weather_description = data.frame("variable" = c(as.character(colnames(weather)), 
                                                "--- DAILY OBSERVATIONS ---", 
                                                as.character(colnames(weather_daily))))

#Because of the numerous different weather variables, a description of the original column names from the 
#weather station data is created, to have an useful orientation point during the whole analysis.

weather_description$description = c("Identification number of the weather measuring station", 
                                    "Quality level of the following columns", 
                                    "Monthly mean of the grade of cloud coverage",
                                    "Monthly mean of the air temperature in two meters height (°C)",
                                    "Monthly mean of the daily maximum of air temperature in 
                                    two meters height (°C)",
                                    "Monthly mean of the daily minimum of air temperature in 
                                    two meters height (°C)",
                                    "Monthly mean of the daily wind level on Beaufort-scale (Bft)",
                                    "Monthly maximum of air temperature in two meters height (°C)",
                                    "Monthly maximum of daily wind level maximum (m/s)",
                                    "Monthly minimum of air temperature in two meters height (°C)",
                                    "Monthly sum of the sunshine hours (h)",
                                    "Quality level of the following columns",
                                    "Monthly sum of the rainfall (mm)",
                                    "Monthly maximum of daily rainfall (mm)",
                                    "End of Data record",
                                    "Beginning of the interval of measurement in R Date format", 
                                    "End of the interval of mesurement in R Date format",
                                    "--- DAILY OBSERVATIONS ---",
                                    "Daily maximum of of wind speed (m/s)",
                                    "Daily mean of wind speed (m/s)",
                                    "Daily amount of rainfall (mm)",
                                    "Form of rainfall (0 - 9) -> 0-no rainfall; 1-only rain; 4-unknown form; 
                                    6-only rain; 7-only snow; 8-rain and snow; 9-not measurable rainfall",
                                    "Daily hours of Sunshine (h)",
                                    "Daily mean of cloud coverage (1/8)",
                                    "Daily mean of temperature (°C)",
                                    "Daily maximum of air temperature in two meters height (°C)",
                                    "Daily minimum of air temperature in two meters height (°C)",
                                    "Daily minimum of air temperature in 0.05 meters height (°C)",
                                    "Date of measurement in R Date format (format = '%Y%m%d')",
                                    "Combination of month and year (format = '%Y-%m')"
)


### import tourism data ###
#monthly number of guests
guests = data.frame(read.csv("guests.csv", header = TRUE, sep = ";"))

#monthly number of overnight stays
overnight = data.frame(read.csv("overnight.csv", header = TRUE, sep = ";"))

#change column names
colnames(guests) = c("trav_area", "year", "month", "data_basis", "num_guests", "comments")
colnames(overnight) = c("trav_area", "year", "month", "data_basis", "num_nights", "comments")

#extract relevant columns from tourism data
guest_count = guests$num_guests
night_count = overnight$num_nights


### combining weather data and tourism data in a data frame called tourism ###
tourism = data.frame(weather, guest_count, night_count)


### including names for month (January - December) ###
tourism$month_name = factor(rep(c(month.name), 8), levels = c(month.name), ordered = TRUE)
```R
