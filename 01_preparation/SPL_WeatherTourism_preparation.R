# Name of Quantlet: SPL_WeatherTourism_preparation
# Published in:     'Statistical programming languages - Student Project on ''Impact of Meteorological Factors on Regional Tourism'' '
# Description:      'Loading Packages, Import and first preparation of data, Description of Weather Variables'
# Keywords:         packages, import, csv, date format, matching data sources
# Author:           Tim Peschenz
# Submitted:        So, Mar 17 2019


## set the working directory
wd = "C:/Users/timpe_000/Desktop/sPL-WISE-2018/01_preparation"
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

citation("caret")


############################################################
## Data Import
############################################################

## import monthly weather data ------------------------------
# read monthly weather data and transform into data frame
weather_all = data.frame(read.csv("weather_data.csv", header = TRUE, sep = ";", na.strings = "-999"))

# add dates in date format to replace orignal date structure
weather_all$date_beg = as.Date(sapply(weather_all$MESS_DATUM_BEGINN, toString), format = "%Y%m%d")
weather_all$date_end = as.Date(sapply(weather_all$MESS_DATUM_ENDE, toString), format = "%Y%m%d")

weather_all$MESS_DATUM_BEGINN = NULL
weather_all$MESS_DATUM_ENDE   = NULL

# get relevant time period of the weather data to match tourism observations
weather           = subset(weather_all, as.numeric(format(date_beg, format="%Y")) >= 2010)
rownames(weather) = seq(1:nrow(weather))


## import daily weather data ------------------------------
#read daily weather data and transform into data frame
weather_all_daily = data.frame(read.csv("weather_data_daily.csv", header = TRUE, sep = ";", na.strings = "-999"))

#converting date into R Date format and replace former format
weather_all_daily$date = as.Date(sapply(weather_all_daily$MESS_DATUM, toString), format = "%Y%m%d")

weather_all_daily$MESS_DATUM = NULL

#extract relevant period of time
weather_daily = subset(weather_all_daily, as.numeric(format(date, format="%Y")) >= 2010)

#deleting not relevant columns
weather_daily[, c(1, 2, 5, 9, 11, 12, 14, 18)] = NULL

#adding month-year combination for matching daily weather data with monthly tourism_data
weather_daily$date_id = as.character(format(weather_daily$date, format = "%m-%Y"))


## description for weather variables
weather_description = data.frame("variable" = c(as.character(colnames(weather)), 
                                                "--- DAILY OBSERVATIONS ---", 
                                                as.character(colnames(weather_daily))))

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
                                    6-only rain; 7-only snow; 8-rain and sonw; 9-not measurable rainfall",
                                    "Daily hours of Sunshine (h)",
                                    "Daily mean of cloud coverage (1/8)",
                                    "Daily mean of temperature (°C)",
                                    "Daily maximum of air temperature in two meters heigth (°C)",
                                    "Daily minimum of air temperature in two meters heigth (°C)",
                                    "Daily minimum of air temperature in 0.05 meters heigth (°C)",
                                    "Date of measurement in R Date format (format = '%Y%m%d')",
                                    "Combination of month and year (format = '%Y-%m')"
)
