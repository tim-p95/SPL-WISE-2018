## set the working directory
wd = "C:/Users/timpe_000/Desktop/SPL Projekt"
setwd(wd)


## weather data ------------------------------------------------------------------------

#read weather data and transform into dataframe
weather_all = data.frame(read.csv("weather_data.csv", header = TRUE, sep = ";", na.strings = "-999"))
str(weather_all)

#add dates in date format
str(weather_all$MESS_DATUM_BEGINN); str(weather_all$MESS_DATUM_ENDE)

weather_all$date_beg = as.Date(sapply(weather_all$MESS_DATUM_BEGINN, toString), format = "%Y%m%d")
weather_all$date_end = as.Date(sapply(weather_all$MESS_DATUM_ENDE, toString), format = "%Y%m%d")

class(weather_all$date_beg); class(weather_all$date_end)

#get relevant time period of the weather data to match tourism data
tail(weather_all)
weather = subset(weather_all, as.numeric(format(date_beg, format="%Y")) >= 2010)
rownames(weather) <- seq(1:nrow(weather))


## import daily weather data for certain variables
weather_all_daily = data.frame(read.csv("weather_data_daily.csv", header = TRUE, sep = ";", na.strings = "-999"))
str(weather_all_daily)

#converting date into R Date format
weather_all_daily$date = as.Date(sapply(weather_all_daily$MESS_DATUM, toString), format = "%Y%m%d")
weather_daily = subset(weather_all_daily, as.numeric(format(date, format="%Y")) >= 2010)

#deleting not relevant columns
weather_daily[, c(1, 2, 3, 6, 12, 13, 15, 19)] = NULL

#adding month-year combination as date_id for matching with the monthly tourism_data
weather_daily$date_id = as.character(format(weather_daily$date, format = "%m-%Y"))

#check for missing data
na_count = sapply(weather_daily, function(x){sum(is.na(x))})

#delete measurement of snow due to many missing observations
weather_daily$SHK_TAG = NULL


#replace other missing variables with mean value
mean_fx = mean(weather_daily$FX, na.rm = TRUE)
mean_fm = mean(weather_daily$FM, na.rm = TRUE)
mean_nm = mean(weather_daily$NM, na.rm = TRUE)

weather_daily$FX[is.na(weather_daily$FX)] = mean_fx
weather_daily$FM[is.na(weather_daily$FM)] = mean_fm
weather_daily$NM[is.na(weather_daily$NM)] = mean_nm


## description for weather variables
weather_description = data.frame("variable" = c(as.character(colnames(weather)), "--- DAILY OBSERVATIONS ---", as.character(colnames(weather_daily))))
weather_description$description = c("Identification number of the weather measuring station", 
                                    "Beginning of the interval of measurement", 
                                    "End of the interval of mesurement",
                                    "Quality level of the following columns", 
                                    "Monthly mean of the grade of cloud coverage",
                                    "Monthly mean of the air temperature in two meters height (°C)",
                                    "Monthly mean of the daily maximum of air temperature in two meters height (°C)",
                                    "Monthly mean of the daily minimum of air temperature in two meters height (°C)",
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
                                    "Form of rainfall (0 - 9) -> 0-no rainfall; 1-only rain; 4-unknown form; 6-only rain; 7-only snow; 8-rain and sonw; 9-not measurable rainfall",
                                    "Daily hours of Sunshine (h)",
                                    "Daily mean of cloud coverage (1/8)",
                                    "Daily mean of temperature (°C)",
                                    "Daily maximum of air temperature in two meters heigth (°C)",
                                    "Daily minimum of air temperature in two meters heigth (°C)",
                                    "Daily minimum of air temperature in 0.05 meters heigth (°C)",
                                    "Date of measurement in R Date format (format = '%Y%m%d')",
                                    "Combination of month and year (format = '%Y-%m')"
                                    )


## tourism data ------------------------------------------------------------------------

## import tourism data
#monthly number of guests
guests = data.frame(read.csv("guests.txt", header = TRUE, sep = ";"))

#monthly number of overnight stays
overnight = data.frame(read.csv("overnight.txt", header = TRUE, sep = ";"))

#change column names
colnames(guests) = c("trav_area", "year", "month", "data_basis", "num_guests", "comments","X")
colnames(overnight) = c("trav_area", "year", "month", "data_basis", "num_nights", "comments","X")

#extract relevant numbers from tourism data
guest_count = guests$num_guests
night_count = overnight$num_nights

#compare beginning and end of tourism and weather data to see whether all start and end at the same date
head(guests); head(overnight); head(weather)
tail(guests); tail(overnight); tail(weather)

#combining weather data and tourism data
tourism = data.frame(weather, guest_count, night_count)

#including names for month
tourism$month_name = factor(rep(c(month.name), 8), levels = c(month.name), ordered = TRUE)


## Handle Missing Data:
#count columnwise NAs:
na_count = sapply(tourism, function(x){sum(is.na(x))})

#extracting relevant columns:
na_count[which(na_count != 0)]

#calculate monthwise means for all potentially relevant tourism variables
numeric_cols = unlist(lapply(tourism, is.numeric)) 
tourism_num = data.frame("month" = seq(1:12), tourism[ , numeric_cols])
tourism_num[2:4] = NULL

#install.packages("dplyr")
library(dplyr)

mon = tourism_num %>%
  group_by(month) %>%
  summarise_all(funs(mean), na.rm = TRUE)

month_means = data.frame(mon)

mean_names = paste("mean_", colnames(tourism_num), sep = "")
colnames(month_means) = mean_names

month_means = data.frame("month_name" = month.name, month_means)


#exchanging the missing values with the mean of the particular month from month_means data frame
tourism$MO_N[is.na(tourism$MO_N)] = month_means$mean_MO_N[as.numeric(format(tourism$date_beg, format="%m"))[is.na(tourism$MO_N)]]
tourism$MO_FK[is.na(tourism$MO_FK)] = month_means$mean_MO_FK[as.numeric(format(tourism$date_beg, format="%m"))[is.na(tourism$MO_FK)]]
tourism$MO_RR[is.na(tourism$MO_RR)] = month_means$mean_MO_RR[as.numeric(format(tourism$date_beg, format="%m"))[is.na(tourism$MO_RR)]]


## checking for outliers in guests and nights
summary(tourism$guest_count); summary(tourism$night_count)

library(ggplot2)

ggplot(tourism, aes(y = guest_count, x = month_name, fill=month_name)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  ggtitle("Boxplots of the number of Guest in different months respectively") +
  xlab("Month") + 
  ylab("Guests")

ggplot(tourism, aes(y = night_count, x = month_name, fill=month_name)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  ggtitle("Boxplots of the number of overnight stays in different months respectively") +
  xlab("Month") + 
  ylab("Overnight Stays")

#extracting the outlier in April
april = subset(tourism, month_name == "April")
april_outl = which(april$guest_count > mean(april$guest_count) + 2*sd(april$guest_count))
april[april_outl, ]
