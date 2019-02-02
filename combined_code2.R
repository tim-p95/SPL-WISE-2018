## set the working directory
wd = "C:/Users/timpe_000/Desktop/SPL Projekt"
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
weather_all$MESS_DATUM_ENDE = NULL

# get relevant time period of the weather data to match tourism observations
weather = subset(weather_all, 
                 as.numeric(format(date_beg, format="%Y")) >= 2010)

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


## import tourism data ------------------------------
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


## combining weather data and tourism data
tourism = data.frame(weather, guest_count, night_count)


## including names for month
tourism$month_name = factor(rep(c(month.name), 8), levels = c(month.name), ordered = TRUE)



############################################################
## Analysis of Weather
############################################################

## Handle Missing Data for monthly weather data ------------------------------
#count columnwise NAs:
na_count = sapply(tourism, function(x){sum(is.na(x))})

#extracting relevant columns:
na_count[which(na_count != 0)]

#calculate monthwise means for all potentially relevant tourism and weather variables
numeric_cols = unlist(lapply(tourism, is.numeric)) 
tourism_num = data.frame("month" = seq(1:12), tourism[ , numeric_cols])
tourism_num[c(2, 3, 13)] = NULL

mon = tourism_num %>%
  group_by(month) %>%
  summarise_all(funs(mean), na.rm = TRUE)

month_means = data.frame(mon)

#change column names of monthwise observations 
colnames(month_means) = paste("mean_", colnames(tourism_num), sep = "")

#addig month names
month_means = data.frame("month_name" = month.name, month_means)

#exchanging the missing values with the mean of the particular month from month_means data frame
tourism$MO_N[is.na(tourism$MO_N)] = 
  month_means$mean_MO_N[as.numeric(format(tourism$date_beg, format="%m"))[is.na(tourism$MO_N)]]
tourism$MO_FK[is.na(tourism$MO_FK)] = 
  month_means$mean_MO_FK[as.numeric(format(tourism$date_beg, format="%m"))[is.na(tourism$MO_FK)]]
tourism$MO_RR[is.na(tourism$MO_RR)] = 
  month_means$mean_MO_RR[as.numeric(format(tourism$date_beg, format="%m"))[is.na(tourism$MO_RR)]]


## check for missing data in daily weather data ------------------------------
na_count = sapply(weather_daily, function(x){sum(is.na(x))})

#replace other missing variables with mean value
weather_daily$FX[is.na(weather_daily$FX)] = mean(weather_daily$FX, na.rm = TRUE)
weather_daily$FM[is.na(weather_daily$FM)] = mean(weather_daily$FM, na.rm = TRUE)
weather_daily$NM[is.na(weather_daily$NM)] = mean(weather_daily$NM, na.rm = TRUE)


## long time development of weather for the whole available period of time ------------------------------
#temperature
ggplot(data = weather_all, aes(x = date_beg)) + 
  geom_smooth(aes(y = MO_TT, color = "MO_TT"), method = "auto", se = FALSE) +
  geom_smooth(aes(y = MO_TX, color = "MO_TX"), method = "auto", se = FALSE) +
  geom_smooth(aes(y = MO_TN, color = "MO_TN"), method = "auto", se = FALSE) +
  labs(title = "Long-run Temperature Development", x = "Year", y = "Temperature [°C]", color = "Measures:") +
  scale_color_manual(labels = c("Minimum Temperature", "Average Temperature", "Maximum Temperature"), values = c("blue", "green", "red")) +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.justification=c(1,0), 
        legend.position=c(1,0))



## categorize days as rainy, sunny, cloudy, windy, hot ------------------------------
#rainy days (rainfall amount above 3mm a day)
weather_daily$rainy = sapply(weather_daily$RSK, FUN = function(x) ifelse(x > 1, 1, 0))

#count total number of rainy days in time period
sum(weather_daily$rainy)


#sunny days (sunshine hours above 10 hours a day)
weather_daily$sunny = sapply(weather_daily$SDK, FUN = function(x) ifelse(x > 10, 1, 0))

#count total number of sunny days in time period
sum(weather_daily$sunny)


#cloudy days (cloud coverage above 7.75 on okta scale)
weather_daily$cloudy = sapply(weather_daily$NM, FUN = function(x) ifelse(x > 7.75, 1, 0))

#count total number of cloudy days in time period
sum(weather_daily$cloudy)


#windy days (wind speed above 6 m/s)
weather_daily$windy = sapply(weather_daily$FX, FUN = function(x) ifelse(x > 6, 1, 0))

#count total number of windy days in time period
sum(weather_daily$windy)


#hot days (maximum temperature above 27 °C)
weather_daily$hot = sapply(weather_daily$TXK, FUN = function(x) ifelse(x > 27, 1, 0))

#count total number of hot days in time period
sum(weather_daily$hot)


## aggregate weather categories according to month-year combination
weather_days = weather_daily[, c(12:17)]
days = data.frame(weather_days %>% group_by(date_id) %>% summarize_all(sum))

#arrange observations in correct time order
days$month = substr(days$date_id, 1, 2)
days$year = substr(days$date_id, 4, 7)

days = days %>% arrange(year, month)

#adding day counts of weather categories to respective observation in tourism data frame
tourism$rainy = days$rainy
tourism$sunny = days$sunny
tourism$cloudy = days$cloudy
tourism$windy = days$windy
tourism$hot = days$hot


## create dataframe with monthly weather deviations ------------------------------
weather_num = data.frame(tourism[, c(3:11, 13, 14)])
weather_means = data.frame(month_means[3:13])

#compare observations
data.frame(colnames(weather_num), colnames(weather_means))

#repeat means for whole observation period
weather_means = do.call("rbind", replicate(8, weather_means, simplify = FALSE))

weather_deviation = weather_num - weather_means


## check for possible reductions of weather variables using factor analysis ------------------------------
#prepare seperate data frame for factor analysis
fa = data.frame(weather_num)

#normalize data frame
fa_n = scale(fa)

#create correlation matrix for numeric variables
fa_cor = round(cor(fa_n), 3)

#visualize correlations
corrplot(fa_cor, method="circle", type = "upper", col = brewer.pal(n=8, name="RdYlBu"))

#create screeplot to specify numbers of factors
plot(eigen(cor(fa[, 3:8]))$values, type = "o", col = "blue", pch = 16, 
     cex = 2, xlab = "Number of Factors", ylab = "Eigenvalues", lwd = 2)
abline(h = 1, lwd = 2, col = "red")

#extract factors
rotated_factor = fa(r = fa, nfactors = 2, fm = "pa", rotate = "varimax", 
                    scores = "regression", min.err = 0.002)
print(rotated_factor, cut = 0, digits = 3) 

#visualize factors
factor.plot(rotated_factor, labels=rownames(rotated_factor$loadings))
fa.diagram(rotated_factor, simple = FALSE, cut = 0, digits = 3, errors = TRUE)


## use average temerature as single temperature variable
tourism[, c(5:6, 8, 10)] = NULL



############################################################
## Analysis of Tourism
############################################################

## checking for outliers in guests and nights ------------------------------
ggplot(tourism, aes(y = guest_count, x = month_name, fill=month_name)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  ggtitle("Boxplots of the Number of Guest in Different Months Respectively") +
  xlab("Month") + 
  ylab("Guests") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.justification=c(1,1), 
        legend.position=c(1,1))

ggplot(tourism, aes(y = night_count, x = month_name, fill=month_name)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  ggtitle("Boxplots of the Number of Overnight Stays in Different Months Respectively") +
  xlab("Month") + 
  ylab("Overnight Stays") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.justification=c(1,1), 
        legend.position=c(1,1))


## extracting the outlier in April
april = subset(tourism, month_name == "April")
april_outl = which(april$guest_count > mean(april$guest_count) + 2*sd(april$guest_count))
april[april_outl, ]


## calculate correlations between number of guests and nights ------------------------------
cor(tourism$guest_count, tourism$night_count)


## visualize distribution of guests counts ------------------------------
ggplot(tourism, aes(x = guest_count)) + 
  geom_histogram(aes(y=..density..), binwidth = 2500, colour="black", fill="white") +
  geom_density(alpha=.15, fill="#31B404") +
  ggtitle("Density Function and Histogramm of the Number of Guests Over the Observed Time Period") +
  xlab("Guests") + 
  ylab("Density Function and Histogramm") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))


## visualize development of guests ------------------------------
ggplot(data = tourism, aes(x = date_beg, y = guest_count)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the Number of Guests") +
  xlab("Guests") + 
  ylab("Number of Guests") +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.justification=c(1,0), 
        legend.position=c(1,0))


## adding average number of days spent in region (overnight stays / guests) ------------------------------
tourism$avg_time = tourism$night_count/tourism$guest_count


## creating overall monthly averages for tourism data ------------------------------
tourism_tab = data.table(tourism)
monthly = data.frame(tourism_tab[,list(mean_guest = mean(guest_count),
                                       sd_guest = sd(guest_count), 
                                       mean_nights = mean(night_count),
                                       sd_nights = sd(night_count)),
                                 by=tourism$month_name])

colnames(monthly)[1] = "Month"


## comparing average guests per month to find out seasonal month ------------------------------
ggplot(data = monthly, aes(x = Month, y = mean_guest)) +
  geom_bar(stat="identity") +
  ggtitle("Average Number of Guests in Corresponding Month") +
  xlab("Month") + 
  ylab("Guests") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))


#define season month (guests > 50000)
season_month = monthly$Month[which(monthly$mean_guest > 50000)]

#adding binary variable that specifies whether a month is in the season or not
season_check = function(x, y){
  if(x %in% y){
    return ("Yes")
  }else{
    return("No")
  }
}

tourism$season = factor(sapply(as.list(tourism$month_name), season_check, season_month))


#comparing numbers in season and no season
mean(tourism$guest_count[tourism$season == "Yes"])
mean(tourism$guest_count[tourism$season == "No"])


##development of average number of nights per guest ------------------------------
ggplot(data = tourism, aes(x = date_beg, y = avg_time)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the average number of nights spent per guest") +
  xlab("Time") + 
  ylab("Average number of nights") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
  

## relationship between season and average nights per guest ------------------------------
cor(as.numeric(tourism$season), tourism$avg_time)


## calculate the absolute deveation of guests and overnigth stays from monthly mean ------------------------------
tourism$abs_guest_deviation = 
  tourism$guest_count - c(replicate(8, month_means$mean_guest_count))
tourism$abs_night_deviation = 
  tourism$night_count - c(replicate(8, month_means$mean_night_count))


## calculate relative deviation in percent ------------------------------
tourism$rel_guest_deviation = tourism$abs_guest_deviation/tourism$guest_count
tourism$rel_night_deviation = tourism$abs_night_deviation/tourism$night_count


## calculate annual growth of guests and nights compared to basis year = 2010 ------------------------------
obs = data.frame("year" = format(tourism$date_beg, format = "%Y"))
obs$guest_count = tourism$guest_count
obs$night_count = tourism$night_count

years_index = obs %>%
  group_by(year) %>%
  summarize_all(sum)

years_index$guest_growth = (years_index$guest_count/years_index$guest_count[1])-1
years_index$night_growth = (years_index$night_count/years_index$night_count[1])-1

#plot growth rates compared to basis year
ggplot(years_index, aes(x = c(2010:2017))) + 
  geom_line(aes(y = guest_growth, color = "blue")) +
  geom_line(aes(y = night_growth, color = "red")) +
  ggtitle("Development of Growthrates for Guests and Overnight Stays") +
  xlab("Year") + 
  ylab("Growth rate") +
  scale_colour_manual(labels = c("Guests", "Nights"), values = c("blue", "red")) +
  geom_point(aes(y = guest_growth, color = "blue")) +
  geom_point(aes(y = night_growth, color = "red")) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.justification=c(1,0), 
        legend.position=c(1,0),
        legend.title=element_blank())


## recalculate guests and nights "without" growth (in values of basis year 2010) ------------------------------
tourism$guest_basis = 0

j = 2010
for(i in c(1:8)){
  tourism[which(as.numeric(format(tourism$date_beg, format = "%Y")) == j), ]$guest_basis = 
    tourism$guest_count[which(as.numeric(format(tourism$date_beg, format = "%Y")) == j)]/
    (1+years_index$guest_growth[i])
  j = j + 1
}

#for nights
tourism$night_basis = 0

j = 2010
for(i in c(1:8)){
  tourism[which(as.numeric(format(tourism$date_beg, format = "%Y")) == j), ]$night_basis = 
    tourism$night_count[which(as.numeric(format(tourism$date_beg, format = "%Y")) == j)]/
    (1+years_index$night_growth[i])
  j = j + 1
}


## check adjusted development ------------------------------
ggplot(data = tourism, aes(x = date_beg, y = guest_basis)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the number of guests stays for the observed time period") +
  xlab("Guests") + 
  ylab("Number of guests") +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))


## calculate monthly means of newly generated basis variables ------------------------------
basis = data.frame("month" = rep(seq(1:12), 8))
basis$guest_basis = tourism$guest_basis
basis$night_basis = tourism$night_basis

basis = data.table(basis)
mon_basis = data.frame(basis[,list(mean_guest_basis = mean(guest_basis),
                                   sd_guest_basis = sd(guest_basis), 
                                   mean_nights_basis = mean(night_basis),
                                   sd_nights_basis = sd(night_basis)),
                             by=basis$month])


## calculate the deveation of guests and overnigth stays from monthly mean of the basis observations ------------------------------
tourism$abs_guest_dev_basis = tourism$guest_basis - c(replicate(8, mon_basis$mean_guest_basis))
tourism$abs_night_dev_basis = tourism$night_basis - c(replicate(8, mon_basis$mean_nights_basis))


## find out relative deviation in percent from monthly mean of the basis observations ------------------------------
tourism$rel_guest_dev_basis = tourism$abs_guest_dev_basis/tourism$guest_basis
tourism$rel_night_dev_basis = tourism$abs_night_dev_basis/tourism$night_basis


#delete not relevant columns from tourism data frame
tourism$abs_guest_deviation = NULL
tourism$abs_night_deviation = NULL
tourism$rel_guest_deviation = NULL
tourism$rel_night_deviation = NULL




############################################################
## Build Regression Models
############################################################

## extract the relevant data for the analysis
lr_data = data.frame("rel_guest_dev_basis" = tourism$rel_guest_dev_basis,
                     "rel_night_dev_basis" = tourism$rel_night_dev_basis,
                     "avg_time" = tourism$avg_time,
                     "MO_TT" = tourism$MO_TT,
                     "MO_SD_S" = tourism$MO_SD_S,
                     "MO_RR" = tourism$MO_RR)


## Stage 1: run linear regression with absolute values for weather variables ------------------------------
#1 average nights per guest
lm_avg_nights = lm(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
summary(lm_avg_nights)

#2 relative deviation of guests
lm_guest_dev = lm(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
summary(lm_guest_dev)

#3 relative deviation of nights
lm_night_dev = lm(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)
summary(lm_night_dev)


#create output tables for latex
stargazer(lm_avg_nights, lm_guest_dev, lm_night_dev, title="Regression Results with Original Weather Measures", 
          align=TRUE, 
          dep.var.labels=c("Average Nights","Deviation of Guests", "Deviation of Nights"),
          covariate.labels=c("Temperature (°C)","Sun Hours (h)", "Rainfall (mm)"))


## Durbin-Watson Test for autocorrelation
dwtest(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
dwtest(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
dwtest(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)


## check for multicollineraity
#correlations
cor(lr_data[, 4:6])

#variance infation factor
car::vif(lm_avg_nights)
car::vif(lm_guest_dev)
car::vif(lm_night_dev)


## check for heteroskedasticity
bptest(lm_avg_nights)
bptest(lm_guest_dev)
bptest(lm_night_dev)


## Stage 2: run linear regression with deviations of weather variables ------------------------------
#adjust data for linear regression
lr_data[, 4:6] = NULL
lr_data$temp_dev = weather_deviation$MO_TT
lr_data$sun_dev = weather_deviation$MO_SD_S
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


#create output tables for latex ------------------------------
stargazer(lm_avg_nights, lm_guest_dev, lm_night_dev, title="Regression Results with deviations weather measures", 
          align=TRUE, 
          dep.var.labels=c("Average Nights","Deviation of Guests", "Deviation of Nights"),
          covariate.labels=c("Deviation of Temperature","Deviation of Sun Hours", "Deviation of Rainfall"))


## Durbin-Watson Test for autocorrelation ------------------------------
dwtest(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
dwtest(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
dwtest(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)


## check for multicollineraity
#correlations
cor(lr_data[, 4:6])

#variance infation factor
car::vif(lm_avg_nights)
car::vif(lm_guest_dev)
car::vif(lm_night_dev)


## check for heteroskedasticity
bptest(lm_avg_nights)
bptest(lm_guest_dev)
bptest(lm_night_dev)





## Stage 3: regression with adding day counts of weather categories
#adjust data for regression
lr_data$rainy = days$rainy
lr_data$sunny = days$sunny
lr_data$cloudy = days$cloudy
lr_data$windy = days$windy
lr_data$hot = days$hot

#1 average nights per guest
lm_avg_nights = lm(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
summary(lm_avg_nights)

#2 relative deviation of guests
lm_guest_dev = lm(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
summary(lm_guest_dev)

#3 relative deviation of nights
lm_night_dev = lm(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)
summary(lm_night_dev)

#create output tables for latex
stargazer(lm_avg_nights, lm_guest_dev, lm_night_dev, title="Regression Results with deviations weather measures and day counts for weather categories", 
          align=TRUE, 
          dep.var.labels=c("Average Nights","Deviation of Guests", "Deviation of Nights"),
          covariate.labels=c("Deviation of Temperature","Deviation of Sun Hours", 
                             "Deviation of Rainfall", "Rainy Days", "Sunny Days", "Cloudy Days", "Windy Days", "Hot Days"))


## Durbin-Watson Test for autocorrelation
dwtest(avg_time ~ .-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
dwtest(rel_guest_dev_basis ~ .-rel_night_dev_basis-avg_time, data = lr_data)
dwtest(rel_night_dev_basis ~ .-rel_guest_dev_basis-avg_time, data = lr_data)


## check for multicollineraity
#correlations
cor(lr_data[, 4:11])

#variance infation factor
car::vif(lm_avg_nights)
car::vif(lm_guest_dev)
car::vif(lm_night_dev)


## check for heteroskedasticity with 
bptest(lm_avg_nights)
bptest(lm_guest_dev)
bptest(lm_night_dev)

