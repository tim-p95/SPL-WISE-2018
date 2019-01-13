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



############################################################
#Analysis of Weather
############################################################

#analyzing weather data
summary(tourism[4:16])

#development of weather for the whole available period of time
#temperature
ggplot(data = weather_all, aes(x = date_beg)) + 
  geom_smooth(aes(y = MO_TT, color = "MO_TT"), method = "auto", se = FALSE) +
  geom_smooth(aes(y = MO_TX, color = "MO_TX"), method = "auto", se = FALSE) +
  geom_smooth(aes(y = MO_TN, color = "MO_TN"), method = "auto", se = FALSE) +
  labs(title = "Temperature Development", x = "Year", y = "Temperature [°C]", color = "Measures:") +
  scale_color_manual(labels = c("Minimum Temperature", "Average Temperature", "Maximum Temperature"), values = c("blue", "green", "red")) +
  expand_limits(y = 0)

#rainfall
ggplot(data = weather_all, aes(x = date_beg)) + 
  geom_smooth(aes(y = MO_RR, color = "MO_RR"), method = "auto", se = FALSE) +
  geom_smooth(aes(y = MX_RS, color = "MX_RS"), method = "auto", se = FALSE) +
  labs(title = "Rainfall Development", x = "Year", y = "Amount of Rainfall [mm]", color = "Measures:") +
  scale_color_manual(labels = c("Average Rainfall", "Maximum Rainfall"), values = c("blue", "green")) +
  expand_limits(y = 0)

#wind
ggplot(data = weather_all, aes(x = date_beg)) + 
  geom_smooth(aes(y = MO_FK, color = "MO_FK"), method = "auto", se = FALSE) +
  geom_smooth(aes(y = MX_FX, color = "MX_FX"), method = "auto", se = FALSE) +
  labs(title = "Wind Strength Development", x = "Year", y = "Wind Level [Bft]", color = "Measures:") +
  scale_color_manual(labels = c("Average Wind Level", "Maximum Wind Level"), values = c("blue", "green")) +
  expand_limits(y = 0)

#sunshine hours
ggplot(data = weather_all, aes(x = date_beg)) + 
  geom_smooth(aes(y = MO_SD_S, color = "MO_SD_S"), method = "auto", se = FALSE) +
  labs(title = "Sunshine Hours Development", x = "Year", y = "Sunshine [h]", color = "Measures:") +
  scale_color_manual(labels = c("Average Sunshine Hours"), values = c("yellow")) +
  expand_limits(y = 0)

#cloud coverage
ggplot(data = weather_all, aes(x = date_beg)) + 
  geom_smooth(aes(y = MO_N, color = "MO_N"), method = "auto", se = FALSE) +
  labs(title = "Cloud Coverage Development", x = "Year", y = "Cloud Coverage", color = "Measures:") +
  scale_color_manual(labels = c("Average Clud Coverage"), values = c("orange")) +
  expand_limits(y = 0)


## Analysis of the actual period of time
#rainfall amount in mm
ggplot(data = tourism, aes(x = date_beg, y = MO_RR)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Rainfall development") +
  xlab("Time") + 
  ylab("Rainfall in mm")

#sum of sunshine in hours
ggplot(data = tourism, aes(x = date_beg, y = MO_SD_S)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Sunshine development") +
  xlab("Time") + 
  ylab("Sunshine in hours")


## categorize days as rainy, sunny, cloudy, windy, hot
#rainy days (rainfall amount above 3mm a day)
hist(weather_daily$RSK[weather_daily$RSK > 1])
weather_daily$rainy = sapply(weather_daily$RSK, FUN = function(x) ifelse(x > 3, 1, 0))

#count total number of rainy days in time period
sum(weather_daily$rainy)


#sunny days (sunshine hours above 8 hours a day)
hist(weather_daily$SDK)
weather_daily$sunny = sapply(weather_daily$SDK, FUN = function(x) ifelse(x > 8, 1, 0))

#count total number of sunny days in time period
sum(weather_daily$sunny)


#cloudy days (cloud coverage above 7.75 on okta scale)
hist(weather_daily$NM)
weather_daily$cloudy = sapply(weather_daily$NM, FUN = function(x) ifelse(x > 7.75, 1, 0))

#count total number of cloudy days in time period
sum(weather_daily$cloudy)


#windy days (wind speed above 4 m/s)
hist(weather_daily$FM); mean(weather_daily$FM)
weather_daily$windy = sapply(weather_daily$FM, FUN = function(x) ifelse(x > 4, 1, 0))

#count total number of windy days in time period
sum(weather_daily$windy)


#hot days (maximum temperature above 28 °C)
hist(weather_daily$TXK); mean(weather_daily$TXK)
weather_daily$hot = sapply(weather_daily$TXK, FUN = function(x) ifelse(x > 28, 1, 0))

#count total number of hot days in time period
sum(weather_daily$hot)


#aggregate weather categories according to month-year category
library(dplyr)
weather_days = weather_daily[, c(12:17)]
days = data.frame(weather_days %>% group_by(date_id) %>% summarize_all(sum))

days$month = substr(days$date_id, 1, 2)
days$year = substr(days$date_id, 4, 7)

days = days %>% arrange(year, month)

#adding day counts of weather categories to respective observation in tourism data frame
tourism$rainy = days$rainy
tourism$sunny = days$sunny
tourism$cloudy = days$cloudy
tourism$windy = days$windy
tourism$hot = days$hot

#classify month as good, bad and normal for weather conditions
#create dataframe with monthly deviations of means for respective month
weather_num = data.frame(tourism[4:16])
weather_means = data.frame(month_means[3:15])

colnames(weather_num)
colnames(weather_means)

weather_means = do.call("rbind", replicate(8, weather_means, simplify = FALSE))

deviation = weather_num - weather_means

#normalize deviation data frame (Z-Transformation)
norm_deviation = data.frame(scale(deviation))

#divide into "positive" and "negative" weather factors and calculate row-wise sums of both data frames
positive_dev = data.frame(norm_deviation[, c(3, 4, 5, 10)], "pos_mean" = rowSums(norm_deviation[, c(3, 4, 5, 10)])/4)
negative_dev = data.frame(norm_deviation[, c(2, 6, 7, 8, 9, 12, 13)], "neg_mean" = rowSums(norm_deviation[, c(2, 6, 7, 8, 9, 12, 13)])/7)

#compare sums of "positive" and "negative" weather factors
weather_means = data.frame("pos_mean" = positive_dev$pos_mean, "neg_mean" = negative_dev$neg_mean)
weather_means$difference = weather_means$pos_mean - weather_means$neg_mean

check_weather_class = function(x){  
  if(x >= 1){
    "very good"
  }else if(x < 1 && x >= 0.25){
    "good"
  }else if(x < 0.25 && x >= -0.25){
    "normal"
  }else if(x < -0.25 && x >= -1){
    "bad"
  }else{
    "very bad"
  }
}

weather_means$weather_class = sapply(weather_means$difference, FUN = check_weather_class)

#add weather_class to tourism data frame
tourism$weather_class = factor(weather_means$weather_class, levels = c("very bad", "bad", "normal", "good", "very good"))

#add numeric values for weather_class to tourism data frame
tourism$weather_class_num = weather_means$difference

#add season of the year to tourism data frame
season_year = c(rep("winter", 2), rep("spring", 3), rep("summer", 3), rep("autumn", 3), "winter")

tourism$season_year = factor(season_year)


#try to reduce number of weather variables using factor analysis
#prepare seperate data frame for factor analysis
fa = data.frame(weather_num[, -1])
fa$QN_6 = NULL

#normalize data frame
fa_n = scale(fa)
head(fa_n)

#create correlation matrix for numeric variables
fa_cor = round(cor(fa_n), 3)

#visualize correlations
library(corrplot)
library(RColorBrewer)
corrplot(fa_cor, method="circle", type = "upper", col = brewer.pal(n=8, name="RdYlBu"))

#create screeplot to specify numbers of factors
plot(eigen(cor(fa[, 3:8]))$values, 
     type = "o", 
     col = "blue",
     pch = 16,
     cex = 2,
     #main = "Screeplot",
     xlab = "Number of Factors",
     ylab = "Eigenvalues",
     lwd = 2)
abline(h = 1, lwd = 2, col = "red")

#extract factors
#install.packages("psych")
library(psych)
rotated_factor <- fa(r = fa, 
                     nfactors = 2, 
                     fm = "pa", 
                     rotate = "varimax", 
                     scores = "regression",
                     min.err = 0.002)
print(rotated_factor, cut = 0, digits = 3) 

#visualize factors
factor.plot(rotated_factor, labels=rownames(rotated_factor$loadings))

fa.diagram(rotated_factor, simple = FALSE, cut = 0, digits = 3, errors = TRUE)

#factor loadings on respective factors
L <- unclass(rotated_factor$loadings)
round(L, 3)

#compute squared factor loadings
L2 <- L^2
round(L2, 3)

#communalities
h2 <- rowSums(L2)
round(h2, 3)

#uniqueness
u2 <- 1 - h2
round(u2, 3)

#eigenvalues
Eig <- colSums(L2)
round(Eig, 3)

#variance explanation
sum(h2) # explanated variance
sum(u2) # not explanated variance
sum(h2) / ncol(fa) # part of explanated variance
round(Eig / ncol(fa), 3) # part of explanated variance for each factor

#check model fit (based on original correlation matrix)
#original correlation matrix
round(fa_cor, 3)

#estimated correlation matrix (Rhat = L*L')
Rhat <- tcrossprod(L)
round(Rhat, 3)

#residuals matrix (R - Rhat)
round(fa_cor - Rhat, 3)

#merge temerature variables to factor "temperature"
#extract and normalize temperature variables (paper)
temp = data.frame(scale(weather_num[,c(3:5, 7, 9)]))
temp$mean_norm_temp = rowSums(temp)/5

#exchange temperature variables in tourism with new calculated factor for temperature
tourism[,c(6:8, 10, 12)] = NULL
tourism$mean_norm_temp = temp$mean_norm_temp





############################################################
#Analysis of Tourism
############################################################

# set the working directory
wd = "C:/Users/timpe_000/Desktop/SPL Projekt"
setwd(wd)

#analysis tourism data

#show distribution of data
ggplot(tourism, aes(x = guest_count)) + 
  geom_histogram(aes(y=..density..), binwidth = 2500, colour="black", fill="white") +
  geom_density(alpha=.15, fill="#31B404") +
  ggtitle("Density function and Histogramm of the number of guests over the observed time period") +
  xlab("Guests") + 
  ylab("Density function and Histogramm")

ggplot(tourism, aes(x = night_count)) + 
  geom_histogram(aes(y=..density..), binwidth = 5000, colour="black", fill="white") +
  geom_density(alpha=.15, fill="#31B404") +
  ggtitle("Density function and Histogramm of the number of overnight stays over the observed time period") +
  xlab("Overnight stays") + 
  ylab("Density function and Histogramm")

#show development of guest and overnight stays
ggplot(data = tourism, aes(x = date_beg, y = guest_count)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the number of guests stays for the observed time period") +
  xlab("Guests") + 
  ylab("Number of guests") +
  expand_limits(y = 0)

ggplot(data = tourism, aes(x = date_beg, y = night_count)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the number of overnight stays for the observed time period") +
  xlab("Overnight stays") + 
  ylab("Number of overnight stays") +
  expand_limits(y = 0)

#adding average number of days spent in region (overnight stays / guests)
tourism$avg_time = tourism$night_count/tourism$guest_count

#creating overall monthly averages for tourism data
#install.packages("data.table")
library(data.table)
tourism_tab = data.table(tourism)
monthly = data.frame(tourism_tab[,list(mean_guest = mean(guest_count),sd_guest = sd(guest_count), mean_nights = mean(night_count),sd_nights = sd(night_count)),by=tourism$month_name])

colnames(monthly)[1] = "Month"
monthly

#comparing average guests per month to find out seasonal month
ggplot(data = monthly, aes(x = Month, y = mean_guest)) +
  geom_bar(stat="identity") +
  ggtitle("Barplot of monthly guests") +
  xlab("Month") + 
  ylab("Guests")

ggplot(data = monthly, aes(x = Month, y = mean_nights)) +
  geom_bar(stat="identity") +
  ggtitle("Barplot of monthly overnight stays") +
  xlab("Month") + 
  ylab("Overnight stays")

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

tourism$season = sapply(as.list(tourism$month_name), season_check, season_month)
tourism$season = factor(tourism$season)

#comparing numbers in season and no season
mean(tourism$guest_count[tourism$season == "Yes"])
mean(tourism$guest_count[tourism$season == "No"])

#t-test -> assumes normal distribution -> not fulfilled (see density function)
t.test(tourism$guest_count~tourism$season)

#average number of nights spent per guest
ggplot(data = tourism, aes(x = date_beg, y = avg_time)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the average number of nights spent per guest") +
  xlab("Time") + 
  ylab("Average number of nights")

#relationship between season and average nights per guest
cor(as.numeric(tourism$season), tourism$avg_time)

season_rows = which(tourism$season == "Yes")

mean(tourism[season_rows, "avg_time"])
mean(tourism[-season_rows, "avg_time"])


#calculate the deveation of guests and overnigth stays from monthly mean as additional target variable
tourism$abs_guest_deviation = tourism$guest_count - c(replicate(8, month_means$mean_guest_count))
tourism$abs_night_deviation = tourism$night_count - c(replicate(8, month_means$mean_night_count))

#find out relative deviation in percent
tourism$rel_guest_deviation = tourism$abs_guest_deviation/tourism$guest_count
tourism$rel_night_deviation = tourism$abs_night_deviation/tourism$night_count


#calculate annual growth of guests and nights compared to previous year
obs = data.frame("year" = format(tourism$date_beg, format = "%Y"))
obs$guest_count = tourism$guest_count
obs$night_count = tourism$night_count

library(dplyr)
years = obs %>%
  group_by(year) %>%
  summarize_all(sum)

years$prev_guest = c(0, years$guest_count[1:7])
years$prev_night = c(0, years$night_count[1:7])

years$guest_growth = (years$guest_count/years$prev_guest) -1
years$night_growth = (years$night_count/years$prev_night) -1

#years = as.data.frame(years[-1, ])
#years[, 2:5] = NULL


#plot growth rates on year to year basis
library(ggplot2)

ggplot(years[2:8, ], aes(x = c(2011:2017))) + 
  geom_line(aes(y = guest_growth, color = "blue")) +
  geom_line(aes(y = night_growth, color = "red")) +
  ggtitle("Development of growthrates for guests and overnight stays") +
  xlab("Year") + 
  ylab("Growth rate") +
  scale_colour_manual(labels = c("Guest", "Night"), values = c("blue", "red")) +
  geom_point(aes(y = guest_growth, color = "blue")) +
  geom_point(aes(y = night_growth, color = "red"))


#problem: "natural growth" -> observations at beginning of the observation_period are smaller than at the end
#try to calculate out growth rate to have an equal basis for comparison

#calculate annual growth of guests and nights compared to basis year = 2010
years_index = obs %>%
  group_by(year) %>%
  summarize_all(sum)

years_index$guest_basis = years$guest_count[1]
years_index$night_basis = years$night_count[1]

years_index$guest_growth = (years_index$guest_count/years_index$guest_basis)-1
years_index$night_growth = (years_index$night_count/years_index$night_basis)-1

#plot growth rates compared to basis year
library(ggplot2)

ggplot(years_index, aes(x = c(2010:2017))) + 
  geom_line(aes(y = guest_growth, color = "blue")) +
  geom_line(aes(y = night_growth, color = "red")) +
  ggtitle("Development of growthrates for guests and overnight stays") +
  xlab("Year") + 
  ylab("Growth rate") +
  scale_colour_manual(labels = c("Guest", "Night"), values = c("blue", "red")) +
  geom_point(aes(y = guest_growth, color = "blue")) +
  geom_point(aes(y = night_growth, color = "red"))


#recalculate guests and nights "without" growth (in values of basis year 2010)
tourism$guest_basis = 0

j = 2010
for(i in c(1:8)){
  tourism[which(as.numeric(format(tourism$date_beg, format = "%Y")) == j), ]$guest_basis = tourism$guest_count[which(as.numeric(format(tourism$date_beg, format = "%Y")) == j)]/(1+years_index$guest_growth[i])
  j = j + 1
}

#for nights
tourism$night_basis = 0

j = 2010
for(i in c(1:8)){
  tourism[which(as.numeric(format(tourism$date_beg, format = "%Y")) == j), ]$night_basis = tourism$night_count[which(as.numeric(format(tourism$date_beg, format = "%Y")) == j)]/(1+years_index$night_growth[i])
  j = j + 1
}


#check development
ggplot(data = tourism, aes(x = date_beg, y = guest_basis)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the number of guests stays for the observed time period") +
  xlab("Guests") + 
  ylab("Number of guests") +
  expand_limits(y = 0)

ggplot(data = tourism, aes(x = date_beg, y = night_basis)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the number of overnight stays for the observed time period") +
  xlab("Overnight stays") + 
  ylab("Number of overnight stays") +
  expand_limits(y = 0)


#calculate monthly means of newly generated basis variables
basis = data.frame("month" = rep(seq(1:12), 8))
basis$guest_basis = tourism$guest_basis
basis$night_basis = tourism$night_basis

library(data.table)
basis = data.table(basis)
mon_basis = data.frame(basis[,list(mean_guest_basis = mean(guest_basis),sd_guest_basis = sd(guest_basis), mean_nights_basis = mean(night_basis),sd_nights_basis = sd(night_basis)),by=basis$month])

#calculate the deveation of guests and overnigth stays from monthly mean of the basis observations
tourism$abs_guest_dev_basis = tourism$guest_basis - c(replicate(8, mon_basis$mean_guest_basis))
tourism$abs_night_dev_basis = tourism$night_basis - c(replicate(8, mon_basis$mean_nights_basis))

#find out relative deviation in percent from monthly mean of the basis observations
tourism$rel_guest_dev_basis = tourism$abs_guest_dev_basis/tourism$guest_basis
tourism$rel_night_dev_basis = tourism$abs_night_dev_basis/tourism$night_basis


#delete not relevant columns from tourism data frame
tourism$abs_guest_deviation = NULL
tourism$abs_night_deviation = NULL
tourism$rel_guest_deviation = NULL
tourism$rel_night_deviation = NULL





############################################################
#Build Regression Models
############################################################

#find interesting weather variables with relevance for tourism activities
str(tourism)
lr_data = tourism[, c(5:8, 10:11, 18:27, 29:30, 33:34)]
colnames(lr_data)

#change level labels for regression table
contrasts(lr_data$weather_class)

colnames(contrasts(lr_data$weather_class)) <- c("_bad", "_normal", "_good", "_very_good")
contrasts(lr_data$weather_class)

#numeric values
lr_data_num = lr_data[, unlist(lapply(lr_data, is.numeric))]

#create correlation matrix with numeric independen variables
cor_matrix = round(cor(lr_data_num), 3); cor_matrix

#visualize correlations
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor_matrix, method = "circle")

#plot several relationsships in scatterplot matrix for numeric independent variables variables
pairs(lr_data_num)

#run linear regression for 3 target variables (guests, overnight stays, average nights per guest)
#1 guest count
lm_guest_basis = lm(guest_basis ~ .-night_basis-avg_time-rel_guest_dev_basis-rel_night_dev_basis,data = lr_data)
summary(lm_guest_basis)

#2 number of overnight stays
lm_night_basis = lm(night_basis ~ .-guest_basis-avg_time-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
summary(lm_night_basis)

#3 average nights per guest
lm_avg_nights = lm(avg_time ~ .-guest_basis-night_basis-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data)
summary(lm_avg_nights)

#4 average nights per guest
lm_guest_dev = lm(rel_guest_dev_basis ~ .-guest_basis-night_basis-rel_night_dev_basis-avg_time, data = lr_data)
summary(lm_guest_dev)

#5 average nights per guest
lm_night_dev = lm(rel_night_dev_basis ~ .-guest_basis-night_basis-rel_guest_dev_basis-avg_time, data = lr_data)
summary(lm_night_dev)



#create data frame with all available variables for temperature
lr_data2 = data.frame(lr_data[, 1:20], weather[, c(6:8, 10, 12)])
str(lr_data2)
lr_data2$mean_norm_temp = NULL

#extract numeric data
lr_data2_num = lr_data2[, unlist(lapply(lr_data2, is.numeric))]

#compute correlation matrix
cor_matrix2 = round(cor(lr_data2_num), 3); cor_matrix2


#run linear regression with all temperature variables
#1 guest count
lm_guest_basis = lm(guest_basis ~ .-night_basis-avg_time-rel_guest_dev_basis-rel_night_dev_basis,data = lr_data2)
summary(lm_guest_basis)

#2 number of overnight stays
lm_night_basis = lm(night_basis ~ .-guest_basis-avg_time-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data2)
summary(lm_night_basis)

#3 average nights per guest
lm_avg_nights = lm(avg_time ~ .-guest_basis-night_basis-rel_guest_dev_basis-rel_night_dev_basis, data = lr_data2)
summary(lm_avg_nights)

#4 average nights per guest
lm_guest_dev = lm(rel_guest_dev_basis ~ .-guest_basis-night_basis-rel_night_dev_basis-avg_time, data = lr_data2)
summary(lm_guest_dev)

#5 average nights per guest
lm_night_dev = lm(rel_night_dev_basis ~ .-guest_basis-night_basis-rel_guest_dev_basis-avg_time, data = lr_data2)
summary(lm_night_dev)


## export of selected regression tables
#sink("output.txt")
#summary(lm_guest_dev)
#sink()
