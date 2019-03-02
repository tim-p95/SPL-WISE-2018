# Name of Quantlet: SPL_WeatherTourism_01preparation
# Published in:     'Statistical programming languages - Student Project on ''Impact of Meteorological Factors on Regional Tourism'' '
# Description:      'Loading packages, Import and first preparation of data, Description of weather variables'
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

#delete former date variables
weather_all$MESS_DATUM_BEGINN = NULL
weather_all$MESS_DATUM_ENDE   = NULL

#get relevant time period of the weather data to match tourism observations
weather           = subset(weather_all, as.numeric(format(date_beg, format="%Y")) >= 2010)
rownames(weather) = seq(1:nrow(weather))

#Observations for tourism start in January 2010 until December 2017 (96 observations) and the
#weather data has to be adapted to this time frame.


### import daily weather data ###
#read daily weather data and transform it into a data frame
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



# Name of Quantlet: SPL_WeatherTourism_02weatherAnalysis
# Published in:     'Statistical programming languages - Student Project on ''Impact of Meteorological Factors on Regional Tourism'' '
# Description:      'Analysis and transformation of the daily and monthly weather observations, Categorization of days, Visualization of weather developments'
# Keywords:         meteorological analysis, categorization, transformation, visualization, missing data
# Author:           Tim Peschenz
# Submitted:        So, Mar 17 2019


############################################################
## Analysis of Weather
############################################################

### Handle Missing Data for monthly weather data ###
#count column-wise NAs
na_count = sapply(tourism, function(x){sum(is.na(x))})

#extracting relevant columns
na_count[which(na_count != 0)]

## calculate month-wise means for all potentially relevant tourism and weather variables
#extract numeric columns
numeric_cols             = unlist(lapply(tourism, is.numeric)) 
tourism_num              = data.frame("month" = seq(1:12), tourism[ , numeric_cols])
tourism_num[c(2, 3, 13)] = NULL

#month-wise aggregation by calculating the mean
mon = tourism_num %>%
  group_by(month) %>%
  summarise_all(funs(mean), na.rm = TRUE)

month_means = data.frame(mon)

#change column names of month-wise observations by adding '_mean' label to the name
colnames(month_means) = paste("mean_", colnames(tourism_num), sep = "")

#adding month names (January - December)
month_means = data.frame("month_name" = month.name, month_means)

#exchanging the missing values with the mean of the particular month from month_means data frame
tourism$MO_N[is.na(tourism$MO_N)]   = 
  month_means$mean_MO_N[as.numeric(format(tourism$date_beg, format="%m"))[is.na(tourism$MO_N)]]
tourism$MO_FK[is.na(tourism$MO_FK)] = 
  month_means$mean_MO_FK[as.numeric(format(tourism$date_beg, format="%m"))[is.na(tourism$MO_FK)]]
tourism$MO_RR[is.na(tourism$MO_RR)] = 
  month_means$mean_MO_RR[as.numeric(format(tourism$date_beg, format="%m"))[is.na(tourism$MO_RR)]]


### check for missing values in daily weather data ###
na_count = sapply(weather_daily, function(x){sum(is.na(x))})

#extracting relevant columns
na_count[which(na_count != 0)]

#replace missing values with mean value 
weather_daily$FX[is.na(weather_daily$FX)] = mean(weather_daily$FX, na.rm = TRUE)
weather_daily$FM[is.na(weather_daily$FM)] = mean(weather_daily$FM, na.rm = TRUE)
weather_daily$NM[is.na(weather_daily$NM)] = mean(weather_daily$NM, na.rm = TRUE)

#Since there are no highly season-specific variables missing like temperature, replacing the  
#missing values with the overall mean is appropriate in this case.


### long time development of weather for the whole available period of time ###
#temperature development (1887 - 2017)
ggplot(data = weather_all, aes(x = date_beg)) + 
  geom_smooth(aes(y = MO_TT, color = "MO_TT"), method = "auto", se = FALSE) +
  geom_smooth(aes(y = MO_TX, color = "MO_TX"), method = "auto", se = FALSE) +
  geom_smooth(aes(y = MO_TN, color = "MO_TN"), method = "auto", se = FALSE) +
  labs(title = "Long-run Temperature Development", x = "Year", y = "Temperature [°C]", color = "Measures:") +
  scale_color_manual(labels = c("Minimum Temperature", "Average Temperature", "Maximum Temperature"), values = c("blue", "green", "red")) +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.border         = element_blank(), 
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank(), 
        axis.line            = element_line(colour = "black"),
        legend.justification = c(1,0), 
        legend.position      = c(1,0))



### categorize days as rainy, sunny, cloudy, windy, hot ###
#rainy days (rainfall amount above 1mm a day)
weather_daily$rainy = sapply(weather_daily$RSK, FUN = function(x) ifelse(x > 1, 1, 0))

#count total number of rainy days in time period
sum(weather_daily$rainy)

#percentage of rainy days
sum(weather_daily$rainy)/(nrow(weather_daily))


#sunny days (sunshine hours above 10 hours a day)
weather_daily$sunny = sapply(weather_daily$SDK, FUN = function(x) ifelse(x > 10, 1, 0))

#count total number of sunny days in time period
sum(weather_daily$sunny)

#percentage of sunny days 
sum(weather_daily$sunny)/(nrow(weather_daily))


#cloudy days (cloud coverage above 7.75 on okta scale)
weather_daily$cloudy = sapply(weather_daily$NM, FUN = function(x) ifelse(x > 7.75, 1, 0))

#count total number of cloudy days in time period
sum(weather_daily$cloudy)

#percentage of cloudy days
sum(weather_daily$cloudy)/(nrow(weather_daily))


#windy days (wind speed above 6 m/s)
weather_daily$windy = sapply(weather_daily$FX, FUN = function(x) ifelse(x > 6, 1, 0))

#count total number of windy days in time period
sum(weather_daily$windy)

#percentage of windy days
sum(weather_daily$windy)/(nrow(weather_daily))


#hot days (maximum temperature above 27 °C)
weather_daily$hot = sapply(weather_daily$TXK, FUN = function(x) ifelse(x > 27, 1, 0))

#count total number of hot days in time period
sum(weather_daily$hot)

#percentage of hot days
sum(weather_daily$hot)/(nrow(weather_daily))


#Due to the design of the categorization, it is possible that a specific day can belong to none, one or 
#several of the different weather categories.


### aggregate weather categories according to month-year combination to create day counts per month ###
weather_days = weather_daily[, c(12:17)]
days         = data.frame(weather_days %>% group_by(date_id) %>% summarize_all(sum))

#arrange observations in correct time order
days$month = substr(days$date_id, 1, 2)
days$year  = substr(days$date_id, 4, 7)

days = days %>% arrange(year, month)

#adding day counts of weather categories to respective observation in tourism data frame
tourism$rainy  = days$rainy
tourism$sunny  = days$sunny
tourism$cloudy = days$cloudy
tourism$windy  = days$windy
tourism$hot    = days$hot


### create data frame with monthly weather deviations ###
weather_num   = data.frame(tourism[, c(3:11, 13, 14)])
weather_means = data.frame(month_means[3:13])

#compare variable names
data.frame(colnames(weather_num), colnames(weather_means))

#repeat means for whole observation period
weather_means = do.call("rbind", replicate(8, weather_means, simplify = FALSE))

#calculate deviations by subtracting the actual observations from the corresponding month-wise mean values
weather_deviation = weather_num - weather_means


### check for possible reductions of weather variables using factor analysis ###
#prepare data frame for factor analysis
fa = data.frame(weather_num)

#normalize data frame
fa_n = scale(fa)

#create correlation matrix for numeric variables
fa_cor = round(cor(fa_n), 3)

#The different temperature (MO_T...) measures are highly correlated with each other. The sun hours (MO_SD_S)
#are also highly positively correlated with the temperature measures. Sun hours are also negatively 
#correlated with the degree of cloud coverage (MO_N).

#visualize correlations
corrplot(fa_cor, method="circle", type = "upper", col = brewer.pal(n=8, name="RdYlBu"))

#create screeplot to specify numbers of factors
plot(eigen(cor(fa[, 3:8]))$values, type = "o", col = "blue", pch = 16, 
     cex = 2, xlab = "Number of Factors", ylab = "Eigenvalues", lwd = 2)
abline(h = 1, lwd = 2, col = "red")

#The optimal number of factors for this analysis is a two factor solution, because the two factor
#solution delivers an eigenvalue score which is still above 1. Using the "elbow" technique to decide for 
#the number of factors, a 1 factor solution would be more appropriate.

#extract factors
rotated_factor = fa(r = fa, nfactors = 2, fm = "pa", rotate = "varimax", 
                    scores = "regression", min.err = 0.002)
print(rotated_factor, cut = 0, digits = 3) 

#visualize factors
factor.plot(rotated_factor, labels=rownames(rotated_factor$loadings))
fa.diagram(rotated_factor, simple = FALSE, cut = 0, digits = 3, errors = TRUE)

#As expected beforehand, the different temperature variables load heavily on the same factor. Therefore 
#it is sensible to reduce them to one variable representing the temperature, i.e. average temperature.
#Sunhours are left as an individual column.


## use average temperature as single temperature variable (deleting other temperature columns)
tourism[, c(5:6, 8, 10)] = NULL



# Name of Quantlet:  SPL_WeatherTourism_03tourismAnalysis
# Published in:      'Statistical programming languages - Student Project on ''Impact of Meteorological Factors on Regional Tourism'' '
# Description:       'Analysis and transformation of the monthly observations of guests and nights, Creation of target variables for regression analysis, Visualization of tourism developments'
# Keywords:          tourism analysis, transformation, visualization, indexing, relative deviations
# Author:            Tim Peschenz
# Submitted:         So, Mar 17 2019


############################################################
## Analysis of Tourism
############################################################

### checking for outliers in guests and nights ###
ggplot(tourism, aes(y = guest_count, x = month_name, fill=month_name)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  ggtitle("Boxplots of the Number of Guest in Different Months Respectively") +
  xlab("Month") + 
  ylab("Guests") +
  theme_bw() +
  theme(panel.border         = element_blank(), 
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank(), 
        axis.line            = element_line(colour = "black"),
        legend.justification = c(1,1), 
        legend.position      = c(1,1))

ggplot(tourism, aes(y = night_count, x = month_name, fill=month_name)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  ggtitle("Boxplots of the Number of Overnight Stays in Different Months Respectively") +
  xlab("Month") + 
  ylab("Overnight Stays") +
  theme_bw() +
  theme(panel.border         = element_blank(), 
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank(), 
        axis.line            = element_line(colour = "black"),
        legend.justification = c(1,1), 
        legend.position      = c(1,1))

#It can be seen that there is one outlier in April, represented by the red circle in both, the number of
#guests and the number of nights.


### extracting the outlier in April ###
april      = subset(tourism, month_name == "April")
april_outl = which(april$guest_count > mean(april$guest_count) + 2*sd(april$guest_count))
april[april_outl, ]

#The outlier was in April 2017. Since the values are not unrealistic, the data point was not changed.


### calculate correlations between number of guests and nights ###
cor(tourism$guest_count, tourism$night_count)

#There is a high correlation between the number of guests and nights. Therefore, the following
#visualizations are only made for the number of guests, since the graphs would look quite similar
#for the number of nights, just with up-scaled numbers.


### visualize distribution of guests counts ###
ggplot(tourism, aes(x = guest_count)) + 
  geom_histogram(aes(y=..density..), binwidth = 2500, colour="black", fill="white") +
  geom_density(alpha=.15, fill="#31B404") +
  ggtitle("Density Function and Histogramm of the Number of Guests Over the Observed Time Period") +
  xlab("Guests") + 
  ylab("Density Function and Histogramm") +
  theme_bw() +
  theme(panel.border     = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line        = element_line(colour = "black"))

#There are two "peaks" in the number of guests. The first represents the no-season time and the second 
#one the tourism season period in the region.


### visualize development of guests ###
ggplot(data = tourism, aes(x = date_beg, y = guest_count)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the Number of Guests") +
  xlab("Guests") + 
  ylab("Number of Guests") +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.border        = element_blank(), 
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank(), 
        axis.line            = element_line(colour = "black"),
        legend.justification = c(1,0), 
        legend.position      = c(1,0))

#The seasonal developments are represented by the red line. We can see that there is a high seasonality
#between winter and summer. Also, there is an overall upward trend, represented by the blue line.


### adding average number of days per guest spent in region (overnight stays / guests) ###
tourism$avg_time = tourism$night_count/tourism$guest_count


### creating overall monthly averages and standard deviations for tourism data ###
tourism_tab = data.table(tourism)
monthly     = data.frame(tourism_tab[,list(mean_guest  = mean(guest_count),
                                           sd_guest    = sd(guest_count), 
                                           mean_nights = mean(night_count),
                                           sd_nights   = sd(night_count)),
                                     by = tourism$month_name])

colnames(monthly)[1] = "Month"

#Having now the month-wise means, it is possible to calculate the deviations of tourism flows for the 
#individual data points.


### comparing average guests per month to find out seasonal month ###
ggplot(data = monthly, aes(x = Month, y = mean_guest)) +
  geom_bar(stat="identity") +
  ggtitle("Average Number of Guests in Corresponding Month") +
  xlab("Month") + 
  ylab("Guests") +
  theme_bw() +
  theme(panel.border     = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line        = element_line(colour = "black"))

#We can see a similar shape than in the boxplots beforehand. Again, the seasonal changes can be observed,
#with a clear focus in the spring and summer time and the beginning of autumn.


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

#According to the above definition, the tourism season in the region is from May until October.


#comparing numbers in season and no season
mean(tourism$guest_count[tourism$season == "Yes"])
mean(tourism$guest_count[tourism$season == "No"])

#The mean value of tourists per month is more than double in the seasonal month compared to the 
#non-season period.


### development of average number of nights per guest ###
ggplot(data = tourism, aes(x = date_beg, y = avg_time)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the average number of nights spent per guest") +
  xlab("Time") + 
  ylab("Average number of nights") +
  theme_bw() +
  theme(panel.border     = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line        = element_line(colour = "black"))

#For the average number of nights per guest, we can observe a much smaller seasonality than 
#for the absolute numbers of guests and nights. Also, there is a slight downward tendency, represented by 
#the blue line again. This means that the average duration of stay decreased during the observation period.


### calculate the absolute deviation of guests and overnight stays from monthly mean ###
tourism$abs_guest_deviation = 
  tourism$guest_count - c(replicate(8, month_means$mean_guest_count))
tourism$abs_night_deviation = 
  tourism$night_count - c(replicate(8, month_means$mean_night_count))

plot(tourism$abs_guest_deviation)

#We can observe a systematic upward trend of the deviations, which is caused by the overall growth in
#the number of guests and nights.


### calculate relative deviation in percent ###
tourism$rel_guest_deviation = tourism$abs_guest_deviation/tourism$guest_count
tourism$rel_night_deviation = tourism$abs_night_deviation/tourism$night_count

plot(tourism$rel_guest_deviation)

#We can observe the same systematic upward trend for the relative deviation with the same reasoning. 
#Therefore it is necessary to recalculate the number of guests and nights without the growth 
#developments to use them in the analysis.


### calculate annual growth of guests and nights compared to basis year = 2010 ###
obs             = data.frame("year" = format(tourism$date_beg, format = "%Y"))
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
  ggtitle("Development of Growth-rates for Guests and Overnight Stays") +
  xlab("Year") + 
  ylab("Growth rate") +
  scale_colour_manual(labels = c("Guests", "Nights"), values = c("blue", "red")) +
  geom_point(aes(y = guest_growth, color = "blue")) +
  geom_point(aes(y = night_growth, color = "red")) +
  theme_bw() +
  theme(panel.border         = element_blank(), 
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank(), 
        axis.line            = element_line(colour = "black"),
        legend.justification = c(1,0), 
        legend.position      = c(1,0),
        legend.title         = element_blank())

#The growth development is quite strong. Between 2010 and 2017, there was an increase of 41,2% for guests
#and 36,7% for the number of nights per year.


### recalculate guests and nights "without" growth (in values of basis year 2010) ###
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


### check adjusted development ###
ggplot(data = tourism, aes(x = date_beg, y = guest_basis)) + 
  geom_point() +
  stat_smooth(span=1/12, col = "#ff4d4d", se = FALSE) +
  geom_smooth(col = "#99ccff", method = "lm", se = FALSE) +
  ggtitle("Development of the number of guests stays for the observed time period") +
  xlab("Guests") + 
  ylab("Number of guests") +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.border     = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line        = element_line(colour = "black"))

#By using the indexed versions of the tourism measures, we can now see that there is no overall 
#upward trend anymore. Therefore, calculating the deviations from the indexed values has no 
#systematic trend any more. The deviations can now be used for the analysis.


### calculate monthly means of newly generated indexed basis variables ###
basis             = data.frame("month" = rep(seq(1:12), 8))
basis$guest_basis = tourism$guest_basis
basis$night_basis = tourism$night_basis

basis     = data.table(basis)
mon_basis = data.frame(basis[,list(mean_guest_basis  = mean(guest_basis),
                                   sd_guest_basis    = sd(guest_basis), 
                                   mean_nights_basis = mean(night_basis),
                                   sd_nights_basis   = sd(night_basis)),
                             by=basis$month])


### calculate the deveation of guests and overnight stays from monthly mean of the basis observations ###
tourism$abs_guest_dev_basis = tourism$guest_basis - c(replicate(8, mon_basis$mean_guest_basis))
tourism$abs_night_dev_basis = tourism$night_basis - c(replicate(8, mon_basis$mean_nights_basis))

#The indexed absolute deviation helps to decrease the impact of the seasonality on the analysis. But there are
#still differences between the numbers in season and non-season.

plot(tourism$abs_guest_dev_basis)

#Now there is no systematic trend in the data any more.


### find out relative deviation in percent from monthly mean of the basis observations ###
tourism$rel_guest_dev_basis = tourism$abs_guest_dev_basis/tourism$guest_basis
tourism$rel_night_dev_basis = tourism$abs_night_dev_basis/tourism$night_basis

#The indexed relative deviation now helps to create an equal basis for all deviations. The deviations are now 
#aligned with the tourism values of the respective months.

plot(tourism$rel_guest_dev_basis)

#The same is true for the relative night deviations.


#delete not relevant columns from tourism data frame
tourism$abs_guest_deviation = NULL
tourism$abs_night_deviation = NULL
tourism$rel_guest_deviation = NULL
tourism$rel_night_deviation = NULL



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
