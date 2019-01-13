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
