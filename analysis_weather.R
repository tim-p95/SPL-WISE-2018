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
