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


#export of selected regression tables
sink("output.txt")
summary(lm_guest_dev)
sink()
