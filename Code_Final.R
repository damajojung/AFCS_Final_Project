
# Final Code




library(fpp2)
library(fpp3)
library(tidyverse)
library(dplyr)
library(tools)
library(gridExtra)

load("Data_First_Steps.rdata")

########## Data load and preprocessing

### Load Data
cal = read.csv("calendar_afcs2021.csv", header = T)
train = read.csv("sales_train_validation_afcs2021.csv", header = T)
test = read.csv("sales_test_validation_afcs2021.csv", header = T)
samp_sub = read.csv("sample_submission_afcs2021.csv", header = T)
price = read.csv("sell_prices_afcs2021.csv", header = T)

data = merge(train, test, by = "id")


# Pivot longer
train_long = pivot_longer(train, cols =  2:1914, names_to = "d", values_to = "sales")
test_long = pivot_longer(test, cols =  2:29, names_to = "d", values_to = "sales")
data_long = pivot_longer(data, cols =  2:1942, names_to = "d", values_to = "sales")

# Extract time information
dates_df = as_tibble(cal[c("date", "d")])

# Merge the two data frames
merge_df = merge(dates_df, train_long, by = "d") # Natural inner join
merge_df1 = merge(dates_df, test_long, by = "d") # Natural inner join
merge_df2= merge(dates_df, data_long, by = "d") # Natural inner join

# Change the date from character to date format
merge_df["date"] = as.Date(merge_df$date, format = "%m/%d/%Y")
merge_df1["date"] = as.Date(merge_df1$date, format = "%m/%d/%Y")
merge_df2["date"] = as.Date(merge_df2$date, format = "%m/%d/%Y")

merge_df <- merge_df[order(merge_df$date),] 
merge_df1 <- merge_df1[order(merge_df1$date),] 
merge_df2 <- merge_df2[order(merge_df2$date),] 

# Convert Data frame to tsibble
merge_ts = merge_df %>%  as_tsibble(index = "date", key = c('id')) 
merge_ts1 = merge_df1 %>%  as_tsibble(index = "date", key = c('id')) 
merge_ts2 = merge_df2 %>%  as_tsibble(index = "date", key = c('id')) 




#######################################################################################
################################ Prediction ##########################################
#######################################################################################





############################
## Predictions of whole data sets - TEST TRAIN
############################

# merge_ts = train
# merge_ts1 = test
# merge_ts2 = complete - needed for kaggle

fall_fit = merge_ts  %>% model('Seasonal_naive' = SNAIVE(sales))

fall_fc = fall_fit %>% forecast(h = '28 days')

acc = fall_fc %>% accuracy(merge_ts1) %>% select(.model, id, RMSE)

###
# Names
###

n = 1:28
names = c()
for (i in n){
  names[i] = paste("F", as.character(i), sep = "")
}

names = rep(names, length(unique(fall_fc$id)))

#########
# Pivot wider
#########


a = dplyr::bind_cols(fall_fc,names)
b = as_tibble(a)

final = b[, c("id", '.mean', "...6")]

summary(test)

###### Pivot wider


final_df = pivot_wider(final, names_from = '...6', values_from = '.mean')

write.csv(final_df,"/Users/dj/Desktop/kaggle_sub.csv", row.names = TRUE)


############################
## LIVE AMMUNITION FOR KAGGLE
############################

fall_fit = merge_ts2  %>% model('Seasonal_naive' = SNAIVE(sales))

fall_fc = fall_fit %>% forecast(h = '28 days')

# acc = fall_fc %>% accuracy(merge_ts1) %>% select(.model, id, RMSE)

###
# Names
###

n = 1:28
names = c()
for (i in n){
  names[i] = paste("F", as.character(i), sep = "")
}

names = rep(names, length(unique(fall_fc$id)))

#########
# Pivot wider
#########


a = dplyr::bind_cols(fall_fc,names)
b = as_tibble(a)

final = b[, c("id", '.mean', "...6")]


###### Pivot wider


final_df = pivot_wider(final, names_from = '...6', values_from = '.mean')

write.csv(final_df,"/Users/dj/Desktop/kaggle_sub.csv", row.names = TRUE)
