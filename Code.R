
library(fpp2)
library(fpp3)
library(tidyverse)
library(dplyr)
library(tools)
library(gridExtra)


### Load Data
cal = read.csv("calendar_afcs2021.csv", header = T)
train = read.csv("sales_train_validation_afcs2021.csv", header = T)
samp_sub = read.csv("sample_submission_afcs2021.csv", header = T)
price = read.csv("sell_prices_afcs2021.csv", header = T)
steval = read.csv("sales_test_validation_afcs2021.csv", header = T)

# Pivot longer
train_long = pivot_longer(train, cols =  2:1914, names_to = "d", values_to = "sales")

# Extract time information
dates_df = as_tibble(cal[c("date", "d")])

# Merge the two data frames
merge_df = merge(dates_df, train_long, by = "d")

# Change the date from character to date format
merge_df["date"] = as.Date(merge_df$date, format = "%m/%d/%Y")

merge_df <- merge_df[order(merge_df$date),] 

# Convert Data frame to tsibble
merge_ts = merge_df %>%  as_tsibble(index = "date", key = c('id')) 


#######
# LOAD merge_df
#######

load("Data_First_Steps.RData") # Load everything



#######
# Play around
#######

####################### TS

merge_ts %>% filter(id == 'FOODS_3_001_TX_3_validation') %>% select(date, sales) %>% autoplot(sales)

Pfood1 = merge_ts %>% filter(id == 'FOODS_3_001_TX_3_validation') %>% select(date, sales)

########## 
##Playing around with Naive Methods
########## 

f1_fit = food1  %>% model('Seasonal_naive' = SNAIVE(sales),
                                                           'Naive' = NAIVE(sales),
                                                           'Drift' = RW(sales ~ drift()),
                                                           'Mean' = MEAN(sales))

f1_fc = f1_fit %>% forecast(h = '28 days')


f1_fc %>% autoplot(food1, level = NULL) + labs(title = "Food one", y = "Sold units") + guides(colour = guide_legend(title = 'Forecast'))


########## 
## ACF plot
########## 

food1 %>% gg_tsdisplay(difference(sales), plot_type = "partial") # With one difference the mean becomes 0, but does not look like WN








