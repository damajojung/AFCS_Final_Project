
library(fpp3)
library(tidyverse)
library(dplyr)


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

# Convert Data frame to tsibble
merge_ts = merge_df %>%  as_tsibble(index = "date", key = c("d", "id")) # does not work yet - memory issues 


#######
# LOAD merge_df
#######

load("Data_First_Steps.RData") # Load everything

























