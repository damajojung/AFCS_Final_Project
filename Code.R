
library(fpp3)
library(tidyverse)
library(dplyr)

### Load Data
cal = read.csv("calendar.csv", header = T)
stval = read.csv("sales_train_validation.csv", header = T)
samp_sub = read.csv("sample_submission.csv", header = T)
price = read.csv("sell_prices.csv", header = T)
steval = read.csv("sales_train_evaluation.csv", header = T)

# Pivot longer
stval_long = pivot_longer(stval, cols =  7:1919, names_to = "d", values_to = "sales")

# Extract time information
dates_df = as_tibble(cal[c("date", "d")])

# Merge the two data frames
merge_df = merge(dates_df, stval_long, by = "d")

# Change the date from character to date format
merge_df["date"] = as.Date(merge_df$date, format = "%Y-%m-%d")

# Convert Data frame to tsibble
merge_ts = merge_df %>%  as_tsibble(index = "date", key = c("d", "id", "item_id")) # does not work yet - memory issues 

# Plot
merge_df %>% autoplot(sales)


#######
# LOAD merge_df
#######

load("Data_First_Steps.RData") # Load everything

























