
# Final Code


library(fpp2)
library(fpp3)
library(tidyverse)
library(dplyr)
library(tools)
library(gridExtra)
library(ggplot2)

load("Data_First_Step.rdata")

########## Data load and preprocessing

### Load Data
cal = read.csv("calendar_afcs2021.csv", header = T)
train = read.csv("sales_train_validation_afcs2021.csv", header = T)
test = read.csv("sales_test_validation_afcs2021.csv", header = T)
samp_sub = read.csv("sample_submission_afcs2021.csv", header = T)
price = read.csv("sell_prices_afcs2021.csv", header = T)

data = merge(train, test, by = "id")

# write.csv(reduced,"/Users/dj/Desktop/reduced.csv", row.names = TRUE)


# Pivot longer
train_long = pivot_longer(train, cols =  2:1914, names_to = "d", values_to = "sales")
test_long = pivot_longer(test, cols =  2:29, names_to = "d", values_to = "sales")
data_long = pivot_longer(data, cols =  2:1942, names_to = "d", values_to = "sales")

# Extract time information
dates_df = as_tibble(cal[c("date", "d", 'weekday', 'event_name_1', 'event_type_1')])

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


m = c(1:3)
foods = c()
for (i in m){
  foods[i] = paste("FOODS_3_00", as.character(i) , "_TX_3_validation", sep = "")
}
foods

merge_ts[merge_ts["id" == foods,]]
str(merge_ts)

############################
## Predictions of whole data sets - TEST TRAIN
############################


######## One year

reduced = merge_ts %>% filter_index("2016-03-28" ~ "2016-04-24") # 2016-03-28" ~ "2016-04-24"
oneyear = merge_ts %>% filter_index("2015-03-28" ~ "2015-04-24") %>% select(sales, id)
names(oneyear)[names (oneyear) == "sales"] = "sales_1"

test1  = dplyr::bind_cols(reduced, oneyear$sales_1)
names(test1)[names(test1) == "...5"] = "sales_1"

df_pois = as_tibble(test1)

names = unique(df_pois$id)

subset = df_pois[df_pois$id == names[2],]

fit = glm(sales ~ sales_1, family = 'poisson', link = log, data = subset)

predict(fit)


######## Multiple Years

reduced = merge_ts %>% filter_index("2016-03-28" ~ "2016-04-24") # 2016-03-28" ~ "2016-04-24"
oneyear = merge_ts %>% filter_index("2015-03-28" ~ "2015-04-24") %>% select(sales, id)
twoyear = merge_ts %>% filter_index("2014-03-28" ~ "2014-04-24") %>% select(sales, id)
threeyear = merge_ts %>% filter_index("2013-03-28" ~ "2013-04-24") %>% select(sales, id)
foureyear = merge_ts %>% filter_index("2012-03-28" ~ "2012-04-24") %>% select(sales, id)

names(oneyear)[names (oneyear) == "sales"] = "sales_1"
names(twoyear)[names (twoyear) == "sales"] = "sales_2"
names(threeyear)[names (threeyear) == "sales"] = "sales_3"
names(foureyear)[names (foureyear) == "sales"] = "sales_4"

test1  = dplyr::bind_cols(reduced, oneyear$sales_1)
test1  = dplyr::bind_cols(test1, twoyear$sales_2)
test1  = dplyr::bind_cols(test1, threeyear$sales_3)
test1  = dplyr::bind_cols(test1, foureyear$sales_4)

names(test1)[names(test1) == "...5"] = "sales_1"
names(test1)[names(test1) == "...6"] = "sales_2"
names(test1)[names(test1) == "...7"] = "sales_3"
names(test1)[names(test1) == "...8"] = "sales_4"

# fall_fit = test1  %>% model(glm(sales ~ sales_y, family = 'poisson'))

df_pois = as_tibble(test1)

fit = glm(sales ~ sales_1 + sales_2 + sales_3 + sales_4, family = 'poisson', data = df_pois)

summary(fit)

fall_fc = fit %>% forecast(newdata= reduced[c('', "id")], h = '28 days')

acc = fall_fc %>% accuracy(merge_ts1) %>% select(.model, id, RMSE);acc

mean(acc$RMSE)

##################### As it used to be 

fall_fit = merge_ts %>% model(NAIVE(sales))

fall_fc = fall_fit %>% forecast(h = '28 days')

acc = fall_fc %>% accuracy(merge_ts1) %>% select(.model, id, RMSE);acc

mean(acc$RMSE)

# SNAIV = 2.321729
# Drift = 2.348751
# MEAN = 2.112849
# NAIVE = 2.340162
# Arima012 = 1.898984 # But 12 Nans

# Croston = 1.899094
# Croston15 = 1.899094
# Croston0.001 = 1.899094

# ETS AAA = 1.901174
# ETS ANA = 1.893387
# ETS ANN = 1.889149
# ETS AAN = 1.895408
# ETS AAdN0.9 = 1.88914
# ETS AAdN0.8 = 1.889015 # Not bettern then croston in kaggle
# ETS AAdA0.8 = 1.893927
# ETS MNN: NAN

# MEAN28days =  1.839535

####################
# Arima playground
####################

aics = c()
for (i in 1:9){
  food = paste("FOODS_3_00", as.character(i) , "_TX_3_validation", sep = "")
  slice = merge_ts %>% filter(id == food)
  arima_fit <- slice %>% model(NNETAR(sqrt(sales))) 
  print(arima_fit)
  aic = glance(arima_fit) %>% arrange(AICc) %>% select(AICc)
  aics = append(aics,aic)
}
aics

slice = merge_ts %>% filter(id == 'FOODS_3_002_TX_3_validation')

arima_fit <- slice %>% 
  model(stepwise = ARIMA(sales))

test = glance(arima_fit) %>% arrange(AICc) %>% select(AICc)
append(aics,test)


###########
# Names
###########

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

fall_fit = merge_ts2  %>%  filter_index("2016-03-28" ~ "2016-04-24") %>% model(MEAN(sales))

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

write.csv(final_df,"/Users/dj/Desktop/kaggle_MEAN28.csv", row.names = TRUE)



#### Poisson Regression

hist(merge_ts$sales, breaks = 600, col = "lightblue", main = "Histogram of Sales", xlab = "Amount", xlim = range(c(0:20)))

qplot(merge_ts$sales, geom="histogram", xlim=c(0,100)) 


table(table(merge_ts$id))
