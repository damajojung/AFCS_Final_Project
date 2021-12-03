
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


# Pivot longer
train_long = pivot_longer(train, cols =  2:1914, names_to = "d", values_to = "sales")
test_long = pivot_longer(test, cols =  2:29, names_to = "d", values_to = "sales")

# Extract time information
dates_df = as_tibble(cal[c("date", "d")])

# Merge the two data frames
merge_df = merge(dates_df, train_long, by = "d") # Natural inner join
merge_df1 = merge(dates_df, test_long, by = "d") # Natural inner join

# Change the date from character to date format
merge_df["date"] = as.Date(merge_df$date, format = "%m/%d/%Y")
merge_df1["date"] = as.Date(merge_df1$date, format = "%m/%d/%Y")

merge_df <- merge_df[order(merge_df$date),] 
merge_df1 <- merge_df1[order(merge_df1$date),] 


# Convert Data frame to tsibble
merge_ts = merge_df %>%  as_tsibble(index = "date", key = c('id')) 
merge_ts1 = merge_df1 %>%  as_tsibble(index = "date", key = c('id')) 


#######
# LOAD merge_df
#######

load("Data_First_Steps.RData") # Load everything

######################## 
# Forecasting
######################## 


#######
# Play around
#######

####################### TS

merge_ts %>% filter(id == 'FOODS_3_007_TX_3_validation') %>% select(date, sales) %>% autoplot(sales)

dat = merge_ts %>% filter(id == 'FOODS_3_007_TX_3_validation') %>% select(date, sales)

########## 
## Playing around with Naive Methods - only food 1
########## 

naive_fit = dat  %>% model('Seasonal_naive' = SNAIVE(sales),
                        'Naive' = NAIVE(sales),
                        'Drift' = RW(sales ~ drift()),
                        'Mean' = MEAN(sales))

naive_fc = naive_fit %>% forecast(h = '28 days')


naive_fc %>% autoplot(dat, level = NULL) + labs(title = "Food one", y = "Sold units") + guides(colour = guide_legend(title = 'Forecast'))

########## 
## STL
########## 

dat %>% model(stl = STL(sales ~ season(window = 9), robust = T)) %>% components() %>%   autoplot()

########## 
## TSLM
########## 

tslm_fit = dat %>% model(tslm = TSLM(sales ~ trend()))
tslm_fc = forecast(tslm_fit, h = 28)
tslm_fc %>%  autoplot()
tslm_fc %>%  autoplot(dat)


##########
## Multiplicative and Additive ETS
##########

ets_fit <- dat %>% model(multiplicative = ETS(sales ~ error("M") + trend("A") + season("M")),
                         additive = ETS(sales ~ error("A") + trend("A") + season("A")))

ets_fc <- ets_fit %>% forecast(h = "28 days")
ets_fc %>% autoplot()

ets_fc %>% autoplot(dat, level = NULL) 

##########
## ARIMA
##########

arima_fit <- dat %>% 
  model(arima100 = ARIMA(sales ~ pdq(1,0,0)),
        arima200 = ARIMA(sales ~ pdq(2,0,0)),
        arima001 = ARIMA(sales ~ pdq(0,0,1)),
        arima002 = ARIMA(sales ~ pdq(0,0,2)),
        arima110 = ARIMA(sales ~ pdq(1,1,0)),
        arima011 = ARIMA(sales ~ pdq(0,1,1)),
        arima210 = ARIMA(sales ~ pdq(2,1,0)),
        arima012 = ARIMA(sales ~ pdq(0,1,2)),
        stepwise = ARIMA(sales),
        search = ARIMA(sales, stepwise=FALSE, approximation = FALSE))

glance(arima_fit) %>% arrange(AICc) %>% select(.model:BIC)

arima_fit = dat %>% model(arima012 = ARIMA(sales ~ pdq(0,1,2)))

arima_fc =  arima_fit %>% forecast(h = "28 days")

arima_fc %>% autoplot()

##########
## Harmonic Regression
##########

har_fit = dat %>% model(k1 = TSLM(sales ~ trend() + fourier(K=1)),
                          k2 = TSLM(sales ~ trend() + fourier(K=2)),
                          k3 = TSLM(sales ~ trend() + fourier(K=3)))


glance(har_fit) %>% arrange(AICc) %>% select(.model:BIC)

har_fit = dat %>% model(k1 = TSLM(sales ~ trend() + fourier(K=1)))

har_fc = har_fit %>% forecast(h = "28 days")

har_fc %>% autoplot()

##########
## Croston - Rather predicting a mean - does not look very good
##########

cros_fit = dat %>%model(CROSTON(sales)) 

cros_fc = cros_fit %>% forecast(h = 28)

cros_fc %>% autoplot(dat)

##########
## Neural Network
##########

NN_fit = dat %>% model(NNETAR(sqrt(sales))) 

NN_fc = NN_fit %>% forecast(h = 28) 

NN_fc %>% autoplot()

NN_fc %>% autoplot(dat) + labs(x = "Days", y = "Counts", title = "Dayily Sales Prediction")



###### All foods

fall_fit = merge_ts  %>% model('Seasonal_naive' = SNAIVE(sales))

fall_fc = fall_fit %>% forecast(h = '28 days')



########## 
## ACF plot
########## 

food1 %>% gg_tsdisplay(difference(sales), plot_type = "partial") # With one difference the mean becomes 0, but does not look like WN




############################
## TEST AND TRAIN DATA 
############################

dat_train = merge_ts %>% filter_index(~"2016-03-27")
dat_test =  merge_ts %>% filter_index("2016-03-28" ~ "2016-04-24")


############################
## Predictions of whole data sets
############################

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

write.csv(final_df,"/Users/dj/Desktop/kaggle_sub.csv", row.names = TRUE,)

?write.csv



