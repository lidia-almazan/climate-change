###############################
# Temperature evolution Project 
# Lidia Almazan
# 08/06/2019
###############################

##################################
# Analysis of the temperature data
##################################

# loading necessary library
library(tidyverse)
library(caret)
library(ggplot2)
library(randomForest)

# Data copied from https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data
# The raw data comes from the Berkeley Earth data page http://berkeleyearth.org/data/
temp_country = read.csv("temperature-data/GlobalLandTemperaturesByCountry.csv")

# reading the temperature in the country file and removing NA values
head(temp_country)
temp_country <- temp_country %>% na.omit(temp_country)
n_distinct(temp_country$Country) # there is data from 242 different countries
summary(temp_country)

# selecting only the temperature from spain
temp_country_spain <- temp_country %>% filter(Country=="Spain")
head(temp_country_spain)
n_distinct(temp_country_spain)
summary(temp_country_spain)

# selection of the data after 1800 averaging for the every year
temp_spain_year_1800_2013 <- temp_country_spain %>% 
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  filter(Year>1800) %>%
  group_by(Year) %>% 
  summarise(Temp = mean(AverageTemperature))
summary(temp_spain_year_1800_2013)

# plot of the average temperature in spain between 1800 and 2013 with smoothing loess
qplot(Year,
      Temp,
      data=temp_spain_year_1800_2013,
      main="Spain Average Temperature 1800-2013",
      geom=c("point")) +
  geom_smooth(method =  "loess",color="red",  span = 0.15, method.args = list(degree=1)) +
  aes(colour = Temp) +
  scale_color_gradient(low="blue", high="red") +
  ylab("Temperature (°C)")

# selection of the data averaged by month in the range between 1800-2013
temp_spain_month <- temp_country_spain %>% 
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  filter(Year>1800) %>%
  group_by(Month) %>% 
  summarise(Temp = mean(AverageTemperature))

# adding a new column with the month names instead of numbers
temp_spain_month$Month.Name <- with(temp_spain_month, month.name[Month])

# plot of the average temperature along the months in the range of 
# years 1800 and 2013
ggplot(temp_spain_month, aes(x=Month.Name,y=Temp)) +
  theme(axis.text.x = element_text(angle =45, hjust = 1)) +
  geom_point() +
  labs(title="Average Temperatures by Month in Spain",
       x="Month",
       y="Temperature (°C)") +
  scale_x_discrete(limits=c("January", "February", "March", "April", "May", 
                            "June", "July", "August", "September", "October", 
                            "November", "December"))

# selection of the data after 1800 averaging for the every month
temp_spain_month_1800_2013 <- temp_country_spain %>% 
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  filter(Year>1800) %>%
  group_by(Month)

# adding a new column with the month names instead of numbers
temp_spain_month_1800_2013$Month.Name <- with(temp_spain_month_1800_2013, month.name[Month])

# plot of the average temperature along the years for each month
ggplot(temp_spain_month_1800_2013,
       aes(x=Year,y=AverageTemperature,colour=reorder(Month.Name, -AverageTemperature,mean)))+
  geom_point()+
  geom_smooth(method =  "loess", span = 0.15, method.args = list(degree=1)) +
  labs(title="Average Temperatures by Month in Spain",
       x="Year",
       y="Temperature (°C)",
       colour="Month")

# countries inside the eurozone
eurozone <- c("Austria","Belgium","Cyprus","Estonia","Finland","France","Germany",
              "Greece","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta",
              "Netherlands","Portugal","Slovakia","Slovenia","Spain")

# selection of the countries on the eurozone from 1800
temp_eurozone_year_1800_2013 <- temp_country %>% 
  filter(Country %in% eurozone) %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  filter(Year>1800) %>%
  group_by(Year,Country) %>% summarise(Temp = mean(AverageTemperature))

# plot of the average temperature along the years for each country in eurozone
ggplot(temp_eurozone_year_1800_2013,
       aes(x=Year,y=Temp,colour=reorder(Country, -Temp,mean)))+
  geom_point()+
  geom_smooth(method =  "loess", span = 0.15, method.args = list(degree=1)) +
  labs(title="Average Temperatures by Countries in the Eurozone",
       x="Year",
       y="Temperature (°C)",
       colour="Country")

##########################################
# Machine learning on the temperature data
##########################################

set.seed(1)

# creation of the data partition with 80% of the data in train the rest in test
y <- temp_spain_year_1800_2013$Temp
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train_set <- temp_spain_year_1800_2013 %>% slice(-test_index)
test_set <- temp_spain_year_1800_2013 %>% slice(test_index)

# plot of the 'train' and 'test' data with different colours
ggplot() + 
  geom_point(data=train_set, aes(x=Year, y=Temp, colour = "Train set")) + 
  geom_point(data=test_set, aes(x=Year, y=Temp, colour = "Test set")) +
  labs(title="Selection of the train and test set",x="Year",y="Temperature (°C)")

## Method 1 : naive method, mean

# average of the temperature ignoring the year influence
m <- mean(train_set$Temp)
m
# computing the squared loss
model1_rmse <- mean((m - test_set$Temp)^2)

# saving the prediction in a data frame
rmse_results <- data_frame(Model = "1 - Mean", RMSE = model1_rmse)
rmse_results %>% knitr::kable()

## Method 2 : least square

# using least square as method for estimating the slope taking into account the 
# year
fit_lm <- lm(Temp ~ Year, data = train_set)
fit_lm

# estimated temperature with the coefficients of the lm method
y_hat_lm <- fit_lm$coef[1] + fit_lm$coef[2]*test_set$Year
# mean squared error of the lm obtained from the train for the test method
model2_rmse <- mean((y_hat_lm - test_set$Temp)^2)
# saving the prediction in a data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="2 - Least square",  
                                     RMSE = model2_rmse ))
rmse_results %>% knitr::kable()

# plot of the lm function with the test data
ggplot(test_set, aes(Year)) + 
  geom_point(aes(y = test_set$Temp, colour = "Test data")) + 
  geom_line(aes(y = y_hat_lm, colour = "Fit lm")) +
  labs(x="Year",y="Temperature (°C)")

# plot of the lm function with the whole data
temp_spain_year_1800_2013 %>%
  mutate(y_hat_lm = predict(fit_lm, newdata = temp_spain_year_1800_2013)) %>% 
  ggplot() +
  geom_point(aes(Year, Temp, colour = "Temp data")) +
  geom_line(aes(Year, y_hat_lm, colour = "Fit lm")) +
  labs(x="Year",y="Temperature (°C)")

## Method 3 : random forest

# random forest model with default parameters
fit_rf <- randomForest(Temp ~ Year , data = train_set, importance = TRUE) 
fit_rf # number of trees used 500

# plot of the error in function of the number of trees
plot(fit_rf)

# output of the temp obtained from rf model in the test set
y_hat_rf = predict(fit_rf, newdata = test_set)
# mean squared error for rf
model3_rmse <- mean((y_hat_rf - test_set$Temp)^2)
# saving the prediction in a data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="3 - Random forest",  
                                     RMSE = model3_rmse ))
rmse_results %>% knitr::kable()

# plot of the rf method obtained with the train set applied to the test set
test_set %>%
  mutate(y_hat_rf = predict(fit_rf, newdata = test_set)) %>% 
  ggplot() +
  geom_point(aes(Year, Temp, colour = "Test data")) +
  geom_line(aes(Year, y_hat_rf, colour = "Fit rf")) +
  labs(x="Year",y="Temperature (°C)")

# plot of the rf method obtained with the train set applied to all the data
temp_spain_year_1800_2013 %>%
  mutate(y_hat_rf = predict(fit_rf, newdata = temp_spain_year_1800_2013)) %>% 
  ggplot() +
  geom_point(aes(Year, Temp, colour = "Temp data")) +
  geom_line(aes(Year, y_hat_rf, colour = "Fit rf")) +
  labs(x="Year",y="Temperature (°C)")

# final results with the comparison of the three models
# RMSE final results
rmse_results %>% knitr::kable()
