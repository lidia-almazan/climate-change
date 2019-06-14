########################
# Climate change Project 
# Lidia Almazan
# 08/06/2019
########################

##################################
# Analysis of the temperature data
##################################

# loading necessary library
library(tidyverse)
library(caret)
library(ggplot2)

# Data copied from https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data
# The raw data comes from the Berkeley Earth data page http://berkeleyearth.org/data/
temp_country = read.csv("temperature-data/GlobalLandTemperaturesByCountry.csv")

# reading the temperature in the country file and removing NA values
head(temp_country)
temp_country <- temp_country %>% na.omit(temp_country)
n_distinct(temp_country$Country) # there is data from 242 different countries

# selecting only the temperature from spain
temp_country_spain <- temp_country %>% filter(Country=="Spain")
head(temp_country_spain)
n_distinct(temp_country_spain)

# selection of the data after 1800 averaging for the every year
temp_spain_year_1800_2013 <- temp_country_spain %>% 
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  filter(Year>1800) %>%
  group_by(Year) %>% 
  summarise(Temp = mean(AverageTemperature))

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

eurozone <- c("Austria","Belgium","Cyprus","Estonia","Finland","France","Germany",
              "Greece","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta",
              "Netherlands","Portugal","Slovakia","Slovenia","Spain")
temp_eurozone_year_1800_2013 <- temp_country %>% 
  filter(Country %in% eurozone) %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  filter(Year>1800) %>%
  group_by(Year,Country) %>% summarise(Temp = mean(AverageTemperature))

# plot of the average temperature along the years for each country
ggplot(temp_eurozone_year_1800_2013,
       aes(x=Year,y=Temp,colour=reorder(Country, -Temp,mean)))+
  geom_point()+
  geom_smooth(method =  "loess", span = 0.15, method.args = list(degree=1)) +
  labs(title="Average Temperatures by Month in Spain",
       x="Year",
       y="Temperature (°C)",
       colour="Country")

##########################################
# Machine learning on the temperature data
##########################################

# creation of the data partition
y <- temp_spain_year_1800_2013$Temp
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train_set <- temp_spain_year_1800_2013 %>% slice(-test_index)
test_set <- temp_spain_year_1800_2013 %>% slice(test_index)

# average of the temperature ignoring the year influence
m <- mean(train_set$Temp)
m
# computing the squared loss
mean((m - test_set$Temp)^2)

# using least square as method for estimating the slope taking into account the 
# year
fit <- lm(Temp ~ Year, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$Year
mean((y_hat - test_set$Temp)^2)
