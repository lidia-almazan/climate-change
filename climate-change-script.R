########################
# Climate change Project 
# Lidia Almazan
# 08/06/2019
########################

##################################
# Analysis of the temperature data
##################################

# loading necessary library
library(dplyr)
library(tidyr)
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
  geom_smooth(method="loess") +
  aes(colour = Temp) +
  scale_color_gradient(low="blue", high="red") +
  ylab("Temperature (°C)")

# selection of the data after 1800 averaging for the every month
temp_spain_month_1800_2013 <- temp_country_spain %>% 
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  filter(Year>1800) %>%
  group_by(Month)

temp_spain_month_1800_2013$Month.String <- format(temp_spain_month_1800_2013$Month,"%B")

temp_spain_month_1800_2013$Month.Name <- with(temp_spain_month_1800_2013, month.name[Month])

ggplot(temp_spain_month_1800_2013,
       aes(x=Year,y=AverageTemperature,colour=reorder(Month.Name, -AverageTemperature,mean)))+
  geom_point()+
  geom_smooth(method="loess")+
  labs(title="Average Temperatures by Month in Spain",
       x="Year",
       y="Average Temperature",
       colour="Month")
