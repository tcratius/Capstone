#~~~~~~~~~~~~
# Libraries
#~~~~~~~~~~~
library(tidyr)
library(dplyr)
library(ggplot2)
library(weathermetrics)
library(datetime)
library(chron)
#~~~~~~~~~~~~
# Source
#~~~~~~~~~~~
source("R/Capstone_func.R")

#~~~~~~~~~~~~
# Input file
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather.csv"
Weather_Data <- read.csv(csv, header = TRUE, sep = ",", dec = ".", na.strings = "-")

#~~~~~~~~~~~~
# Change types
#~~~~~~~~~~~
Weather_Data[, c("tmax", "tmin","sunrise", "sunset")] <- 
  lapply(Weather_Data[, c("tmax", "tmin", "sunrise", "sunset")], 
         function(x) as.numeric(levels(x))[x]) 

#~~~~~~~~~~~~
# Change data type from factor to Date and Time for Sunrise Sunset
#~~~~~~~~~~~
Weather_Data$date <- as.Date(Weather_Data$date, format = "%Y-%M-%M")

#~~~~~~~~~~~~
# Extract date, station_nbr, and sunrise values for cooler weather 
# Group_By Station ID
# COOLING (SEASON BEGINS WITH JANUARY)
#~~~~~~~~~~~
Weather_Data_Sunrise_Cool <- Weather_Data %>%
  group_by(station_nbr) %>%
  filter(date > as.Date("2012-01-01"))%>%
  filter(date < as.Date("2012-06-30")) %>%
  filter(date > as.Date("2013-01-01") | date < as.Date("2013-06-30")) %>%
  filter(date > as.Date("2014-01-01") | date < as.Date("2014-06-30")) %>%
  select(station_nbr, date, sunrise)

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the sunrise in cooler weather based 
# on values existing in sunrise and by using columns station_nbr & date
#~~~~~~~~~~~
model_one <- lm(chron(sunrise) ~ station_nbr + date, data = Weather_Data_Sunrise_Cool)
I <- is.na(Weather_Data_Sunrise_Cool$sunrise)
Weather_Data_Sunrise_Cool$sunrise[I] <- predict(model_one, newdata = Weather_Data_Sunrise_Cool[I, ])

#~~~~~~~~~~~~
# Extract date, station_nbr, and sunset values for cooler weather 
# Group_By Station ID
# COOLING (SEASON BEGINS WITH JANUARY)
#~~~~~~~~~~~
Weather_Data_Sunset_Cool <- Weather_Data %>%
  group_by(station_nbr) %>%
  mutate(date >= "2012-01-01" & date <= "2012-06-30") %>%
  mutate(date >= "2013-01-01" & date <= "2013-06-30") %>%
  mutate(date >= "2014-01-01" & date <= "2014-06-30") %>%
  select(station_nbr, date, sunset)

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the sunset in cooler weather based 
# on values existing in sunset and by using columns station_nbr & date
#~~~~~~~~~~~
model_two <- lm(sunset ~ station_nbr + date, data = Weather_Data_Sunset_Cool)
J <- is.na(Weather_Data_Sunset_Cool$sunset)
Weather_Data_Sunset_Cool$sunset[J] <- predict(model_two, newdata = Weather_Data_Sunset_Cool[J, ])

#~~~~~~~~~~~~
# Extract date, station_nbr, and sunset values for hotter weather 
# Group_By Station ID
# HEATING (SEASON BEGINS WITH JULY) Last date is 2014-10-31
#~~~~~~~~~~~
Weather_Data_Sunset_Hot <- Weather_Data %>%
  group_by(station_nbr) %>%
  mutate(date >= "2012-07-01" & date <= "2012-12-31") %>%
  mutate(date >= "2013-07-01" & date <= "2013-12-31") %>%
  mutate(date >= "2014-07-01" & date <= "2014-10-31") %>%
  select(station_nbr, date, sunset)

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the sunset in hotter weather based 
# on values existing in sunset and by using columns station_nbr & date
#~~~~~~~~~~~
model_three <- lm(sunset ~ station_nbr + date, data = Weather_Data_Sunset_Hot)
K <- is.na(Weather_Data_Sunset_Hot$sunset)
Weather_Data_Sunset_Hot$sunset[K] <- predict(model_three, newdata = Weather_Data_Sunset_Hot[K, ])

#~~~~~~~~~~~~
# Extract date, station_nbr, and sunrise values for hotter weather 
# Group_By Station ID
# HEATING (SEASON BEGINS WITH JULY) Last date is 2014-10-31
#~~~~~~~~~~~
Weather_Data_Sunrise_Hot <- Weather_Data %>%
  group_by(station_nbr) %>%
  mutate(date >= "2012-07-01" & date <= "2012-12-31") %>%
  mutate(date >= "2013-07-01" & date <= "2013-12-31") %>%
  mutate(date >= "2014-07-01" & date <= "2014-10-31") %>%
  select(station_nbr, date, sunrise)

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the sunset in hotter weather based 
# on values existing in sunset and by using columns station_nbr & date
#~~~~~~~~~~~
model_four <- lm(sunrise ~ station_nbr + date, data = Weather_Data_Sunrise_Hot)
L <- is.na(Weather_Data_Sunrise_Hot$sunrise)
Weather_Data_Sunrise_Hot$sunrise[L] <- predict(model_three, newdata = Weather_Data_Sunrise_Hot[L, ])

#~~~~~~~~~~~~
# Change to time value
#~~~~~~~~~~~

Weather_Data_Sunrise_Cool$sunrise <- floor(Weather_Data_Sunrise_Cool$sunrise)
Weather_Data_Sunrise$sunrise <- as.time(Weather_Data_Sunrise$sunrise, "%H:%M")

#~~~~~~~~~~~~
# Keep "M" missing values for station five
#~~~~~~~~~~~
Weather_Data_Filter <- Weather_Data %>%
  group_by(station_nbr == 5) %>%
  filter(tmax == 'M') %>%
  filter(tmin == 'M')

#~~~~~~~~~~~~
# Store Weather_Data_Filter contain station 5s Missing values for period 2012-2014
#~~~~~~~~~~~
write.table(Weather_Data_Filter, file = "data/raw1/Walmart/weather_stat5.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

#~~~~~~~~~~~~
# Remove "M" missing values for station five
#~~~~~~~~~~~
Weather_Data_Filter <- Weather_Data %>%
  group_by(station_nbr == 5) %>%
  filter(tmax != 'M') %>%
  filter(tmin != 'M')

#~~~~~~~~~~~~
# Output to file 
#~~~~~~~~~~~
write.table(Weather_Data_Filter, file = "data/raw1/Walmart/weather_all_stations.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

#~~~~~~~~~~~~
# Input file
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather_all_stations.csv"
Weather_Data <- read.csv(csv, header = TRUE, sep = ",", dec = ".", na.strings = "M")

#~~~~~~~~~~~~
# Change temperatur from Fahrenheit to Celsius
#~~~~~~~~~~~
Weather_Data_Filter <- Weather_Data_Filter %>%
  transform(tmax = convert_temperature(tmax, old_metric = "f", new_metric = "c")) %>%
  transform(tmin = convert_temperature(tmin, old_metric = "f", new_metric = "c"))
  


#~~~~~~~~~~~~
# Count the number of NA's which were originally "-"
# 9656 for Sunrise/Sunset
#~~~~~~~~~~~
colSums(is.na(Weather_Data))
length(Weather_Data$sunrise) # 20517
colSums(Weather_Data == "M")
Dum <- Weather_Data %>%
  filter(snowfall != "M" & cool != "M" )

