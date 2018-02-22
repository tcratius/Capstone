#~~~~~~~~~~~~
# Libraries
#~~~~~~~~~~~
library(tidyr)
library(dplyr)
library(ggplot2)
library(weathermetrics)
library(chron)
#~~~~~~~~~~~~
# Source
#~~~~~~~~~~~
source("R/Capstone_func.R")

#~~~~~~~~~~~~
# Input file, replace "-" with NA
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather.csv"

Weather_Data <- read.csv(csv, header = TRUE, sep = ",", dec = ".", na.strings = "-")

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
write.table(Weather_Data_Filter, file = "data/raw1/Walmart/weather_leftover_stations.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

rm(Weather_Data_Filter) # Remove file

#~~~~~~~~~~~~
# Input file
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather_leftover_stations.csv"
Weather_Data_Rise_Set <- read.csv(csv, header = TRUE, sep = ",", dec = ".")


#~~~~~~~~~~~~
# Change data type from factor to Date and Time for Sunrise Sunset
#~~~~~~~~~~~
Weather_Data_Rise_Set$date <- as.Date(Weather_Data_Rise_Set$date, format = "%Y-%m-%d")

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the sunrise based on values existing
# values in sunrise and by using columns station_nbr & date
#~~~~~~~~~~~
model_one <- lm(chron(sunrise) ~ station_nbr + date, data = Weather_Data_Rise_Set)
I <- is.na(Weather_Data_Rise_Set$sunrise)
Weather_Data_Rise_Set$sunrise[I] <- predict(model_one, newdata = Weather_Data_Rise_Set[I, ])

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the sunset based on values 
# existing in sunset and by using columns sunrise, station_nbr & date
#~~~~~~~~~~~
model_two <- lm(chron(sunset) ~ sunrise + date + station_nbr, data = Weather_Data_Rise_Set)
J <- is.na(Weather_Data_Rise_Set$sunset)
Weather_Data_Rise_Set$sunset[J] <- predict(model_two, newdata = Weather_Data_Rise_Set[J, ])

#~~~~~~~~~~~~
# Change sunrise sunset values using floor 
#~~~~~~~~~~~
Weather_Data_Rise_Set$sunrise <- floor(Weather_Data_Rise_Set$sunrise)
Weather_Data_Rise_Set$sunset <- floor(Weather_Data_Rise_Set$sunset)

#~~~~~~~~~~~~
# Output to file 
#~~~~~~~~~~~
write.table(Weather_Data_Rise_Set, file = "data/raw1/Walmart/weather_leftover_stations_rs.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

#~~~~~~~~~~~~
# Remove files 
#~~~~~~~~~~~
rm(model_one, model_two, I, J, Weather_Data_Rise_Set)

#~~~~~~~~~~~~
# Input file
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather_leftover_stations_rs.csv"
Weather_Data_Missing_Values <- read.csv(csv, header = TRUE, sep = ",", dec = ".", na.strings = "M")

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the tmax based on values 
# existing in tmax plus columns station_nbr, date, tmin, dewpoint, wetbulb, sunrise, sunset
#~~~~~~~~~~~
model_three <- lm(tmax ~ station_nbr + date + tmin + dewpoint + wetbulb + sunrise + sunset, data = Weather_Data_Missing_Values)
K <- is.na(Weather_Data_Missing_Values$tmax)
Weather_Data_Missing_Values$tmax[K] <- predict(model_three, newdata = Weather_Data_Missing_Values[K, ])

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the tmin based on values 
# existing in tmin plus columns station_nbr, date, tmax, dewpoint, wetbulb, sunrise, sunset
#~~~~~~~~~~~
model_four <- lm(tmin ~ station_nbr + date + tmax + dewpoint + wetbulb + sunrise + sunset, data = Weather_Data_Missing_Values)
L <- is.na(Weather_Data_Missing_Values$tmin)
Weather_Data_Missing_Values$tmin[L] <- predict(model_four, newdata = Weather_Data_Missing_Values[L, ])

#~~~~~~~~~~~~
# Change types
#~~~~~~~~~~~
Weather_Data_Missing_Values$tmax <- as.numeric(Weather_Data_Missing_Values$tmax)
Weather_Data_Missing_Values$tmin <- as.numeric(Weather_Data_Missing_Values$tmin)

#~~~~~~~~~~~~
# Change temperatur from Fahrenheit to Celsius
#~~~~~~~~~~~
Weather_Data_Missing_Values <- Weather_Data_Missing_Values %>%
  transform(tmax = convert_temperature(tmax, old_metric = "f", new_metric = "c")) %>%
  transform(tmin = convert_temperature(tmin, old_metric = "f", new_metric = "c")) 
  
Weather_Data_Missing_Values$tmax <- floor(Weather_Data_Missing_Values$tmax)  
Weather_Data_Missing_Values$tmin <- floor(Weather_Data_Missing_Values$tmin)  

rm(model_four, model_three, K, L)  


#~~~~~~~~~~~~
# Using function transform the avg daily temperature can be calculated
#~~~~~~~~~~~
Weather_Data_Missing_Values <- Weather_Data_Missing_Values %>%
  transform(tavg = (tmax + tmin)/2)