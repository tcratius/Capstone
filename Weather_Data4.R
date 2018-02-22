#~~~~~~~~~~~~
# Libraries
#~~~~~~~~~~~
library(tidyr)
library(dplyr)
library(ggplot2)
library(weathermetrics)
library(chron)
library(VIM)
#~~~~~~~~~~~~
# Source
#~~~~~~~~~~~
source("R/Capstone_func.R")

#~~~~~~~~~~~~
# Input file, replace "-" with NA
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather.csv"
Weather_Data_Rise_Set <- read.csv(csv, header = TRUE, sep = ",", dec = ".", na.strings = "-")

#~~~~~~~~~~~~
# Change data type from factor to Date
#~~~~~~~~~~~
Weather_Data_Rise_Set$date <- as.Date(Weather_Data_Rise_Set$date, format = "%Y-%m-%d")

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the sunrise based on values existing
# values in sunrise and by using columns station_nbr & date
# Chron set sunset and sunrise values to numeric and in time formate i.e. not exceeding 60
# minutes in an hours o
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
write.table(Weather_Data_Rise_Set, file = "data/raw1/Walmart/weather_data_rise_set.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

#~~~~~~~~~~~~
# Remove files 
#~~~~~~~~~~~
rm(model_one, model_two, I, J, Weather_Data_Rise_Set)

#~~~~~~~~~~~~
# Input file
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather_data_rise_set.csv"
Weather_Data_Missing_Values <- read.csv(csv, header = TRUE, sep = ",", dec = ".", na.strings = "M")

#~~~~~~~~~~~~
# Change types
#~~~~~~~~~~~
Weather_Data_Missing_Values$tmax <- as.numeric(Weather_Data_Missing_Values$tmax)
Weather_Data_Missing_Values$tmin <- as.numeric(Weather_Data_Missing_Values$tmin)
Weather_Data_Missing_Values$date <- as.Date(Weather_Data_Missing_Values$date, format = "%Y-%m-%d")

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the tmax based on values 
# existing in tmax plus columns station_nbr, date, tmin, dewpoint, wetbulb, sunrise, sunset
#~~~~~~~~~~~
model_three <- lm(tmax ~ station_nbr + date + tmin + dewpoint + wetbulb +
                  sunrise + sunset, data = Weather_Data_Missing_Values)
K <- is.na(Weather_Data_Missing_Values$tmax)
Weather_Data_Missing_Values$tmax[K] <- predict(model_three, newdata = Weather_Data_Missing_Values[K, ])

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the tmin based on values 
# existing in tmin plus columns station_nbr, date, tmax, dewpoint, wetbulb, sunrise, sunset
#~~~~~~~~~~~
model_four <- lm(tmin ~ station_nbr + date + tmax + dewpoint + wetbulb + sunrise + sunset,
                 data = Weather_Data_Missing_Values)
L <- is.na(Weather_Data_Missing_Values$tmin)
Weather_Data_Missing_Values$tmin[L] <- predict(model_four, newdata = Weather_Data_Missing_Values[L, ])

#~~~~~~~~~~~~
# Change temperatur from Fahrenheit to Celsius
#~~~~~~~~~~~
Weather_Data_Missing_Values <- Weather_Data_Missing_Values %>%
  transform(tmax = convert_temperature(tmax, old_metric = "f", new_metric = "c")) %>%
  transform(tmin = convert_temperature(tmin, old_metric = "f", new_metric = "c")) 

#~~~~~~~~~~~~
# Round up to the nearest integer with floor
#~~~~~~~~~~~
Weather_Data_Missing_Values$tmax <- floor(Weather_Data_Missing_Values$tmax)  
Weather_Data_Missing_Values$tmin <- floor(Weather_Data_Missing_Values$tmin)  

rm(model_four, model_three, K, L)  


#~~~~~~~~~~~~
# Using function transform the avg daily temperature can be calculated, this 
# will become celsius as the min and max tempearture where in celsius
#~~~~~~~~~~~
Weather_Data_Missing_Values <- Weather_Data_Missing_Values %>%
  transform(tavg = (tmax + tmin)/2)

#~~~~~~~~~~~~
# Round up to the nearest integer with floor
#~~~~~~~~~~~
Weather_Data_Missing_Values$tavg <- floor(Weather_Data_Missing_Values$tavg)
 
#~~~~~~~~~~~~
# Apply linear model {stats} to predict the dewpoint based on values 
# existing in dewpoint plus columns station_nbr, date, tmax, tmin, wetbulb, stdpressure, sealevel
#~~~~~~~~~~~
model_five <- lm(dewpoint ~ station_nbr + date + tmax + tmin + wetbulb + stnpressure + sealevel, data = Weather_Data_Missing_Values)
M <- is.na(Weather_Data_Missing_Values$dewpoint)
Weather_Data_Missing_Values$dewpoint[M] <- predict(model_five, newdata = Weather_Data_Missing_Values[M, ])

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the wetbulb based on values 
# existing in wetbulb plus columns station_nbr, date, tmax, tmin, dewpoint, stdpressure
#~~~~~~~~~~~
model_six <- lm(wetbulb ~ station_nbr + date + tmax + tmin + dewpoint + stnpressure + sealevel, data = Weather_Data_Missing_Values)
N <- is.na(Weather_Data_Missing_Values$wetbulb)
Weather_Data_Missing_Values$wetbulb[N] <- predict(model_six, newdata = Weather_Data_Missing_Values[N, ])

#~~~~~~~~~~~~
# Change temperatur from Fahrenheit to Celsius
#~~~~~~~~~~~
Weather_Data_Missing_Values <- Weather_Data_Missing_Values %>%
  transform(dewpoint = convert_temperature(dewpoint, old_metric = "f", new_metric = "c")) %>%
  transform(wetbulb = convert_temperature(wetbulb, old_metric = "f", new_metric = "c")) 

#~~~~~~~~~~~~
# Round up to the nearest integer with floor
#~~~~~~~~~~~
Weather_Data_Missing_Values$dewpoint <- floor(Weather_Data_Missing_Values$dewpoint)
Weather_Data_Missing_Values$wetbulb <- floor(Weather_Data_Missing_Values$wetbulb)

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the heating day based on values 
# existing in heating plus columns station_nbr, date, tmax, tmin, wetbulb, resultspeed, resultdir
#~~~~~~~~~~~
model_seven <- lm(heat ~ station_nbr + date + tmax + tmin + wetbulb + resultspeed + resultdir, data = Weather_Data_Missing_Values)
O <- is.na(Weather_Data_Missing_Values$heat)
Weather_Data_Missing_Values$heat[O] <- predict(model_seven, newdata = Weather_Data_Missing_Values[O, ])

#~~~~~~~~~~~~
# Apply linear model {stats} to predict the cool based on values 
# existing in cool plus columns station_nbr, date, tmax, tmin, dewpoint, resultspeed, resultdir
#~~~~~~~~~~~
model_eight <- lm(cool ~ station_nbr + date + tmax + tmin + dewpoint + resultspeed + resultdir, data = Weather_Data_Missing_Values)
P <- is.na(Weather_Data_Missing_Values$cool)
Weather_Data_Missing_Values$cool[P] <- predict(model_eight, newdata = Weather_Data_Missing_Values[P, ])

#~~~~~~~~~~~~
# Round up to the nearest integer with floor
#~~~~~~~~~~~
Weather_Data_Missing_Values$heat <- floor(Weather_Data_Missing_Values$heat)
Weather_Data_Missing_Values$cool <- floor(Weather_Data_Missing_Values$cool)

#~~~~~~~~~~~~
# Set level "T" in factor snowfall to mean of 0.00 and 0.1 and preciptotal to mean of 0.00 and 0.01
#~~~~~~~~~~~
levels(Weather_Data_Missing_Values$snowfall)[levels(Weather_Data_Missing_Values$snowfall)=="  T"] <- 0.05
levels(Weather_Data_Missing_Values$preciptotal)[levels(Weather_Data_Missing_Values$preciptotal)=="  T"] <- 0.005

#~~~~~~~~~~~~
# Output to file 
#~~~~~~~~~~~
write.table(Weather_Data_Missing_Values, file = "data/raw1/Walmart/weather_data_missing_values.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

#~~~~~~~~~~~~
# Input
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather_data_missing_values.csv"
Weather_Data_knn <- read.csv(csv, header = TRUE, sep = ",", dec = ".")

#~~~~~~~~~~~
# total_Nas <- (sum(7224,860,929,1724,589,589,875))/20517 = 0.6233855
# Using Vim's kNN based on the principal that nearest matches should be contained in 
# Last 7 columns
#~~~~~~~~~~
Weather_Data_knn <- kNN(Weather_Data_knn, k = 5, imp_var = FALSE)

#~~~~~~~~~~~~
# Change type from factor to numeric
#~~~~~~~~~~~
Weather_Data_Missing_Values$snowfall <- as.numeric(Weather_Data_Missing_Values$snowfall)
Weather_Data_Missing_Values$preciptotal <- as.numeric(Weather_Data_Missing_Values$preciptotal)