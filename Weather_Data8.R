#~~~~~~~~~~~~
# Libraries
#~~~~~~~~~~~
library(tidyr)
library(dplyr)
library(ggplot2)
library(weathermetrics)
library(chron)
library(VIM)
library(stringr)
library(scales)

library(glue)
library(EnvStats)
#~~~~~~~~~~~~
# Source
#~~~~~~~~~~~
source("R/Miles_To_Kms.R")
source("R/Mercury_To_Hectopascal.R")
source("R/Strip.R")

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
# Set level "T" in factor snowfall to mean of 0.00 and 0.1 and preciptotal to mean of 0.00 and 0.01
#~~~~~~~~~~~
levels(Weather_Data_Missing_Values$snowfall)[levels(Weather_Data_Missing_Values$snowfall)=="  T"] <- 0.05
levels(Weather_Data_Missing_Values$preciptotal)[levels(Weather_Data_Missing_Values$preciptotal)=="  T"] <- 0.005

#~~~~~~~~~~~~
# Change types
#~~~~~~~~~~~
Weather_Data_Missing_Values$tmax <- as.numeric(Weather_Data_Missing_Values$tmax)
Weather_Data_Missing_Values$tmin <- as.numeric(Weather_Data_Missing_Values$tmin)
Weather_Data_Missing_Values$date <- as.Date(Weather_Data_Missing_Values$date, format = "%Y-%m-%d")
Weather_Data_Missing_Values$snowfall <- as.numeric(Weather_Data_Missing_Values$snowfall)
Weather_Data_Missing_Values$preciptotal <- as.numeric(Weather_Data_Missing_Values$preciptotal)

#~~~~~~~~~~~~
# Change temperatur from Fahrenheit to Celsius and wind speed from knots to kilometre per hour
#~~~~~~~~~~~
Weather_Data_Missing_Values <- Weather_Data_Missing_Values %>%
  transform(dewpoint = convert_temperature(dewpoint, old_metric = "f", new_metric = "c")) %>%
  transform(wetbulb = convert_temperature(wetbulb, old_metric = "f", new_metric = "c")) %>%
  transform(tmax = convert_temperature(tmax, old_metric = "f", new_metric = "c")) %>%
  transform(tmin = convert_temperature(tmin, old_metric = "f", new_metric = "c")) %>%
  transform(resultspeed = Mph_Kmph(resultspeed)) %>%
  transform(avgspeed = Mph_Kmph(avgspeed)) %>%
  transform(stnpressure = Hectopascal(stnpressure)) %>%
  transform(sealevel = Hectopascal(sealevel))

#~~~~~~~~~~~~
# Convert the numeric value to proximal integer value using floor for;
# resultspeed, avgspeed, stnpressure, sealevel
#~~~~~~~~~~~
Weather_Data_Missing_Values$resultspeed <- floor(Weather_Data_Missing_Values$resultspeed)
Weather_Data_Missing_Values$avgspeed <- floor(Weather_Data_Missing_Values$avgspeed)
Weather_Data_Missing_Values$stnpressure <- floor(Weather_Data_Missing_Values$stnpressure)
Weather_Data_Missing_Values$sealevel <- floor(Weather_Data_Missing_Values$sealevel)

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
# Round up to the nearest integer with floor
#~~~~~~~~~~~
Weather_Data_Missing_Values$tmax <- floor(Weather_Data_Missing_Values$tmax)  
Weather_Data_Missing_Values$tmin <- floor(Weather_Data_Missing_Values$tmin)  

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
# Output to file 
#~~~~~~~~~~~
write.table(Weather_Data_Missing_Values, file = "data/raw1/Walmart/weather_data_missing_values.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

rm(model_three, model_four, model_five, model_six, model_seven, model_eight, K, L, M, N, O, P)
#~~~~~~~~~~~~
# Input
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather_data_missing_values.csv"
Weather_Data_knn <- read.csv(csv, header = TRUE, sep = ",", dec = ".")

#~~~~~~~~~~~~
# Change codesum from factors to character 
#~~~~~~~~~~~
Weather_Data_knn$codesum <- as.character(Weather_Data_knn$codesum)

#~~~~~~~~~~~~
# Use function strip to change character to numeric value 
#~~~~~~~~~~~
Weather_Data_knn$codesum <- gsub("^PY", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^SQ", 2L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^DR", 0L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^SH", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^PR", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^BC", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^BL", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^VC", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^+FC", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^FC", 1L, Weather_Data_knn$codesum)

Weather_Data_knn$codesum <- gsub("^TS", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^GR", 2L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^RA", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^DZ", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^MI", 0L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^SN", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^FZ", 2L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^SG", 0L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^GS", 2L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^PL", 2L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^IC", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^FG", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^BR", 0L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^UP", 0L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^HZ", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^FU", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^VA", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^SA", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^SS", 1L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^DU", 2L, Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("^PO", 1L, Weather_Data_knn$codesum)

Weather_Data_knn$codesum <- gsub("\\+", "", Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("\\ ", "", Weather_Data_knn$codesum)
Weather_Data_Com <- Weather_Data_knn %>%
  transform(codesum = as.numeric(unlist(strsplit(codesum, split = ""))))

Weather_Data_knn <- Weather_Data_knn %>%
  transform(codesum = as.numeric(unlist(codesum)))




# total_Nas <- (sum(7224,860,929,1724,589,589,875, 11051))/20517 = 0.6233855
# kNN value k was assigned 5 due to fact it is dealing with positive integers
# https://jcu.summon.serialssolutions.com/#!/search?bookMark=ePnHCXMwPV2xDsIgECWmg9V-gRNOTo0FirSz0Th16t4ccHFoTIzR__co0I2Ey-UYuHtHeO92rKC-FdnCTyWwrc8h4REM2LCSyoupL72Qxbpu2m1-ESGsKnvRleyYpEWffOaE3Pg8DHwZERk-zyzxVqz4fn6UP8f7bbw-6jQ3oAbRUlMkUSk0ygVCEUXYGYve6caBAKTrZL3yjRU-YHkTtGFacGjQA3rtrAS1Z6foNheq6R2VH6bI4tVTOhJZHqIluNdqlDf_QzJG5A  
#~~~~~~~~~~
Weather_Data_knn <- kNN(Weather_Data_knn, k = 5, imp_var = FALSE)


#~~~~~~~~~~~~
# Output to file 
#~~~~~~~~~~~
write.table(Weather_Data_knn, file = "data/aggregated_formatted4/weather_data.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

#~~~~~~~~~~~~
# Input 
#~~~~~~~~~~~
csv = "data/aggregated_formatted4/weather_data.csv"
Weather_Data <- read.csv(csv, header = TRUE)

#~~~~~~~~~~~~
# Rescale using library scales
#~~~~~~~~~~~
Weather_Data_Complex <- Weather_Data
Weather_Data_Complex$tmax <- rescale(Weather_Data$tmax)
Weather_Data_Complex$tmin <- rescale(Weather_Data$tmin) 
Weather_Data_Complex$tavg <- rescale(Weather_Data$tavg)
Weather_Data_Complex$depart <- rescale(Weather_Data$depart)
Weather_Data_Complex$wetbulb <- rescale(Weather_Data$wetbulb)
Weather_Data_Complex$dewpoint <- rescale(Weather_Data$dewpoint)
Weather_Data_Complex$sunrise <- rescale(Weather_Data$sunrise)
Weather_Data_Complex$sunset <- rescale(Weather_Data$sunset)
Weather_Data_Complex$snowfall <- rescale(Weather_Data$snowfall)
Weather_Data_Complex$preciptotal <- rescale(Weather_Data$preciptotal)
Weather_Data_Complex$stnpressure <- rescale(Weather_Data$stnpressure)
Weather_Data_Complex$sealevel <- rescale(Weather_Data$sealevel)
Weather_Data_Complex$resultspeed <- rescale(Weather_Data$resultspeed)
Weather_Data_Complex$resultdir <- rescale(Weather_Data$resultdir)
Weather_Data_Complex$avgspeed <-  rescale(Weather_Data$avgspeed)

Weather_Data_Complex$codesum <- gsub("+", "", Weather_Data_Complex$codesum)


  

#Weather_Data_Complex$codesum <- gsub("+", "1", Weather_Data_Complex$codesum)
#Weather_Data_Complex$codesum <- gsub("-", "0.5", Weather_Data_Complex$codesum)

Weather_Data_Complex$codesum <- sum(Weather_Data_Complex$codesum)

Weather_Data_Complex <-  Weather_Data_Complex %>% 
  transform(codesum = sum(as.numeric(codesum)))