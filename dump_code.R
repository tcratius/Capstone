#~~~~~~~~~~~~
# Keep "M" missing values for station five
#~~~~~~~~~~~
Weather_Data_Filter <- Weather_Data %>%
  group_by(station_nbr) %>%
  filter(station_nbr == 5)
filter(tmax == 'M') %>%
  filter(tmin == 'M')

#~~~~~~~~~~~~
# Store Weather_Data_Filter contain station 5s Missing values for period 2012-2014
#~~~~~~~~~~~
write.table(Weather_Data_Filter, file = "data/raw1/Walmart/weather_stat5.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")


#~~~~~~~~~~~~
# Subtract the Weather_Data_Filter from Weather_Data 
#~~~~~~~~~~~
Weather_Data_Update <- Weather_Data[!(Weather_Data$station_nbr %in% Weather_Data_Filter$station_nbr), ]

#~~~~~~~~~~~~
# Output to file 
#~~~~~~~~~~~
write.table(Weather_Data_Update, file = "data/raw1/Walmart/weather_leftover_stations.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

rm(Weather_Data_Filter, Weather_Data_Update) # Remove file











#~~~~~~~~~~~~
# Input file, replace "M" with NA
#~~~~~~~~~~~
csv <-"data/raw1/Walmart/weather2.csv"
Weather_Data <- read.csv(csv, header = TRUE, sep = ",", dec = ".", na.strings = "M")

#~~~~~~~~~~~~
# Output to file Raw1 and remove Dataframe
#~~~~~~~~~~~
write.table(Weather_Data, file = "data/raw1/Walmart/weather2.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
            qmethod = c("escape", "double"), fileEncoding = "")

rm(Weather_Data)

Weather_Data$date <- as.Date(Weather_Data$date, format = "%Y-%m-%d")

Weather_Data <- Weather_Data %>% 
  filter(date >= as.Date("2012-01-01") & date <= as.Date("2012-06-30"))  # ok got it to work

sub_Weather_Data <- Weather_Data %>%
  filter(date >= as.Date("2013-01-01") & date <= as.Date("2013-06-30"))

colSums(is.na(Weather_Data))
length(Weather_Data$sunrise) # 20517
colSums(Weather_Data == "M")
Dum <- Weather_Data %>%
  filter(snowfall != "M" & cool != "M" )

Weather_Data_knn$codesum <- gsub("\\+", "", Weather_Data_knn$codesum)
Weather_Data_knn$codesum <- gsub("\\ ", "", Weather_Data_knn$codesum)

Weather_Data_knn <- Weather_Data_knn %>%
  transform(codesum = sum(as.numeric(codesum)))
