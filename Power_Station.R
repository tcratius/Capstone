#~~~~~~~~~~~~
# Libraries
#~~~~~~~~~~~
library(tidyr)
library(dplyr)
library(ggplot2)
library(weathermetrics)

#~~~~~~~~~~~~
# Source
#~~~~~~~~~~~

#~~~~~~~~~~~~
# Input file
#~~~~~~~~~~~
csv <-"data/raw1/MajorPowerStations_v2.csv"
Power_Data <- read.csv(csv, header = TRUE, sep = ",", dec = ".")