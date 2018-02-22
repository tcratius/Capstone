#~~~~~~~~~~~~
# Libraries
#~~~~~~~~~~~
library(tidyr)
library(dplyr)
library(ggplot)


csv <-"data/raw1/Walmart/wheather.csv"
Wther_Data <- read.csv(csv, header = TRUE, sep = ",", dec = ".")