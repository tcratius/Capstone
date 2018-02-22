#~~~~~~~~~~~~
# Knots to Kmph https://www.convertunits.com/from/km/h/to/knots
#~~~~~~~~~~~
Knots_Kmph <- function(data){
  data <- (data * 1.852)
  return(data)
}