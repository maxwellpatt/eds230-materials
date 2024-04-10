#' Calculate almond yield anomalies based on climate data
#'
#' The almond_model function calculates the almond yield anomaly for each year based on the
#' minimum February temperature and total January precipitation using the model
#' developed by Lobell et al.
#'
#' @param clim_df A data frame containing daily climate data that must 
#' have the following columns:
#'   - year: integer, year of the observation
#'   - month: integer, month of the observation
#'   - tmin_c: numeric, daily minimum temperature in degrees Celsius
#'   - precip: numeric, daily precipitation in millimeters
#' @param params A list of model parameters with the following elements:
#'   - intercept: numeric, intercept term of the model
#'   - a: numeric, coefficient for the February minimum temperature term
#'   - b: numeric, coefficient for the squared February minimum temperature term
#'   - c: numeric, coefficient for the January precipitation term
#'   - d: numeric, coefficient for the squared January precipitation term
#'
#' @return A data frame with two columns:
#'   - year: integer, year of the yield anomaly
#'   - yield_anomaly: numeric, almond yield anomaly in tons per acre
#'
#' @references Lobell et al. (2006). Impacts of future climate change on California perennial crop yields: Model projections with climate and crop uncertainties

library(here)
library(dplyr)
library(tidyverse)

# Almond yield model function
almond_model <- function(clim_df, params) {
  
  # Extract unique years from the data
  years <- unique(clim_df$year)
  
  # Initialize a vector to store yield anomaly for each year
  yield_anom <- vector("numeric", length(years))
  
  # Calculate yield anomaly looping through years
  for (i in seq_along(years)) {
    T_min_Feb <- mean(clim_df$tmin_c[clim_df$month == 2 & clim_df$year == years[i]])
    P_Jan <- sum(clim_df$precip[clim_df$month == 1 & clim_df$year == years[i]])
    
    yield_anom[i] <- params$intercept +
      params$a * T_min_Feb +
      params$b * T_min_Feb^2 +
      params$c * P_Jan +
      params$d * P_Jan^2
  }
  
  # Return data frame with year and yield anomaly 
  return(data.frame(year = years, yield_anomaly = yield_anom))
}

# Define model parameters (from Table 2 in paper)
model_params <- list(
  intercept = 0.28,
  a = -0.015, 
  b = -0.0046,
  c = -0.07,
  d = 0.0043
)

# Read in climate data
clim_df <- read.table(here("Data/clim.txt"), header=TRUE)

# Run function 
yields <- almond_model(clim_df, model_params)

# Display summary stats of yield anomaly amounts
print(summary(yield_anom$yield_anomaly))