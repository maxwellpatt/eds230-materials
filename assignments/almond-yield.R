#' Calculate almond yield anomalies based on climate data
#'
#' The almond_model function calculates the almond yield anomaly for each year based on the
#' minimum February temperature and total January precipitation using the model
#' developed by Lobell et al.
#'
#' @param clim_df A data frame containing daily climate data that must
#' have the following columns:
#' - year: integer, year of the observation
#' - month: integer, month of the observation
#' - tmin_c: numeric, daily minimum temperature in degrees Celsius
#' - precip: numeric, daily precipitation in millimeters
#' @param params A list of model parameters with the following elements:
#' - intercept: numeric, intercept term of the model
#' - a: numeric, coefficient for the February minimum temperature term
#' - b: numeric, coefficient for the squared February minimum temperature term
#' - c: numeric, coefficient for the January precipitation term
#' - d: numeric, coefficient for the squared January precipitation term
#'
#' @return A data frame with the following columns:
#' - year: integer, year of the observation
#' - yield_anomaly: numeric, almond yield anomaly in tons per acre
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
  
  # Remove NA values from both years and yield_anom vectors
  valid_indices <- !is.na(yield_anom)
  years <- years[valid_indices]
  yield_anom <- yield_anom[valid_indices]
  
  # Create a data frame with year and yield_anomaly columns
  yield_anomaly_df <- data.frame(year = years, yield_anomaly = yield_anom)
  
  # Return the data frame
  return(yield_anomaly_df)
}