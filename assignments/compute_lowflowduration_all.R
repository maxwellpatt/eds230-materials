#' Compute Low Flow Duration Metrics (All)
#'
#' This function computes low flow duration metrics between the model estimates and observations.
#' It calculates the number of low flow days for each year based on a specified threshold,
#' computes the error in low flow days, transforms the error to a score between 0 and 1,
#' calculates the correlation between model and observed low flow days, and combines the metrics
#' using specified weights to create a single combined metric.
#'
#' @param m A vector of model estimates of streamflow.
#' @param o A vector of observed streamflow values.
#' @param month A vector of months corresponding to the streamflow values.
#' @param day A vector of days corresponding to the streamflow values.
#' @param year A vector of years corresponding to the streamflow values.
#' @param wy A vector of water years corresponding to the streamflow values.
#' @param low_flow_threshold A numeric value specifying the threshold for considering a flow value as low flow (default is 0.25).
#' @param wts A vector of two weights for combining the metrics (default is c(0.5, 0.5)).
#'
#' @return A list containing the following elements:
#'   - low_flow_days_err: The error in low flow days.
#'   - low_flow_days_cor: The correlation between model and observed low flow days.
#'   - combined_metric: The combined metric calculated using the specified weights.
#'
#'
compute_lowflowduration_all <- function(m, o, month, day, year, wy, low_flow_threshold = 0.25, wts = c(0.5, 0.5)) {
  flow <- cbind.data.frame(m, o, month, day, year, wy)
  
  # Calculate the number of low flow days for each year
  low_flow_days <- flow %>%
    group_by(wy) %>%
    summarize(model_low_flow_days = sum(m < low_flow_threshold),
              obs_low_flow_days = sum(o < low_flow_threshold))
  
  # Calculate the error in low flow days
  low_flow_days_err <- mean(abs(low_flow_days$model_low_flow_days - low_flow_days$obs_low_flow_days))
  
  # Transform the error to a score between 0 and 1
  max_error <- 0.5 * max(low_flow_days$obs_low_flow_days)
  low_flow_days_err_trans <- max(0, (1 - low_flow_days_err / max_error))
  
  # Calculate the correlation between model and observed low flow days
  if (length(unique(low_flow_days$model_low_flow_days)) > 1 && length(unique(low_flow_days$obs_low_flow_days)) > 1) {
    low_flow_days_cor <- cor(low_flow_days$model_low_flow_days, low_flow_days$obs_low_flow_days)
  } else {
    low_flow_days_cor <- NA
  }
  
  # Combine the metrics using weights
  if (!is.na(low_flow_days_cor)) {
    wts <- wts / sum(wts)
    combined_metric <- wts[1] * low_flow_days_err_trans + wts[2] * low_flow_days_cor
  } else {
    combined_metric <- NA
  }
  
  return(list(low_flow_days_err = low_flow_days_err,
              low_flow_days_cor = low_flow_days_cor,
              combined_metric = combined_metric))
}