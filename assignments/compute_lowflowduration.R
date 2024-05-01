#' Low Flow Duration Metric
#'
#' Compute low flow duration metrics between observation and model
#' @param m model estimates
#' @param o observations
#' @param month month
#' @param day day
#' @param year year
#' @param wy water year
#' @param low_flow_threshold threshold for considering a flow value as low flow (default is 0.25)
#' @return annual_low_flow_days_err, annual_low_flow_days_cor

compute_lowflowduration <- function(m, o, month, day, year, wy, low_flow_threshold = 0.25) {
  flow <- cbind.data.frame(m, o, month, day, year, wy)
  
  # Calculate the number of low flow days for each year
  low_flow_days <- flow %>%
    group_by(wy) %>%
    summarize(model_low_flow_days = sum(m < low_flow_threshold),
              obs_low_flow_days = sum(o < low_flow_threshold))
  
  annual_low_flow_days_err <- mean(low_flow_days$model_low_flow_days - low_flow_days$obs_low_flow_days)
  annual_low_flow_days_cor <- cor(low_flow_days$model_low_flow_days, low_flow_days$obs_low_flow_days)
  
  return(list(annual_low_flow_days_err = annual_low_flow_days_err,
              annual_low_flow_days_cor = annual_low_flow_days_cor))
}