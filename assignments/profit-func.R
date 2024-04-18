#' Calculate almond profit based on yield anomaly, base yield, and costs
#'
#' @param yield_anomaly_df A data frame containing the almond yield anomaly
#'   for each year. It must include columns for 'year' and 'yield_anomaly'.
#' @param base_yield Numeric, the base almond yield in tons per acre.
#' @param price Numeric, the price of almonds in dollars per ton.
#' @param total_cost Numeric, the total production cost per acre in dollars.
#' @param acres Numeric, the number of acres of almond production.
#'
#' @return A data frame with the following columns:
#' - year: Integer, the year of the observation.
#' - profit: Numeric, the final profit in dollars for each year.

almond_profit_detailed <- function(yield_anomaly_df, base_yield, price, total_cost, acres) {
  
  # Calculate total yield by adding the base yield to each yield anomaly
  yield_anomaly_df$yield <- base_yield + yield_anomaly_df$yield_anomaly
  
  # Calculate revenue and total cost
  yield_anomaly_df$revenue <- yield_anomaly_df$yield * price * acres
  yield_anomaly_df$total_cost <- total_cost * acres
  
  # Calculate profit
  yield_anomaly_df$profit <- yield_anomaly_df$revenue - yield_anomaly_df$total_cost
  
  return(yield_anomaly_df %>%
           select(year, profit))
}
