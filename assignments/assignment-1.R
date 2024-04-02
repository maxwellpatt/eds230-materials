# Calculate energy produced from a photovoltaic system
#
# This function calculates the energy produced from a photovoltaic system based on
# the solar panel area, panel yield, performance ratio, and annual average solar radiation.
#
# @param A (numeric): The solar panel area in square meters (m^2).
# @param H (numeric): The annual average solar radiation in kilowatt-hours (kWh).
# @param r (numeric): The panel yield, a value between 0 and 1, representing the manufacturing efficiency.
#                     Default value is 0.2.
# @param PR (numeric): The performance ratio, a value between 0 and 1, accounting for site factors that impact efficiency. Default value is 0.75.
#
# @return The energy produced in kilowatt-hours (kWh).
#
# @examples
# photovoltaic_energy(A = 100, H = 1500)
# photovoltaic_energy(A = 200, H = 1800, r = 0.22, PR = 0.8)
#

photovoltaic_energy <- function(A, H, r = 0.2, PR = 0.75) {
  energy <- A * r * H * PR
  return(energy)
}
