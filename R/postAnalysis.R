# Post Process Analysis - Plotting

# --------------------------
# Function takes input: recorded discharge | estimated discharge |time
# Returns: combined data frame with interpolations
compareDischarge <- function(recorded_discharge_df, estimated_discharge_df){
  recorded <- data.frame(time = recorded_discharge_df$time, recDis = recorded_discharge_df$discharge)
  interpret <- stats::approx(recorded$time, recorded$recDis, xout = seq(0, max(recorded$time))) # interpolated
  recorded <- data.frame(time = interpret$x, recDis = interpret$y) # save interpolated data
  # Join the data
  discharge_DF <- dplyr::left_join(x = recorded, y = estimated_discharge_df, by = join_by("time")) # Currently left join, could be others
  return(discharge_DF)
}

