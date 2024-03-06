# Post Process Analysis

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

#-----------------------------
# Function takes input: data frame with columns Discharge Observations | Discharge Predictions
# Inputs: Expects two columns 1st being the obsever
# Optional: Efficiency type: NSE = Mean regression- Nashe-Sutcliffe
#                            KGE = Kling-Gupta
# Returns Model efficiency values
modelEfficiency <- function(dischargeDF, method = "NSE"){
  observed <- dischargeDF[,1]
  modeled <- dischargeDF[,2]
  if(method == "NSE"){
    # Assuming recorded is the first column with observed values
    difference <- sum((modeled - observed)^2)
    mean_diff <- sum((modeled - mean(observed))^2)
    NSE <- 1-difference/mean_diff
    return(round(NSE,4))
  }
  if(method == "KGE"){
    # Get correlation coefficient
    r_diff <- (sqrt(summary(stats::lm(observed ~ modeled))$r.squared) - 1)^2
    sd_squared <- (stats::sd(modeled)/stats::sd(observed) - 1)^2
    mean_squared <- (mean(modeled)/mean(observed) - 1)^2
    KGE <- 1 - sqrt(r_diff + sd_squared + mean_squared)
    return(round(KGE,4))
  }
}

# Test dataset
# obs <- c(3,5,2,4,5,6)
# model <- c(1,2,3,4,5,6)
# 
# obs1 <- c(1,2,3,4,5,6)
# model1 <- c(1,2,3,4,5,6)
# # Combined discharge values
# dischargeDF <- data.frame("recorded" = obs, "estimated" = model)
# dischargeDF1 <- data.frame("recorded" = obs1, "estimated" = model1)
# # Test
# NSE <- modelEfficiency(dischargeDF)
# KGE <- modelEfficiency(dischargeDF, method = "KGE")
# 
# NSE1 <- modelEfficiency(dischargeDF1)
# KGE1 <- modelEfficiency(dischargeDF1, method = "KGE")
