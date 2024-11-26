# Post Process Analysis

# --------------------------
# Function takes input: recorded discharge | estimated discharge |time
# Returns: combined data frame with interpolations
compareDischarge <- function(recorded_discharge_df, estimated_discharge_df){
  recorded <- data.frame(time = recorded_discharge_df$time, recDis = recorded_discharge_df$discharge)
  interpret <- stats::approx(recorded$time, recorded$recDis, xout = seq(0, max(recorded$time))) # interpolated
  recorded <- data.frame(time = interpret$x, recDis = interpret$y) # save interpolated data
  # Join the data
  discharge_DF <- dplyr::left_join(x = recorded, y = estimated_discharge_df, by = dplyr::join_by("time")) # Currently left join, could be others
  # Fill the first predicted discharge with 0. Assumes Prediction in 1st row, 3rd column is 0
  discharge_DF[1,3] <-  0
  return(discharge_DF)
}

#-----------------------------
# Function takes input: data frame with columns Discharge Observations | Discharge Predictions
# Inputs: Expects two columns 1st being the obsever
# Optional: Efficiency type: NSE = Mean regression- Nashe-Sutcliffe
#                            KGE = Kling-Gupta
# Returns Model efficiency values
modelEfficiency <- function(dischargeDF, method = "NSE", timeCol = T){
  if(timeCol){ # assumes column order time | obs | modeled
    observed <- dischargeDF[,2]
    modeled <- dischargeDF[,3]
  }else{
    observed <- dischargeDF[,1]
    modeled <- dischargeDF[,2]
  }

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

## --------------------- Total volume differences
totalVolume <- function(time, discharge){
  if(length(time) == length(discharge)){
    sumVolume <- 0
    arrayLength <- length(time) - 1
    for(x in 1:arrayLength){
      dt <- time[x+1] - time[x]
      mean <- mean(c(discharge[x+1], discharge[x]))
      vol <- mean * dt
      # dDischarge <- discharge[x+1] - discharge[x]
      # triangle <- dt * dDischarge / 2
      # rectangle <- dt * discharge[x]
      # area <- triangle + rectangle
      sumVolume <- sumVolume + vol
    }
    return(sumVolume)
  }
}
# time <- c(0,1,2,3)
# discharge <- c(0,1,2,1)
# dV <- totalVolume(time, discharge)

## ---------------------------- Discharge Figures
# Function that creates discharge figures
#' Discharge Analysis after desertHydro simulation
#'
#' @param ModelFolder Model Folder with saved outputs
#' @param WatershedElements Watershed Folder with Watershed components. Assumes that
#' @param time_step Time in minutes
#' @param simulation_length Length of simulation in minutes
#' @param discharge T/F: Default TRUE: If TRUE, will look for observed discharge
#' hydrographs
#' @param store T/F: Default TRUE: If TRUE, will store created plots in
#' ModelFolder
#' @param date string. Optional plotting setting to add date to title and output
#' file.
#' @param gauge_locations path to shapefile that contains gauge locations for
#' discharge comparisons. Default location checks Watershed Elements folder for
#' "stream_gauge.shp"
#' @return Returns discharge excel table and plots from model simulation
#' @export
#'
#' @examples \dontrun{
#' dischargeAnalysis(ModelFolder, WatershedElements, time_step, simulation_length) #See vignette
#' }
#'
dischargeAnalysis <- function(ModelFolder, WatershedElements, discharge = F, store = T, date = NULL, gauge_locations = file.path(WatershedElements, "stream_gauge.shp"), units = "cm/s",...){
  time <- Total_in <- xsection_next <- NULL # keep the global variables at bay
  surfaceStorage <- terra::rast(file.path(ModelFolder, "surfaceStorage.tif"))
  time_elapsed <- extract_time(surfaceStorage) # time elapsed - seconds
  #velocityStorage <- terra::rast(file.path(ModelFolder, "velocityStorage.tif"))
  #subsurfaceStorage <- terra::rast(file.path(ModelFolder, "soilStorage.tif"))
  #gauge_locations = file.path(WatershedElements, "stream_gauge.shp")
  x_sections_path <- gauge_locations
  if(file.exists(x_sections_path) & discharge){
    print(paste("Creating discharge figures..."))
    rain_discharge <- readr::read_csv(file.path(ModelFolder, "rain-discharge.csv"), show_col_types = F)
    # Create rain-discharge excel sheet

    #x_sections_path <- file.path(ModelElements, "Cross_Section_lines.shp")
    cross_section <- terra::vect(x_sections_path) # bring vector into R
    # # Extract the height from the surface stack
    surface_height_cm <- terra::extract(surfaceStorage, cross_section, method = "simple") # surface height in cm
    #surface_velocity <- terra::extract(velocityStorage, cross_section) # velocity at given time (m/s)


    # cm_to_m2 <- .01 * 10 # conversion factor - Conversion to m time grid size
    # m3_to_ft3 <- 35.3147 # convert meters3 to feet3
    discharge_save <- data.table::data.table() # save empty data table because of loop scope
    for(x in 1:nrow(surface_height_cm)){
      # Calculate discharge
      #surface_discharge <- as.numeric(surface_height_cm[x,2:ncol(surface_height_cm)]* cm_to_m2 * m3_to_ft3 / time_elapsed)
      height <- as.numeric(surface_height_cm[x, 2:ncol(surface_height_cm)])
      xvalues <- round(as.numeric(colnames(surface_height_cm[x,2:ncol(surface_height_cm)])),4)
      surface_discharge <- stream_gauge_discharge(height, time_elapsed, units = "cm")
      estimated <- data.frame(time = xvalues, predDis = surface_discharge)
      # Combined discharges for comparisons
      compareDis <- compareDischarge(rain_discharge, estimated) # outputs: time|recDis|predDis
      if(x == 1){
        discharge_save <- data.table::data.table(time = compareDis[,1], observed = compareDis[,2], xsection_1 = compareDis[,3])
      }else{ # save the observed discharges to be saved to a excel sheet
        discharge_save[, xsection_next := compareDis[,3]]
        lastname <- length(names(discharge_save))
        names(discharge_save)[lastname] <- paste0("xsection_", x)
      }
      method <- "NSE"
      modelScore <- modelEfficiency(compareDis, method = method)
      print(paste("Model efficiency via", method, "is", modelScore))
      dischargePlot <- ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(x = compareDis$time, y = compareDis$recDis, color = "Recorded")) +
        ggplot2::geom_line(ggplot2::aes(x = compareDis$time, y = compareDis$predDis, color = "Predicted")) +
        ggplot2::labs(title = paste0("Predicted Discharge from Rainfall Event: ",  date),
             x = paste0("Time (minutes)"),
             y = paste0("Discharge (ft\u00b3/s)"),
             color = "Legend")

      dischargePlot
      if(store){
        ggplot2::ggsave(filename = file.path(ModelFolder, paste0("compare-discharge-xsection-", x, "-", date,".png")),
               plot = dischargePlot, width = 5.5, height = 4)
      }
      if(x == nrow(surface_height_cm)){ # save the
        data.table::fwrite(discharge_save, file = file.path(ModelFolder, "discharge-raw.csv"))
      }
    }
  }else if(file.exists(x_sections_path)){
    print(paste("Creating discharge figures..."))
    #x_sections_path <- file.path(ModelElements, "Cross_Section_lines.shp")
    cross_section <- terra::vect(x_sections_path) # bring vector into R
    # Gather coordinates from vector
    #coords <- as.matrix(terra::geom(cross_section))[,3:4]
    #surfaceStorage <- surfaceStorage[1]
    # # Extract the height from the surface stack
    surface_height_cm <- terra::extract(surfaceStorage, cross_section)
    # Does this work with multiple rows - noooo
    surface_height_cm$ID <- 0 # surface height in cm
    colnames(surface_height_cm)[1] <- "0" # adjust first column to 0
    xvalues <- as.numeric(colnames(surface_height_cm))
    #surface_height_cm <- c(0, as.numeric(surface_height_cm[,2:ncol(surface_height_cm)]))
    # surface_velocity <- terra::extract(velocityStorage, cross_section) # velocity at given time (m/s)
    # surface_velocity[is.na(surface_velocity)] <- 0
    # surface_velocity <- surface_velocity[,2:ncol(surface_velocity)]
    for(x in 1:nrow(surface_height_cm)){
      estimatedDischarge <- stream_gauge_discharge(surface_height_cm[x,], time_elapsed)
      #estimated <- as.numeric(surface_height_cm[x,2:ncol(surface_height_cm)] * cm_to_m2 * surface_velocity[x,2:ncol(surface_velocity)] * m3_to_ft3) # m^3/s
      #xvalues <- seq(time_step, simulation_length, by = time_step)
      dischargePlot <- ggplot2::ggplot() +
        #geom_line(aes(x = compareDis$time, y = compareDis$recDis, color = "Recorded")) +
        ggplot2::geom_line(ggplot2::aes(x = xvalues, y = estimatedDischarge, color = "Predicted")) +
        ggplot2::labs(title = paste0("Predicted Discharge from Rainfall Event: ",  date),
             x = paste0("Time (minutes)"),
             y = paste0("Discharge (ft\u00b3/s)"),
             color = "Legend")
      dischargePlot
      if(store){
        ggplot2::ggsave(filename = file.path(ModelFolder, paste0("discharge-prediction","-xsection-", x, "-", date,".png")),
               plot = dischargePlot, width = 5.5, height = 4)
      }
    }
  }
  plot(dischargePlot)
  print("Discharge complete...")
}
# Test
#dischargeAnalysis(ModelFolder, WatershedElements)


## ----------------------------------- GIF Creation
# Function to create gifs from stacked raster layers
#' Create GIFS
#'
#' @param ModelFolder Location to save Model Folder
#' @param rainfall_method String: "guages","spatial", "synthetic", "goes based on
#' the input rainfall method
#' @param date String: Optional, will display the rainfall totals in GIF
#' @param discharge T/F: If T, will incorporate discharge data
#' @param saveGraph T/F: If T, will save graph to ModelFolder
#'
#' @return Returns list containing surface and velocity GIFS.
#' @export
#'
#' @examples \dontrun{
#' gifCreation(ModelFolder, saveGraph = T)
#' }
gifCreation <- function(ModelFolder, rainfall_method = "", date = NULL, discharge = F, saveGraph = T){

  surfaceStorage <- terra::rast(file.path(ModelFolder, "surfaceStorage.tif"))
  velocityStorage <- terra::rast(file.path(ModelFolder, "velocityStorage.tif"))
  #subsurfaceStorage <- terra::rast(file.path(ModelFolder, "soilStorage.tif"))

  # Check if stacks have been created
  if(terra::nlyr(surfaceStorage) == 1){
    surfaceStorage <- rasterCompile(ModelFolder, "surface", remove = T)
  }
  if(terra::nlyr(velocityStorage) == 1){
    velocityStorage <- rasterCompile(ModelFolder, "velocity", remove = T)
  }
  rain_file <- rainfallMethodCheck(ModelFolder, rainfall_method = rainfall_method)

  xvalues <- as.vector(stats::na.omit(as.numeric(names(surfaceStorage))))

  if(discharge & rainfall_method != "goes"){ # gathers total rain and rain duration values
    print("Retrieving rainfall data from simulation: rain_discharge")
    rainFiltered_file <- file.path(ModelFolder, paste0("rain-data-", date,".csv"))
    rainFiltered <- readr::read_csv(rainFiltered_file, show_col_types = F)
    total_rain <- round(sum(rainFiltered$Total_in),3)
    total_rain_duration <- as.numeric((utils::tail(rainFiltered$Time_minute, n = 1) - rainFiltered$Time_minute[1]))
    rain <- readr::read_csv(rain_file, show_col_types = F)
  }else if(rainfall_method == "goes"){
    rain <- terra::rast(rain_file)
    total_rain <- max(terra::values(sum(rain)), na.rm = T) / 25.4 # rainfall in inches
    total_rain_duration <- (terra::nlyr(rain) -1) * 10 # rainfall minutes
  }else{
    print(paste("No rain-discharge data: Retrieving rainfall data from rainfile...", rain_file))
    rain <- readr::read_csv(file.path(ModelFolder, "Model-Rainfall.csv"), show_col_types = F)
    total_rain <- round(sum(rain$Total_in),3)
    total_rain_duration <- as.numeric((utils::tail(rain$time, n = 1) - rain$time[1]))
  }

    print(paste("Creating surface depth animation..."))
    # Load in surface storage
    #surfaceStorage <- rast(file.path(ModelFolder, "Surface_Storage.tif"))
    # Use custom function to melt the raster stack into a dataframe for plotting
    meltedSurface <- meltStack(surfaceStorage, timevalues = xvalues) # Surface depths through time

    # Create an animated ggplot - Surface Storage
    surface_plot <- animateStack(meltedSurface,
                                 title = "Surface Depth",
                                 units = "Depth (cm)",
                                caption = paste0(total_rain," inches of rain over ",  round(total_rain_duration, 2)," minutes."))
    # Display the animation
    #gganimate::animate(surface_plot)
    # store the animated GIF
    if(saveGraph){
      gganimate::anim_save(filename = paste0(date,"-surface-Depth.gif"), path = ModelFolder, animation = surface_plot, fps = 10, renderer = gganimate::gifski_renderer())
    }

    ##--------------------------------------
    # Subsurface depths
    #Load in surface moisture
    # print(paste("Creating soil moisture animation..."))
    # if(terra::nlyr(subsurfaceStorage) > 1){
    #   meltedMoistureContent <- meltStack(subsurfaceStorage, timevalues = xvalues) # subsurface % fill through time
    #   # Create an animated ggplot - Subsurface Storage
    #   subsurface_plot <- animateStack(meltedMoistureContent,
    #                                   title = "Moisture Content",
    #                                   units = "% Full",
    #                                   caption = paste0(total_rain," inches of rain over ",
    #                                                    round(total_rain_duration, 2)," minutes."))
    #
    #   # Display the animation
    #   #gganimate::animate(subsurface_plot)
    #   # store the animated GIF
    #   gganimate::anim_save(filename = paste0(date, "-moisture-content.gif"), path = ModelFolder, animation = subsurface_plot, fps = 10, renderer = gganimate::gifski_renderer())
    # }


    ## ------------------------------
    # Load in the velocity storage
    print(paste("Creating surface velocity animation..."))
    #velocityStorage <- rast(file.path(ModelFolder, "Velocities.tif"))
    velocityStack <- meltStack(velocityStorage, timevalues = xvalues) # subsurface % fill through time
    # Create an animated ggplot - Subsurface Storage
    velocity_plot <- animateStack(velocityStack,
                                  title = "Surface Velocities",
                                  units = "m/s",
                                  caption = paste0(total_rain," inches of rain over ", round(total_rain_duration, 2)," minutes."))
    # Display the animation
    #gganimate::animate(velocity_plot)
    # store the animated GIF
    if(saveGraph){
      gganimate::anim_save(filename = paste0(date,"-velocity.gif"), path = ModelFolder, animation = velocity_plot, fps = 10, renderer = gganimate::gifski_renderer())
    }
    return(list(surface_plot, velocity_plot))

}
# Test
# gifCreation(ModelFolder, rainfall_method = "goes")


