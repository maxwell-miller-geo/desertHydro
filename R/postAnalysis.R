# Post Process Analysis

# --------------------------
# Function takes input: recorded discharge | estimated discharge |time
# Returns: combined data frame with interpolations
compareDischarge <- function(recorded_discharge_df, estimated_discharge_df){
  recorded <- data.frame(time = recorded_discharge_df$time, recDis = recorded_discharge_df$discharge)
  interpret <- stats::approx(recorded$time, recorded$recDis, xout = seq(0, max(recorded$time))) # interpolated
  recorded <- data.frame(time = interpret$x, recDis = interpret$y) # save interpolated data

  # Find the maximum time in estimated
  simulation_length <- max(estimated_discharge_df$time)
  recorded_length <- max(recorded$time)
  time_offset <- simulation_length - recorded_length
  if(time_offset < 0){
    time_offset <- 0
  }
  # Janky way of adjusting the time values
  estimated_discharge_df$time <- estimated_discharge_df$time - time_offset
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
  if(is.character(dischargeDF)){
    dischargeDF <- as.data.frame(data.table::fread(dischargeDF))
  }
  if(inherits(discharge_data, "data.table")){
    dischargeDF <- as.data.frame(dischargeDF)
  }
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
totalVolume <- function(time, discharge, units = "mins"){
  if(length(time) == length(discharge)){
    if(units == "mins"){
      time <- diff(time)*60
    }
    mean_discharge <- (discharge[-length(discharge)] + discharge[-1])/2
    volume <- mean_discharge*time
    return(sum(volume))
  }else{
    return(NA)
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
  if(discharge & file.exists(file.path(ModelFolder, "rain-discharge.csv"))){
  # Determine discharge from volumes file
  discharge_file <- data.table::fread(file.path(ModelFolder, "volumes.csv"))
  # Determine extra length of discharge file
  simulation_duration <- max(discharge_file$Time_min)
  surface_discharge <- stream_gauge_discharge(discharge_file$gauge_height_cm, discharge_file$time_elapsed_s, units = "cm", raster = surfaceStorage)
  # Determine volumes
  rain_discharge <- data.table::fread(file.path(ModelFolder, "rain-discharge.csv"))
  estimated <- data.frame(time = discharge_file$Time_min, predDis = surface_discharge) # estimated discharge
  # Combined discharges for comparisons
  compareDis <- compareDischarge(rain_discharge, estimated) # outputs: time|recDis|predDis
  # save discharge
  discharge_save <- data.table::data.table(time = compareDis[,1], observed = compareDis[,2], xsection_1 = compareDis[,3])
  # Determine efficiency
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
    ggplot2::ggsave(filename = file.path(ModelFolder, paste0("Discharge-Outlet", date,".png")),
                    plot = dischargePlot, width = 5.5, height = 4)
  }
  data.table::fwrite(discharge_save, file = file.path(ModelFolder, "discharge-raw.csv"))
  # if(file.exists(x_sections_path) & discharge){
  #   print(paste("Creating discharge figures..."))
  #   rain_discharge <- readr::read_csv(file.path(ModelFolder, "rain-discharge.csv"), show_col_types = F)
  #   # Create rain-discharge excel sheet
  #
  #   #x_sections_path <- file.path(ModelElements, "Cross_Section_lines.shp")
  #   cross_section <- terra::vect(x_sections_path) # bring vector into R
  #   # # Extract the height from the surface stack
  #   surface_height_cm <- terra::extract(surfaceStorage, cross_section, method = "simple") # surface height in cm
  #   #surface_velocity <- terra::extract(velocityStorage, cross_section) # velocity at given time (m/s)
  #
  #   # cm_to_m2 <- .01 * 10 # conversion factor - Conversion to m time grid size
  #   # m3_to_ft3 <- 35.3147 # convert meters3 to feet3
  #   discharge_save <- data.table::data.table() # save empty data table because of loop scope
  #   for(x in 1:nrow(surface_height_cm)){
  #     # Calculate discharge
  #     #surface_discharge <- as.numeric(surface_height_cm[x,2:ncol(surface_height_cm)]* cm_to_m2 * m3_to_ft3 / time_elapsed)
  #     height <- as.numeric(surface_height_cm[x, 2:ncol(surface_height_cm)])
  #     xvalues <- round(as.numeric(colnames(surface_height_cm[x,2:ncol(surface_height_cm)])),4)
  #     surface_discharge <- stream_gauge_discharge(height, time_elapsed, units = "cm", raster = surfaceStorage)
  #     estimated <- data.frame(time = xvalues, predDis = surface_discharge)
  #     # Combined discharges for comparisons
  #     compareDis <- compareDischarge(rain_discharge, estimated) # outputs: time|recDis|predDis
  #     if(x == 1){
  #       discharge_save <- data.table::data.table(time = compareDis[,1], observed = compareDis[,2], xsection_1 = compareDis[,3])
  #     }else{ # save the observed discharges to be saved to a excel sheet
  #       discharge_save[, xsection_next := compareDis[,3]]
  #       lastname <- length(names(discharge_save))
  #       names(discharge_save)[lastname] <- paste0("xsection_", x)
  #     }
  #     method <- "NSE"
  #     modelScore <- modelEfficiency(compareDis, method = method)
  #     print(paste("Model efficiency via", method, "is", modelScore))
  #     dischargePlot <- ggplot2::ggplot() +
  #       ggplot2::geom_line(ggplot2::aes(x = compareDis$time, y = compareDis$recDis, color = "Recorded")) +
  #       ggplot2::geom_line(ggplot2::aes(x = compareDis$time, y = compareDis$predDis, color = "Predicted")) +
  #       ggplot2::labs(title = paste0("Predicted Discharge from Rainfall Event: ",  date),
  #            x = paste0("Time (minutes)"),
  #            y = paste0("Discharge (ft\u00b3/s)"),
  #            color = "Legend")
  #
  #     dischargePlot
  #     if(store){
  #       ggplot2::ggsave(filename = file.path(ModelFolder, paste0("compare-discharge-xsection-", x, "-", date,".png")),
  #              plot = dischargePlot, width = 5.5, height = 4)
  #     }
  #     if(x == nrow(surface_height_cm)){ # save the
  #       data.table::fwrite(discharge_save, file = file.path(ModelFolder, "discharge-raw.csv"))
  #     }
  #   }
  # }
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
      estimatedDischarge <- stream_gauge_discharge(surface_height_cm[x,], time_elapsed, raster = surfaceStorage)
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
#' @param rainfall_only T/F: If TRUE, will only create rainfall GIF
#' @return Returns list containing surface and velocity GIFS.
#' @export
#'
#' @examples \dontrun{
#' gifCreation(ModelFolder, saveGraph = T)
#' }
gifCreation <- function(ModelFolder, rainfall_method = "", date = NULL, discharge = F, saveGraph = T, rainfall_only = F){
  # Find date

  if(is.null(date)){
    inputs <- data.table::fread(file.path(ModelFolder, "input-variables.csv"))
    date <- inputs$date
    #date <- substr(basename(ModelFolder),1,10) # assumes first 10 are numbers
  }

  if(length(rainfall_method) < 2){
    inputs <- data.table::fread(file.path(ModelFolder, "input-variables.csv"))
    rainfall_method <- inputs$rainfall_method
  }
  # paths to storage layers
  surface_path <- file.path(ModelFolder, "surfaceStorage.tif")
  velocity_path <- file.path(ModelFolder, "velocityStorage.tif")
  subsurface_path <- file.path(ModelFolder, "soilStorage.tif")
  # Check if stacks and files exist
  # Surface
  if(file.exists(surface_path)){
    surfaceStorage <- terra::rast(surface_path)
    if(terra::nlyr(surfaceStorage) == 1){
      surfaceStorage <- rasterCompile(ModelFolder, "surface", remove = T)
    }
  }else{
    print(paste(surface_path, "does not exist \n"))
  }
  # Velocity
  if(file.exists(velocity_path)){
      velocityStorage <- terra::rast(velocity_path)
    if(terra::nlyr(velocityStorage) == 1){
      velocityStorage <- rasterCompile(ModelFolder, "velocity", remove = T)
    }
  }else{
    print(paste(velocity_path, "does not exist \n"))
  }
  # Soil
  if(file.exists(file.path(ModelFolder, "soilStorage.tif"))){
    subsurfaceStorage <- terra::rast(file.path(ModelFolder, "soilStorage.tif"))
    if(terra::nlyr(subsurfaceStorage) == 1){
      surfaceStorage <- rasterCompile(ModelFolder, "soil", remove = T)
    }
  }else{
    print(paste(surface_path, "does not exist \n"))
  }

  if(!rainfall_only){
  # Check rainfall method
  rain_file <- rainfallMethodCheck(ModelFolder, rainfall_method = rainfall_method)
  # Determine points taken by raster stack
  xvalues <- as.vector(stats::na.omit(as.numeric(names(surfaceStorage))))
  # Determine how rainfall is read in
  if(discharge && rainfall_method != "goes" && !is.null(date)){ # gathers total rain and rain duration values
    print("Retrieving rainfall data from simulation: rain_discharge")
    rainFiltered_file <- file.path(ModelFolder, paste0("rain-data-", date,".csv"))
    rainFiltered <- readr::read_csv(rainFiltered_file, show_col_types = F)
    total_rain <- round(sum(rainFiltered$Total_in), 3) # in inches
    total_rain_duration <- as.numeric((utils::tail(rainFiltered$Time_minute, n = 1) - rainFiltered$Time_minute[1])) # in hours
    rain <- readr::read_csv(rain_file, show_col_types = F)
  }else if(rainfall_method == "goes"){
    rain <- terra::rast(rain_file)
    total_rain <- max(terra::values(sum(rain)), na.rm = T) / 25.4 # rainfall in inches
    total_rain_duration <- (terra::nlyr(rain)-1) * 10 # rainfall minutes
  }else{
    print(paste("No rain-discharge data: Retrieving rainfall data from rainfile...", rain_file))
    rain <- readr::read_csv(file.path(ModelFolder, "Model-Rainfall.csv"), show_col_types = F)
    total_rain <- round(sum(rain$Total_in),3)
    total_rain_duration <- as.numeric((utils::tail(rain$time, n = 1) - rain$time[1]))
  }

    # Surface DEPTH
    print(paste("Creating surface depth animation..."))
    # Load in surface storage
    #surfaceStorage <- rast(file.path(ModelFolder, "Surface_Storage.tif"))
    # Use custom function to melt the raster stack into a dataframe for plotting
    meltedSurface <- meltStack(surfaceStorage, timevalues = xvalues) # Surface depths through time

    # Create an animated ggplot - Surface Storage
    surface_plot <- animateStack(meltedSurface,
                                 title = "Surface Depth",
                                 units = "Depth (cm)",
                                caption = paste0(round(total_rain,2)," inches of rain over ",  round(total_rain_duration, 2)," minutes"))
    # Display the animation
    #gganimate::animate(surface_plot)
    # store the animated GIF
    if(saveGraph){
      gganimate::anim_save(filename = paste0(date,"-surface-Depth.gif"), path = ModelFolder, animation = surface_plot, fps = 10, renderer = gganimate::gifski_renderer())
    }

    #--------------------------------------
    # Subsurface depths
    # Load in surface moisture
    print(paste("Creating soil moisture animation..."))
    if(file.exists(subsurface_path)){
      meltedMoistureContent <- meltStack(subsurfaceStorage, timevalues = xvalues) # subsurface % fill through time
      # Create an animated ggplot - Subsurface Storage
      subsurface_plot <- animateStack(meltedMoistureContent,
                                      title = "Moisture Content",
                                      units = "% Full",
                                      caption = paste0(round(total_rain,2)," inches of rain over ",
                                                       round(total_rain_duration, 2)," minutes."))

      # Display the animation
      #gganimate::animate(subsurface_plot)
      # store the animated GIF
      gganimate::anim_save(filename = paste0(date, "-moisture-content.gif"), path = ModelFolder, animation = subsurface_plot, fps = 10, renderer = gganimate::gifski_renderer())
    }else{
      print("Could not find not find soil raster.")
    }


    ## ------------------------------
    # Load in the velocity storage
    print(paste("Creating surface velocity animation..."))
    #velocityStorage <- rast(file.path(ModelFolder, "Velocities.tif"))
    velocityStack <- meltStack(velocityStorage, timevalues = xvalues) # subsurface % fill through time
    # Create an animated ggplot - Subsurface Storage
    velocity_plot <- animateStack(velocityStack,
                                  title = "Surface Velocities",
                                  units = "cm/s",
                                  caption = paste0(round(total_rain,2)," inches of rain over ", round(total_rain_duration, 2)," minutes"))
    # Display the animation
    #gganimate::animate(velocity_plot)
    # store the animated GIF
    if(saveGraph){
      gganimate::anim_save(filename = paste0(date,"-velocity.gif"), path = ModelFolder, animation = velocity_plot, fps = 10, renderer = gganimate::gifski_renderer())
    }
}
    # Rainfall - need date to do
    rainFiltered_file <- file.path(ModelFolder, paste0("rain-data-", date,".csv"))
    rainFiltered <- readr::read_csv(rainFiltered_file, show_col_types = F)
    if(rainfall_method == "goes"){
      # find potential goes files
      rain_options <- grep("-goes.tif", list.files(ModelFolder), value = T)
      # Select shortest one - over the .aux file
      rain_path <- file.path(ModelFolder, rain_options[which.min(nchar(rain_options))])
      if(file.exists(rain_path)){
        rain_surface <- terra::rast(rain_path)/6 # mm/hr *1hr/60mins * 10 mins
        xvalues <- 10*seq(terra::nlyr(rain_surface))
        #units <-  "Rainfall (mm hr\u207b\u00b9)"
        units <- "Rainfall (mm)"
        total_rain_duration <- tail(xvalues, 1)
        caption <- paste0("Rainfall intensity over ", round(total_rain_duration, 2)," minutes.")
      }else{
        stop("Could not find GOES rainfall file. Should by in 'ModelFolder' with format YYYY-MM-DD-goes.tif")
      }
    }
    if(rainfall_method == "gauges"){
      # Take the average of all the rainfall
      rain_per_time <- rainFiltered[,c("Time_minute","Total_in")]
      start_end <- get_start_end_time(rain_per_time)
      time_range <- difftime(start_end$end, start_end$start, units = "mins")[[1]]
      range <- data.frame(normalized_time = seq(0, time_range))
      rain_per_time$normalized_time <- as.numeric(difftime(rain_per_time$Time_minute, rain_per_time[1,1][[1]], units = "mins"))
      rain_per_time <- merge(range, rain_per_time,by = "normalized_time", all = T)
      rain_per_time[is.na(rain_per_time)] <- 0
      # select only two columns
      rain_df <- rain_per_time[, c("normalized_time", "Total_in")]
      # Create raster stack of values from the surface stack
      xvalues <- rain_df$normalized_time
      rain_surface <- (surfaceStorage[[1]] + 1)*rain_df$Total_in*25.4 # output in mm
      units <- "Rain (mm)"
      caption <- paste0("Rainfall from gauges over ", round(time_range, 2)," minutes.")
    }
    if(rainfall_method == "spatial"){
      rain_per_time <- rainFiltered[,c("Time_minute","WATER-1", "WATER-2", "WATER-G")]
      time_range <- difftime(tail(rain_per_time$Time_minute,1), rain_per_time$Time_minute[1], units = "mins")[[1]]
      range <- data.frame(normalized_time = seq(0, time_range))
      rain_per_time$normalized_time <- as.numeric(difftime(rain_per_time$Time_minute, rain_per_time[1,1][[1]], units = "mins"))
      rain_per_time <- merge(range, rain_per_time,by = "normalized_time", all = T)
      rain_per_time[is.na(rain_per_time)] <- 0
      # select only two columns
      rain_df <- rain_per_time[, c("normalized_time", "WATER-1", "WATER-2", "WATER-G")]
      # Janky way to load in the voronoi shapefile
      rain_regions <- terra::vect(filePresent("voronoi.shp", ModelFolder))
      gauges <- rain_df[,c("WATER-1", "WATER-2", "WATER-G")]*25.4
      rain_surface <- do.call(c, apply(gauges, MARGIN = 1, FUN = rasterizeRainfall, rain_regions, surfaceStorage[[1]]))
      xvalues <- rain_df$normalized_time
      units <- "Rain (mm)"
      caption <- paste0("Rainfall from gauges over ", round(time_range, 2)," minutes.")
    }

    # Check if file exists
    print(paste("Creating rainfall animation..."))
    # Use custom function to melt the raster stack into a dataframe for plotting
    meltedSurface <- meltStack(rain_surface, timevalues = xvalues) # Surface depths through time

    # Create an animated ggplot - Surface Storage
    rain_plot <- animateStack(meltedSurface,
                                 title = paste("Rainfall", date),
                                 units = units,
                                 caption = caption)
    # Display the animation
    gganimate::animate(rain_plot)
    # store the animated GIF
    if(saveGraph){
      gganimate::anim_save(filename = paste0(date,"-rain-Depth.gif"), path = ModelFolder, animation = rain_plot, fps = 10, renderer = gganimate::gifski_renderer())
    }
    return(NULL)

}
# Test
# gifCreation(ModelFolder, rainfall_method = "goes")

# Create function that looks at the discharge and rainfall data and determines certain characteristics
# Assumes column called Total_in
# load in volume

volume_over_time_plot <- function(ModelFolder, date){
  if(!file.exists(file.path(ModelFolder, "volumes.csv"))){
    return("Error: volumes.csv not found in model folder")
  }
  volumes <- data.table::fread(file.path(ModelFolder, "volumes.csv"))
  # Get total rainfall volume - sum of all the cells (depth of cell * cell dimensions)
  total_rain <- max(volumes[, cumulative_rain_cm]) # cumulative depth cm

  volume_plot <- ggplot2::ggplot(volumes) +
    ggplot2::geom_line(ggplot2::aes(x = Time_min, y = cumulative_surface_percent, color = "Surface")) +
    ggplot2::geom_line(ggplot2::aes(x = Time_min, y = cumulative_infiltration_percent, color = "Infiltrated")) +
    ggplot2::geom_line(ggplot2::aes(x = Time_min, y = cumulative_outflow_percent, color = "Discharge")) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(name = "Cumulative Percent (%)",
                                values = c("Surface" = "blue",
                                           "Infiltrated" ="green",
                                           "Discharge" = "purple")) +
    ggplot2::labs(title = paste("Water Budget:", date),
                  x = "Time (minutes)", y = "Percentage of Water Volume (%)") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  volume_plot

  ggplot2::ggsave(file.path(ModelFolder, paste0("water_budget_",date,".png")), plot = volume_plot,
                  width = 6, height = 2.5, units = "in")
  return("Plot Saved")
}

create_single_graphic <- function(ModelFolder, date = NULL){
  require(magick)

  if(is.null(date)){
    date <- substr(basename(ModelFolder),1,10) # assumes first 10 are numbers
  }
  # Read the GIFs
  gif1 <- magick::image_read(file.path(ModelFolder, paste0(date,"-moisture-content.gif")))
  gif2 <- magick::image_read(file.path(ModelFolder, paste0(date,"-rain-Depth.gif")))
  gif3 <- magick::image_read(file.path(ModelFolder, paste0(date,"-surface-Depth.gif")))
  gif4 <- magick::image_read(file.path(ModelFolder, paste0(date,"-velocity.gif")))
  #
  # Number of frames (assuming all have equal length)
  n_frames <- length(gif1)

  combined_frames <- vector("list", n_frames)

  for(i in 1:n_frames) {

    # Extract each frame and explicitly remove transparency matching the grey background
    frame1 <- image_background(gif1[i], "grey50") %>% image_flatten()
    frame2 <- image_background(gif2[i], "grey50") %>% image_flatten()
    frame3 <- image_background(gif3[i], "grey50") %>% image_flatten()
    frame4 <- image_background(gif4[i], "grey50") %>% image_flatten()

    # Ensure exactly equal dimensions for all frames
    target_size <- geometry_size_pixels(
      width = image_info(frame1)$width,
      height = image_info(frame1)$height
    )
    frame2 <- image_resize(frame2, target_size)
    frame3 <- image_resize(frame3, target_size)
    frame4 <- image_resize(frame4, target_size)

    # Combine horizontally and vertically
    top_row <- image_append(c(frame1, frame2))
    bottom_row <- image_append(c(frame3, frame4))
    combined_frame <- image_append(c(top_row, bottom_row), stack = TRUE)

    combined_frames[[i]] <- combined_frame
  }

  # Join frames without optimization to avoid hazy artifacts
  final_gif <- image_animate(image_join(combined_frames), fps = 10, optimize = FALSE)

  # Save final GIF
  image_write(final_gif, file.path(ModelFolder,paste0(date,"combined_gifs.gif")))
}

# Gets the NSE value, maximum discharge difference, and peak offset time for a single event
compare_discharge <- function(folder = NULL, completed_file = "ModelComplete.txt"){
  if(is.null(folder)){
    folder <- getwd()
  }
  # Check if model is completed
  completed_check <- file.path(folder, completed_file)
  if(file.exists(completed_check)){
    # Find the raw discharge data
    discharge_data <- data.table::fread(file.path(folder, "discharge-raw.csv"))
    NSE <- modelEfficiency(discharge_data)
    predicted_max <- max(discharge_data[,3])
    observed_max <- max(discharge_data[,2])
    peak_discharge_difference <- predicted_max - observed_max
    time_arrival_difference <- discharge_data[xsection_1 == predicted_max,time] -
                               discharge_data[observed == observed_max, time]
    volume_difference <- totalVolume(discharge_data$time, discharge_data$xsection_1) -
                         totalVolume(discharge_data$time, discharge_data$observed)
    out_data_table <- data.table::data.table(NSE = NSE,
                                             peak_discharge_difference = peak_discharge_difference,
                                             peak_discharge_time_offset_minutes = time_arrival_difference,
                                             volume_discharge_difference = volume_difference)
    data.table::fwrite(out_data_table, file.path(folder, "discharge-analysis.csv"))
    return(out_data_table)

  }else{
    print(paste("The folder:", folder, "does not have a completed model."))
    return(NULL)
  }
}

# Helper functions for post analysis stuff
get_discharge_volume <- function(time, disch){
  if(inherits(discharge_df, "data.table")){
    discharge_df <- as.data.frame(discharge_df)
  }
  # makes assumption modeled discharge is in the 3rd column
}

# Copy certain files over for post-analysis
copy_output_files <- function(folder = NULL, date = NULL){
  if(is.null(folder)){
    folder <- getwd()
  }
  # Get date from folder header
  if(is.null(date)){
    date <- substr(basename(folder), 1, 10)
  }
  outputs <- file.path(folder, paste0("Outputs-", basename(folder)))

  # Grab files needed
  output_files <- c("simulation_time.txt",
                    paste0(date, "-velocity.gif"),
                    paste0(date, "-surface-Depth.gif"),
                    paste0(date, "-moisture-content.gif"),
                    paste0(date, "-rain-Depth.gif"),
                    paste0("discharge_rain_", date, ".png"),
                    paste0("Discharge-Outlet-",date, ".png"),
                    "volumes.csv",
                    "model-checks.csv",
                    "Starting_Soil_Characteristics.csv",
                    paste0("Discharge-Outlet",date,".png"),
                    paste0("water_budget_", date, ".png"),
                    paste0("rainfall-comparison-",date,".png"),
                    "discharge-raw.csv")
  # Copy files to Output folder, if they exists
  lapply(file.path(folder, output_files), FUN = copy_if_exists, outputs)
  print("Copied output files")
}
