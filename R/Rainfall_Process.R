# # Functions to process Rainfall Data
# # Rainfall can come in 2 non-spatial varieties: constant and time-dependent
# library(readxl)
# library(tidyverse)
# library(purrr)
# library(lubridate)

## Rainfall Data to bring over
# Items | Model Folder | Options |
# Synthetic - Dummy Data present within function
# Rainfall Data from Gauges
# Weighted Data from Gauges
# Spatially Distributed Rainfall
# Always returns a rainfile path - Expects within the Model Folder
rainfallCreation <- function(ModelFolder, WatershedElements, date = NULL, method = "gauges", weighted = T, overwrite = F){
  print("Rainfall...")
  rain_file <- file.path(ModelFolder, "Model-Rainfall.csv")
  rain_spatial_file <- file.path(ModelFolder, "Model-Spatial-Rainfall.csv")
  rain_goes_file <- file.path(ModelFolder, "goes-rainfall.tif")
  rainFiltered_file <- file.path(ModelFolder, paste0("rain-data-", date,".csv"))
  rainFilterFile <- file.exists(rainFiltered_file)
  # Check for files that have been written
  if(!overwrite){
    if(file.exists(rain_file) & (method == "gauges") & rainFilterFile){
      print("Found a model rainfall...")
      return(rain_file)
    }
    if(file.exists(rain_spatial_file) & (method == "spatial") & rainFilterFile){
      print("Found a spatial rainfall file...")
      return(rain_spatial_file)
    }
    if(file.exists(rain_goes_file) & (method == "goes") & rainFilterFile){
      print("Found GOES processed file")
      return(rain_goes_file)
    }
    if(file.exists(rain_file) & (method == "synthetic")){
      print("Found a synthetic rainfall...")
      return(rain_file)
    }
  }
  if(TRUE){
    # Synthetic rainfall method
    if(method == "synthetic" | is.null(date)){
      print("Using synthetic data")
      # Rainfall for a constant amount
      #date <- "Synthetic"
      total_rain_duration <- 15
      #total_rain <- mean(c(.51, .12, .37))
      total_rain <- .5
      rain_step <- 0.05 # time interval of rainfall (minutes) - should be less than default model time step

      rain_duration <- seq(0, total_rain_duration, rain_step) # rainfall in minutes
      rainfall_amount_per_step <- (total_rain/total_rain_duration)*rain_step # amount of rainfall (rainfall/min) * (min)
      rainfall_rate <- c(0, rep(rainfall_amount_per_step, total_rain_duration/rain_step))
      rain <- cbind.data.frame(rain_duration, rainfall_rate) # Time(minutes) | Rain (in)
      colnames(rain) <- c("time", "Total_in")
      # Create a csv rainfile
      #rain_file <- file.path(ModelFolder, paste0(method, "-Rainfall.csv"))
      readr::write_csv(rain, rain_file)
      return(rain_file)
    }
    # Rainfall filtering
    rainFiltered <- rainfallFilter(date, ModelFolder, WatershedElements, overwrite = T)
    #test <- rainfallFilter(date, ModelFolder, WatershedElements, overwrite = F)

    # Normalize the rainfall data
    if(method == "gauges"){
      # Weight the input rainfall
      if(weighted){
        # Weights from voronoi calculations order is: WATER-1, WATER-2, WATER-G
        weights <- c(.49254, .40299, .10448)
        #weights <- c(.33333, .33333, .33333) # equally weighted
        rainFiltered$`WATER-1` <- rainFiltered$`WATER-1` * weights[1]
        rainFiltered$`WATER-2` <- rainFiltered$`WATER-2` * weights[2]
        rainFiltered$`WATER-G` <- rainFiltered$`WATER-G` * weights[3]

        # Readjust the total column
        rainFiltered$Total_in <- rowSums(rainFiltered[, c("WATER-1", "WATER-2", "WATER-G")])
      }
      #Save rainfall to model folder
      rainNormal <- rainFiltered |>
        dplyr::select(time, Total_in) |>
        round(4) # round columns to 4 decimals

      readr::write_csv(rainNormal, rain_file)
      print("Rainfall created...")
      return(rain_file)
    }
    # Spatial distributed rainfall - a little janky just returns rainfall- not rain-discharge
    if(method == "spatial"){ # create table with time | Water-1 | Water-2 | Water-G
      cols <- c("WATER-1", "WATER-2", "WATER-G")
      spatial_rain <- rainFiltered |>
        dplyr::select(Time_minute, time, all_of(cols)) |> # Select relevant columns
        dplyr::add_row(Time_minute = c(rainFiltered[1,1] - lubridate::minutes(1)),
                       `WATER-1` = 0,
                       `WATER-2` = 0,
                       `WATER-G` = 0,
                       time = 0, .before = 1) |>
        dplyr::select(time, cols) |>
        round(5)

      # Save the output
      readr::write_csv(spatial_rain, rain_spatial_file)
      print("Spatial Rainfall created...")
      return(rain_spatial_file)
    }
    # GOES Rainfall
    if(method == "goes"){
      temporalFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data)"
      rainFolder <- file.path(temporalFolder, paste0(date,"-rain"))
      goes_Rain <- goesRain(date, rainFolder, WatershedElements)
      return(goes_Rain)
    }
  }
}
# Test
# z <- rainfallCreation(ModelFolder = ModelFolder, WatershedElements = WatershedElements,
#                  date = "2012-07-15", method = "synthetic", overwrite = F)
# # Outputs - Rainfall.csv file for model
# # Check if rainfall is already created
# rain_file <- file.path(ModelFolder, paste0(date, "-Rainfall.csv"))
# rain_spatial_file <- file.path(ModelFolder, paste0(date, "-Spatial-Rainfall.csv"))
## ----------------- Rainfall per time step
rainfallAccum <- function(rain, beginning_time, end_time, rainfall_method = "gauges"){
  if(rainfall_method == "spatial"){
    # Get rainfall from shape
    rainForGauges <- cumulativeRain(rain, left = beginning_time, right = end_time, spatial = T)
    # Could adjust voronoi
    rainfall_for_timestep <- rasterizeRainfall(voronoi_shape = "voronoi.shp",
                                               rainAtGauges = rainForGauges,
                                               rainfallRaster = terra::rast(file.path(WatershedElements, "model_dem.tif")))
    return(rainfall_for_timestep)
  }else if(rainfall_method == "goes"){
    if(beginning_time == 1){
      rainfall_for_timestep <- 0
      return(rainfall_for_timestep)
    }else{
      goesRain <- terra::rast(rain)
      layerSelection <- ceiling(beginning_time/10)+1
      mm_to_in <- 0.0393701
      timeElapsed <- end_time - beginning_time
      rainfall_for_timestep <- goesRain[[layerSelection]] / (10/timeElapsed) * mm_to_in # rain fallen in inches
      return(rainfall_for_timestep)
    }
  }else{
    rainfall_for_timestep <- cumulativeRain(rain, left = beginning_time, right = end_time)
    return(rainfall_for_timestep)
  }
}
# Formats rainfall csv file from time/cumulative amount into time/rainfall-rate
rainfallProcess <- function(rainfall, filepath = T, inches = T){
  # Assuming first line is a zero
  # Assumes data structure: Time (min) | Cumulative Rain (in)
  if(filepath){
    rain <- as.data.frame(read_csv(rainfall_file, show_col_types = FALSE))
  }
  else{
    rain <- rainfall
  }
  # Determine the maximum rainfall
  max_rain_depth <- max(rain[,2])
  time_diff <- diff(rain[,1]) # in minutes
  # Normalize the rainfall data
  rain[,2] <- rain[,2] / max_rain_depth
  # Calculate rainfall rates between measured steps
  rainfall_rates <-  diff(rain[,2])/ time_diff # rainfall amount/Time (in/min)

  # Output
  time_min <- seq(0, max(rain[,1]), by = 1) # create sequence in 1 minute intervals until the end of the rain
  rainfall_rate <- c(0, rep(rainfall_rates, time_diff))

  # return combined data frame of time with minute intervals and normalized rainfall
  return(cbind.data.frame(time_min, rainfall_rate))
}
# Test 2
# rainfallDayEvent <- rainfallProcess(rain)
# # Test rainfall process function
# rainfall_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\1hr_1in_initial\Rainfall.csv)"
# rainfallProcess(rainfall_file)
# Example:
# rainfall_times <- c(0, 1, 2, 3, 5, 10, 40, 60) # rainfall steps in minutes
# rainfall_percent <- c(0, .01, .03, .05, .1, .25, .5, 1) # percent rainfall over time
# rainfall_test <- cbind.data.frame(rainfall_times,rainfall_percent)
# rainfallProcess(rainfall_test, filepath = F)

# Get the total rainfall for a particular day
rainfallTotal <- function(rainfall, filepath = T, inches = T){
  rain <- as.data.frame(read_csv(rainfall_file, show_col_types = FALSE))
  # assuming second column is the rainfall amount
  rainfall_total <- dplyr::last(rain[,2]) - rain[1,2]
  return(rainfall_total)
}
# Test rainfall process function
# rainfall_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\1hr_1in_initial\Rainfall.csv)"
# rainfallTotal(rainfall_file)

# Function that takes rainfall data from a watersheds gauges and gathers total rainfall
rainfallTotalRain <- function(rainfall_folder, date, level = "day", gauges = c("WATER-1", "WATER-2", "WATER-G"),  write = T){

  if(substr(date, 1, 4) == "2022"){ # Year 2022
    rainfall_file <- file.path(rainfall_folder, "USGS_Rain_2022.xlsx")
  }else{
    rainfall_file <- file.path(rainfall_folder, "USGS-GCMRC rain-gauge data WY 2000_2021.xlsx")
  }
  # if(new){
  #   rainfall_file <- file.path(rainfall_file, "USGS_Rain_2023.xlsx")
  # } # year 2023

  # Load in the rainfall file - excel sheet
  rain <- rainfall_file |>
    excel_sheets() |>
    set_names() |>
    map(read_excel, path = rainfall_file)

  # Select the rainfall gauges in the list
  rainSelected <- rain[gauges]

  # Dirty way to adjust the cumulative amounts into differences
  for(x in 1:length(rainSelected)){
    # print(x)
    # Determine which column contains the Cumulative Rain
    idx <-  which(colnames(rainSelected[[x]]) == "TOTAL Cumulative Rain (in)", arr.ind = T)
    if(idx > 0){
      y <- idx
    }
    # create vector of differences
    differences <- c(0, diff(rainSelected[[x]][[y]]))
    # If differences are greater than 100 - set equal to 0
    differences[differences > 100] <- 0
    #rainSelected[[x]]$`TOTAL Cumulative Rain (in)`
    rainSelected[[x]][[y]] <- round(differences,2)  # Assign the rainfall to the increment recorded
    rainSelected[[x]] <- rainSelected[[x]] |>
      dplyr::mutate(date_time = `Date and time`, rain_in = `TOTAL Cumulative Rain (in)`) |>
      dplyr::select(date_time, rain_in) |> # reorganize the list for a particular gauge
      stats::aggregate(rain_in ~ floor_date(date_time, unit = level), FUN = sum) |>
      dplyr::rename(date = "floor_date(date_time, unit = level)")

      #mutate(month = lubridate::month(date_time, label = T)) # Create monthly column
  }
  # Join all of the rainfall amounts together
  joinedDF <- dplyr::full_join(rainSelected[[1]],rainSelected[[2]],  by = "date") |>
    dplyr::full_join(rainSelected[[3]], by = "date")
  # #return(rainSelected)
  # return(joinedDF)
  # #rainDay <- plyr::join_all(rainSelected, by = "date", type = "full")
  # rainDay <- dplyr::full_join(rainSelected, by = "date", type = "full")
  #return(rainDay)
  names(joinedDF) <- c(paste0("Time_",level), gauges)
  joinedDF[is.na(joinedDF)] <- 0 # Fill NA values with zero
  #rainDay[(rainDay >= 100)] <- 0 # take take of extraneous values

  ## !Filter the date out if desired
  # if(!is.null(date)){
  #   date <- ymd(date)
  #   rainDay <- rainfallForEvent(rainDF = rainDay, eventDate = date)
  # }
  # Sum the rainfall for a particular time-step
  joinedDF$Total_in <- rowSums(joinedDF[gauges])
  outDF <- joinedDF
  outputName <- paste0("rain-data-", level, "-2022.csv")
  if(write){
    utils::write.csv(outDF, file = outputName, row.names = F)
  }
  return(outDF)
}

# Test total rain
#df <- rainfallTotalRain(WatershedElements)
# gauges <- c("WATER-1", "WATER-2", "WATER-G")
# rainfall_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data\USGS-GCMRC rain-gauge data WY 2000_2021.xlsx)"
# unit <- "second"
# date <- "2002-17-03"
# rainDF <- rainfallTotalRain(rainfall_file = rainfall_file, gaugeList = gauges, level = unit, write = F)
# # Gut check on data
# colMax <- function(data) sapply(data, max, na.rm = TRUE)
# colMax(rainDF)

# Function select data from a given rainfall date - built from rainTotalRainfall() function - second or minute level

rainfallForEvent <- function(rainDF, eventDate, remove = T){
  filteredDF <- rainDF |> # filters recorded rainfall by given date "YYYY-MM-DD"
    filter(lubridate::date(rainDF[,1]) == eventDate) |>
    filter(row_number() <= n()-1)

  if(nrow(filteredDF) == 0){
    return(NULL) # No dates found - exits script
  }
  return(filteredDF)
}
## ------------------------------- rainfallFilter
# Function filters the rainfall by date === if present
rainfallFilter <- function(date, ModelFolder, WatershedElements, overwrite = F){
  # Save the filtered rainfall
  rainFiltered_file <- file.path(ModelFolder, paste0("rain-data-", date, ".csv"))
  if(file.exists(rainFiltered_file) & !overwrite){
    print("Found rainfall file...")
    return(data.frame(readr::read_csv(rainFiltered_file, show_col_types = F)))
  }
  # Check the rainfall database located in the WatershedElements Folder
  rainDF <- rainfallTotalRain(WatershedElements, date, level = "minute", write = F)
  #rainDF <- utils::read.csv("rain-data-minute.csv") # assumes rain is in R directory
  # Filter the rainfall for a given day
  rainFiltered <- rainfallForEvent(rainDF, date)

  # Rainfall Check
  if(is.null(rainFiltered)){ # if no rows in rain data
    errorMessage <- paste("No rainfall data found for", date, ": Exiting script.")
    print(errorMessage)
    print("--------------------------")
    return(write.table(errorMessage, file = file.path(ModelFolder, "errors.txt"), row.names = F, col.names = F))
    #stop(paste("ERROR: No rainfall data found for date:", date))
  }
  # Remove some pesky early data
  rainTemp <- rainFiltered |>
    arrange(rainFiltered$Time_minute)

  # Calculate time difference of first rainfall value
  timeDiff <- diff(rainTemp$Time_minute)[[1]]

  if(timeDiff > 30){ # remove first rainfall value if greater than 30 minutes before next rainfall
    print("Removing minor incident rainfall")
    rainTemp <- rainTemp[2:length(rainTemp$Time_minute),]
  }
  # Further filter the rainfall files
  rainFiltered <-  rainTemp |>
    dplyr::mutate(time = (as.numeric(Time_minute - base::min(Time_minute)) / 60) + 1)

  readr::write_csv(rainFiltered, file = rainFiltered_file)
  return(rainFiltered)
}

# Test if function filters rainfall by date
#rainfallFilter(date, ModelFolder, WatershedElements)

##-----------------------
# Function that checks if rainfall is on a current day
checkRainfall <- function(date_list, WatershedElements, day_list){
  # Check all the days that have recorded discharge and recorded rainfall
  rainDF <- rainfallTotalRain(WatershedElements, level = "minute", write = F)
  # Rainfall list
  rainfallPositive <- c()
  for(x in 1:nrow(day_list)){
    #print(day_list[x, 1])
    date <- day_list[x, 1]
    # Filter the rainfall for a given day
    rainFiltered <- rainfallForEvent(rainDF, date)
    if(is.null(rainFiltered)){ # if no rows in rain data
      #rint(errorMessage)
      #print("--------------------------")
      #return(write.table(errorMessage, file = file.path(ModelFolder, "errors.txt"), row.names = F, col.names = F))
      #stop(paste("ERROR: No rainfall data found for date:", date))
    } else{
      rainfallPositive <- c(rainfallPositive, as.character(date))
    }
  }
  rainDF <- data.frame(time = rainfallPositive)
  rainDF$time <- as.character(lubridate::ymd(rainDF$time))
  readr::write_csv(rainDF, file.path(WatershedElements, "rainfall_discharge_dates.csv"),col_names = F)
  #x <- readr::read_csv(file.path(WatershedElements, "rainfall_discharge_dates.csv"))
  return(rainDF)
}
# days_of_rain <- checkRainfall(day_list)
## Test for rainfall for a given day
# eventDate <- "2012-07-15"
# rainfallEvent <- rainfallForEvent(rainDF = rainDF, eventDate = eventDate)

# Function that takes rainfall for a particular day and obtains duration and intensity (in/hr)
rainfallIntensity <- function(rainfallEvent){
  total_rain <- colSums(rainfallEvent[,-1]) # calculate the sums of each rainfall column
  duration <- as.numeric(difftime(rainfallEvent[nrow(rainfallEvent),1], rainfallEvent[1,1], units = "min")) # select last and first times to find durations in minutes

  intensity <- total_rain/duration * 60 # gives duration of event in inches/hour per gauge
}

# # Test
# rainfallIntensity <- rainfallIntensity(rainfallEvent = rainfallEvent)

# Function that takes input rain data and selects a particular day of rain, if present
rainSelection <- function(rain_dataset, date, sheets = c("WATER-1", "WATER-2", "WATER-G")){ # assumes excel sheets
  #rain_file_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data\USGS-GCMRC rain-gauge data WY 2000_2021.xlsx)"
  rain_tibbles <- lapply(sheets, function(x) readxl::read_excel(rain_dataset, sheet = x)) # creates tibble of x number of
  rain_list <- lapply(rain_tibbles, as.data.frame) # convert into dataframe

  # search through rainfall list for matching dates
  date_matches <- lapply(rain_list, FUN = rainfallForEvent, eventDate = date)
  date_before <- lapply(rain_list, FUN = rainfallForEvent, eventDate = ymd(date) - days(1))

  return(date_matches)
}

# Test - rainfall selection
# data_folder <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleData)" # data folder
# rain_dataset <- file.path(data_folder, "USGS-GCMRC rain-gauge data WY 2000_2021.xlsx") # xlsx file
# date <- "2002-06-08" # key date must be in a yy/mm/dddd format
# q <- rainSelection(rain_dataset, date = date)


# Function that takes rainfall for a particular day and obtains duration and intensity (in/hr)
rainfallDayDistribution <- function(rainfallEvent, units = "minute", write = T){
  # Remove the first entry if it isn't contributing significantly
  rainfallEvent <- rainfallEvent[-1,]
  # Remove the last entry - often entries at the end of day
  #rainfallEvent <- rainfallEvent |> filter(row_number() <= n()-1)

  time <- ceiling_date(rainfallEvent[,1], unit = units)
  sumRain <- rainfallEvent |>
    mutate(Time_date = time) |>
    aggregate(Total_in ~ Time_date, FUN = sum) |> # sum rainfall per time (minute)
    mutate(Time_sec = Time_date - base::min(Time_date)) |> # scale time to 0
    mutate(rain_duration = as.numeric(Time_sec) / 60 + 1) |> # adjust to minutes (add 1 to make start 0)
    mutate(across(c("Total_in"), round, 5)) |> # round the total inches to 5 decimals
    select(rain_duration, Total_in)
  rainDF <- rbind(c(0,0), sumRain)
  if(write){
    write_csv(rainDF, "Rainfall.csv")
  }
  return(rainDF)
}

# #Test
# rainDayDF <- rainfallDayDistribution(rainfallEvent = rainfallEvent)
loadRain <- function(rain_file, rainfall_method = "gauges"){
  if(rainfall_method == "spatial"){
    rain <- readr::read_csv(rain_file, show_col_types = F) # read in rain.csv file
    total_rain_duration <- max(rain$time)
    #rain_step <- mean(diff(rain$time)) # find the average time step
  } else if(rainfall_method == "goes"){
    rain <- rain_file
    total_rain_duration <- (nlyr(terra::rast(rain_file)) - 1) * 10 # duration in minutes
  }else{
    rain <- readr::read_csv(rain_file, show_col_types = F) # read in rain.csv file
    total_rain_duration <- max(rain$time)
  }
  return(list(rain, total_rain_duration))
}
## --------------------------------- Rainfall selection
# Function that checks if rainfall happened on particular dates or before particular dates
rainfallCheck <- function(data, search_dates){
  # read the rainfall data list
  data <- readr::read_csv(data)
  search_dates <- data.frame(readr::read_csv(search_dates))
  col_names <- c("DischargeDate","Rain","RainBefore")
  # Create dataframe to be filled
  df <- data.frame(matrix(nrow = nrow(search_dates), ncol = length(col_names)))
  colnames(df) <- col_names

  for(x in 1:nrow(search_dates)){
    date <- search_dates[x,2]
    df[x,1] <- as.character(date)
    # Assuming the input date is in YYYY/MM/DD
    date <- lubridate::parse_date_time(date, "ymd")
    #date_after <- date + lubridate::days(1) # day after
    date_before <- date + lubridate::days(-1)

    # Check search date, before, and after
    if(date %in% data$day){
      print(paste0("Rain recorded on ", date))
      df[x, 2] <- as.character(date)
      #rainfall_dates_list <- append(rainfall_dates_list, date)
    }
    if(date_before %in% data$day){
      print(paste0("Rain recorded on ", date_before))
      df[x, 3] <- as.character(date_before)
      #rainfall_dates_list <- append(rainfall_dates_list, date_before)
    }
    if(is.null(rainfall_dates_list)){
      paste0("No recorded rainfall found on ", date, " or ", date_before)
    }
  }
  return(df)
}
# Test
# rainfall_dates <- "days-of-disharge.csv" # in root directory
# rainfall_data <- "rain-data-day.csv"
# x <- rainfallCheck(rainfall_data, rainfall_dates)
# readr::write_csv(x, file = "rainfall-check.csv")
# Function that sums the rainfall between to times
cumulativeRain <- function(rainDF, left, right, spatial = F){
  # Function assumes two column rain data frame: time(minutes) | total rainfall
  time <- NULL
  SelectRain <- rainDF |>
    dplyr::filter(dplyr::between(time, left-.001, right)) # because between is inclusive - this should make it not inclusive
  # assumes first column is rain_duration normalized to minutes
  if(spatial){
    rainSum <- colSums(SelectRain[,2:ncol(SelectRain)])
    return(rainSum)
  }else{
    return(sum(SelectRain[,2])) # return the rainfall of second column - assumed total
  }
}

# Test - no test currently
#voronoi
# Function uses Watershed voronoi shapefile, recorded rainfall amounts - outputs rasterized rainfall
rasterizeRainfall <- function(voronoi_shape, rainfallRaster, rainAtGauges){
    # Order must be the same: WATERHOLES-1, WATERHOLES-2, WATERHOLES-G
    shape <- terra::vect(voronoi_shape)
    # Pull out the values and convert to a dataframe
    df <- as.data.frame(values(shape))
    # replace rain values in dataframe with recorded values
    # Note: Make sure the position is corresponding correctly
    replacement_values <- c(round(rainAtGauges, 3))
    conditions <- c(NaN, NaN, NaN) # Should not have any values saved here

    df$rain <- replace(df$rain, df$rain %in% conditions, replacement_values)
    # Assign rainfall to a section the the table
    values(shape) <- df

    # Use an input raster map onto
    newRainfall <- terra::rasterize(shape, rainfallRaster, "rain")
    return(newRainfall)
}
# Test
# voronoi_shape <- watershed_voronoi <- r"(Z:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\waterholes_voronoi.shp)"
# rainAtGauges <- c(1,2,3)#rainSum#
# rainfallRaster <- rast(r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\filled_dem.tif)")
# test <- rasterizeRainfall(voronoi_shape = watershed_voronoi, rainfallRaster = rainfallRaster, rainAtGauges = rainAtGauges)

goesRain <- function(date, rainFolder, WatershedElements, method = "near"){
  # Get rainfall from website
  # Download (optional)
  # Process the rainfall based on when the rainfall starts? How do you know that beforehand
  # Need to account for time conversions 7 hour UTC differences and overlaps
  # Or manually download the times you need into a folder and then process them from here
  # Subset the amount of rainfall you need by using the gauge date !!
  # File location
  goesRain <- file.path(rainFolder, paste0(date,"-rain-", method,".tif"))
  if(file.exists(goesRain)){
    return(goesRain)
  }
  #rainFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data\2022-08-27-rain)"
  files <- list.files(rainFolder, pattern = "*.nc") # GOES files are NC files
  # Loop through files
  rainStack <- terra::rast(file.path(WatershedElements, "cropped_dem.tif")) * 0
  names(rainStack) <- "Start"

  for(file in files){
    # Example file name for GOES satellites- should be standardized
    # "OR_ABI-L2-RRQPEF-M6_G17_s20222392300319_e20222392309386_c20222392309487.nc"
    startTime <- stringr::str_extract_all(file, "(?<=s)([^_]+)") # find characters after s and before _
    endTime <- stringr::str_extract_all(file, "(?<=e)([^_]+)") # find characterts with e and before _
    # Convert strings into time objects
    startT <- parseTimeGOES(startTime)
    endT <- parseTimeGOES(endTime)

    # Load in the raster
    rainRaster <- terra::rast(file.path(rainFolder, file))[[1]]
    # Resample the data
    shape <- terra::vect(file.path(WatershedElements, "waterholes_shape.shp"))
    # Scaled raster
    scale <- terra::rast(file.path(WatershedElements, "cropped_dem.tif"))

    rainProj <- resizeImagery(rainRaster, shape, scale, method = method)
    names(rainProj) <- startT
    rainStack <- c(rainStack, rainProj)
    #terra::writeRaster(rainProj, file = file.path(ModelFolder, "raintest.tif"))
  }
  goesRain <- file.path(rainFolder, paste0(date,"-rain-", method,".tif"))
  terra::writeRaster(rainStack, file = goesRain, overwrite = T)
  return(goesRain)
}
# Test
# rainFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data\2022-08-27-rain)"
# rainTest <- goesRain(date = "2022-08-27", rainFolder = rainFolder, WatershedElements, method = "near")
## ----------------- Parse GOES File name
# Function to parse date strings on GOES objects
parseTimeGOES <- function(time_string){
  # Extract components\
  #Year-Month-Day
  year <- substr(time_string, 1, 4)
  day_of_year <- as.numeric(substr(time_string, 5, 7))
  date <- as.Date(paste(year, day_of_year, sep = "-"), format = "%Y-%j")
  # Hour:Minute
  hour <- substr(time_string, 8, 9)
  minute <- substr(time_string, 10, 11)
  time_comb <- paste0(hour, ":", minute)

  time_posix <- as.POSIXct(paste(date, time_comb), format = "%Y-%m-%d %H:%M", tz = "UTC")
  time_posix <- lubridate::with_tz(time_posix, tzone = "MST")
  # Truncate string - don't need seconds
  return(time_posix)
}
# Test
# time_string <- "2022 239 23 00 31 9"
# goesTime <- parseTimeGOES("20222392300319")
# Plot rain data by day
rainPlots <- function(rainDF){
  # Create months
  rainDF$Second <- minute(rainDF$second)
  rainPlot <- ggplot(rainDF, aes(x = rainDF[,1], y = Total_in, color = Second)) + geom_point() + labs(title = "Total Rainfall Per Second Waterholes: 2001 - 2021", x = "Time", y = "Total Rainfall (in)") + theme(plot.title = element_text(hjust = 0.5))
  rainPlot
  ggsave("second-rainfall-waterholes.png", plot = rainPlot, width = 4.5)
}

# rainDF <- z
# rainPlots(rainDF)

## Clip and save rasterized rainfall
resizeImagery <- function(imagery, outline, targetRaster, method = "near"){
  # outRaster <- terra::resample(imagery, targetRaster)
  reproject <- terra::project(imagery, targetRaster, method = method)
  clipRaster <- terra::crop(reproject, outline, ext = F, mask = T)
  # Select the first layer - the RRQPE
  rainfall <- clipRaster[[1]]
  return(rainfall)
}

# Imagery file
# imagery <- terra::rast("OR_ABI-L2-RRQPEF-M6_G17_s20212030050319_e20212030059386_c20212030059481.nc")
# # Watershed/ outline
# outline <- terra::vect("Example/WatershedElements/waterholes_shape.shp")
# # Grid size file
# targetRaster <- terra::rast("Example/WatershedElements/model_dem.tif")
# # Test script
# resizeImagery(imagery, outline, targetRaster)
