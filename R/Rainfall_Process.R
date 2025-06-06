# Always returns a rainfile path - Expects within the Model Folder
rainfallCreation <- function(ModelFolder, WatershedElements, date = NULL, method = "gauges", weighted = T, overwrite = F, shape = "voronoi.shp", gauges = c("WATER-1", "WATER-2", "WATER-G")){
  # Getting rid of notes
  time <- Total_in <- Time_minute <- NULL
  print("Rainfall...")
  rain_file <- file.path(ModelFolder, "Model-Rainfall.csv")
  rain_spatial_file <- file.path(ModelFolder, "Model-Spatial-Rainfall.csv")
  rain_goes_file <- file.path(ModelFolder, paste0(date, "-goes.tif"))
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
  # browser()
  # Rainfall filtering
  rainFiltered <- rainfallFilter(date, ModelFolder, WatershedElements, overwrite = T)
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

    #test <- rainfallFilter(date, ModelFolder, WatershedElements, overwrite = F)
    # GOES Rainfall
    if(method == "goes"){
      #temporalFolder <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data)"
      #rainFolder <- file.path(temporalFolder, paste0(date,"-rain"))
      # Get rainfall and discharge if possible.
      rain_file <- file.path(ModelFolder, paste0(date,"-goes.tif"))
      if(file.exists(rain_file)){
        return(rain_file)
      }
        # Rainfall filtering
        rainFiltered <- rainfallFilter(date, ModelFolder, WatershedElements, overwrite = T)
        #Save rainfall to model folder
        rainNormal <- rainFiltered |>
          dplyr::select(time, Total_in) |>
          round(4) # round columns to 4 decimals
        readr::write_csv(rainNormal, rain_file)
        print("Rainfall created...")

        # Determine discharge
        discharge <- dischargeCreate(date, ModelFolder, WatershedElements, rain_file = rain_file, discharge = T)
        # Determine start and end times
        #a <- get_start_end_time(discharge)
        rain_file <- get_GOES_Rainfall(ModelFolder, date = date, WatershedElements = WatershedElements, remove = T)
        # rainfall_inches_per_time <- rainfall_stack/6
        # total_rainfall <- sum(rainfall_inches_per_time, na.rm = T)
      return(rain_file)
      }
    # Normalize the rainfall data
    if(method == "gauges"){
      # Weight the input rainfall
      if(FALSE){
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
      cols <- gauges
      spatial_rain <- rainFiltered |>
        dplyr::select(Time_minute, time, dplyr::all_of(cols)) |> # Select relevant columns
        dplyr::add_row(Time_minute = c(rainFiltered[1,1] - lubridate::minutes(1)),
                       `WATER-1` = 0,
                       `WATER-2` = 0,
                       `WATER-G` = 0,
                       time = 0, .before = 1) |>
        dplyr::select(time, cols) |>
        round(5)

      # Save the output
      readr::write_csv(spatial_rain, rain_spatial_file)
      # Copy voronoi shapefile over
      copy_shape <- file.path(WatershedElements, shape)
      # Copy file and convert into the right projection
      voronoi_shape <- terra::vect(copy_shape)
      voronoi_reproj <- terra::project(voronoi_shape, get_crs(file.path(ModelFolder, "model_dem.tif")))
      terra::writeVector(voronoi_reproj, file.path(ModelFolder, "voronoi.shp"), overwrite = T)
      #file.copy(, ModelFolder)
      print("Spatial Rainfall created...")
      return(rain_spatial_file)
    }
    else{
      stop(paste("Could not find rainfall method", method))
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
rainfallAccum <- function(rain, beginning_time, end_time, rainfall_method = "gauges", ModelFolder = "", goes = NA){
  if(rainfall_method == "spatial"){
    # Get rainfall from shape
    rainForGauges <- cumulativeRain(rain, left = beginning_time, right = end_time, spatial = T)
    # Could adjust voronoi
    rainfall_for_timestep <- rasterizeRainfall(rainAtGauges = rainForGauges,
                                               voronoi_shape = file.path(ModelFolder, "voronoi.shp"),
                                               rainfallRaster = terra::rast(file.path(ModelFolder, "model_soil_stack.tif")))
    return(rainfall_for_timestep)
  }else if(rainfall_method == "goes" & !is.logical(goes)){
    if(beginning_time == 0){
      rainfall_for_timestep <- 0
      return(rainfall_for_timestep)
    }else{
      layerSelection <- ceiling(beginning_time/10)
      if(layerSelection == 0){
        layerSelection == 1
      }
      mm_to_in <- 1/25.4
      timeElapsed <- end_time - beginning_time
      rainfall_for_timestep <- goes[[layerSelection]] / (10/timeElapsed) * mm_to_in # rain fallen in inches
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
    rain <- as.data.frame(readr::read_csv(rainfall, show_col_types = FALSE))
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

# Get the total rainfall for a particular day
rainfallTotal <- function(rainfall, filepath = T, inches = T){
  rain <- as.data.frame(readr::read_csv(rainfall, show_col_types = FALSE))
  # assuming second column is the rainfall amount
  rainfall_total <- dplyr::last(rain[,2]) - rain[1,2]
  return(rainfall_total)
}
# Test rainfall process function
# rainfall_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\1hr_1in_initial\Rainfall.csv)"
# rainfallTotal(rainfall_file)

# Function that takes rainfall data from a watersheds gauges and gathers total rainfall
#' Gathers rainfall data based on date from spreadsheet
#'
#' @param rainfall_folder Folder path of rainfall data
#' @param date Date string in format: "2021-07-13"
#' @param level Optional filter: can filter by "day", "hour", "minute" based on
#' utilizes lubridate package
#' @param total_col Name of total column: Default "TOTAL"
#' @param gauges Name of gauges spreadsheets:
#' gauges = c("WATER-1", "WATER-2", "WATER-G")
#' @param write T/F. If TRUE, will write output to rainfall_folder
#'
#' @return Returns data frame with rainfall data filtered by level
#' @export
#'
#' @examples
#' rain_f <- dirname(system.file("extdata", "dem.tif", package = "desertHydro"))
#' rainfallTotalRain(rain_f, "2022-07-05", level = "day")
rainfallTotalRain <- function(rainfall_folder, date, level = "day", total_col = "TOTAL", gauges = c("WATER-1", "WATER-2", "WATER-G"),  write = F){
  rain_in <- date_time <- NULL
  if(substr(date, 1, 4) == "2022"){ # Year 2022
    rainfall_file <- file.path(rainfall_folder, "USGS_Rain_2022.xlsx")
  }else{
    rainfall_file <- file.path(rainfall_folder, "USGS_Rain_2001_2021.xlsx") # Should be changed
  }
  # if(new){
  #   rainfall_file <- file.path(rainfall_file, "USGS_Rain_2023.xlsx")
  # } # year 2023

  # Load in the rainfall file - excel sheet
  rain <- rainfall_file |>
    readxl::excel_sheets() |>
    purrr::set_names() |>
    purrr::map(readxl::read_excel, path = rainfall_file)

  # Select the rainfall gauges in the list
  rainSelected <- rain[gauges]
  # Dirty way to adjust the cumulative amounts into differences
  for(x in 1:length(rainSelected)){
    # print(x)
    column_names <- colnames(rainSelected[[x]])
    # Determine which column contains the Cumulative Rain
    idx <- grep(total_col, column_names)
    column_name <- column_names[idx]

    # Match the date string
    idx_date <- grep("Date", column_names) # this could be changes to be more dynamic
    date_column <- column_names[idx_date]
    #idx <-  which(colnames(rainSelected[[x]]) == "TOTAL Cumulative Rain (in)", arr.ind = T)
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
      #dplyr::mutate(date_time = `Date and time`, rain_in = `TOTAL Cumulative Rain (in)`) |>
      dplyr::rename(date_time = date_column) |>
      dplyr::rename(rain_in = column_name) |>
      dplyr::select(date_time, rain_in) |> # reorganize the list for a particular gauge
      stats::aggregate(rain_in ~ lubridate::floor_date(date_time, unit = level), FUN = sum) |>
      dplyr::rename(date = "lubridate::floor_date(date_time, unit = level)")

      #mutate(month = lubridate::month(date_time, label = T)) # Create monthly column
  }
  # Join all of the rainfall amounts together
  joinedDF <- dplyr::full_join(rainSelected[[1]],rainSelected[[2]],  by = "date") |>
    dplyr::full_join(rainSelected[[3]], by = "date")
  #return(rainSelected)
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

  if(write){
    outputName <- paste0("rain-data-", level, "-2022.csv")
    utils::write.csv(outDF, file = file.path(rainfall_folder, outputName), row.names = F)
  }
  return(outDF)
}

# Function select data from a given rainfall date - built from rainTotalRainfall() function - second or minute level

rainfallForEvent <- function(rainDF, eventDate, remove = T){

  filteredDF <- rainDF |> # filters recorded rainfall by given date "YYYY-MM-DD"
    dplyr::filter(lubridate::date(rainDF[,1]) == eventDate) #|>
    #dplyr::filter(dplyr::row_number() <= dplyr::n()-1) # Why is this here?

  if(nrow(filteredDF) == 0){
    return(NULL) # No dates found - exits script
  }
  return(filteredDF)
}
## ------------------------------- rainfallFilter
# Function filters the rainfall by date === if present
rainfallFilter <- function(date, ModelFolder, WatershedElements, overwrite = F){
  Time_minute <- NULL # should probably be fixed to be more dynamic when selecting column names
  # maybe regex expression
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
    return(NULL)
    #return(utils::write.table(errorMessage, file = file.path(ModelFolder, "errors.txt"), row.names = F, col.names = F))
    #stop(paste("ERROR: No rainfall data found for date:", date))
  }
  # Remove some pesky early data
  rainTemp <- rainFiltered |>
    dplyr::arrange(rainFiltered$Time_minute)

  # Calculate time difference of first rainfall value - if more than 1 value
  if(nrow(rainTemp) != 1){
    timeDiff <- diff(rainTemp$Time_minute)[[1]]
    if(timeDiff > 30){ # remove first rainfall value if greater than 30 minutes before next rainfall
      print("Removing minor incident rainfall")
      rainTemp <- rainTemp[2:length(rainTemp$Time_minute),]
    }
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
  date_before <- lapply(rain_list, FUN = rainfallForEvent, eventDate = lubridate::ymd(date) - lubridate::days(1))

  return(date_matches)
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
    total_rain_duration <- (terra::nlyr(terra::rast(rain_file))) * 10 # duration in minutes
  }else{
    rain <- readr::read_csv(rain_file, show_col_types = F) # read in rain.csv file
    total_rain_duration <- max(rain$time)
  }
  return(list(rain, total_rain_duration))
}

# Function that sums the rainfall between to times
cumulativeRain <- function(rainDF, left, right, spatial = F){
  # Function assumes two column rain data frame: time(minutes) | total rainfall
  time <- NULL
  if(is.character(rainDF)){
    rainDF <- readr::read_csv(rainDF, show_col_types = F)
  }
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
# voronoi
# Function uses Watershed voronoi shapefile, recorded rainfall amounts - outputs rasterized rainfall
rasterizeRainfall <- function(rainAtGauges, voronoi_shape, rainfallRaster){
    # Order must be the same: WATERHOLES-1, WATERHOLES-2, WATERHOLES-G
    if(inherits(voronoi_shape, "character")){
      shape <- terra::vect(voronoi_shape)
    }else{
      shape <- voronoi_shape
    }
    # Pull out the values and convert to a dataframe
    df <- as.data.frame(terra::values(shape))
    df$rain <- NA
    matchingIDX <- match(df$Gauge, names(rainAtGauges))
    df$rain <- round(rainAtGauges[unlist(matchingIDX)],3)
    # Assign rainfall to a section the the table
    terra::values(shape) <- df
    # Use an input raster map onto
    newRainfall <- terra::rasterize(shape, rainfallRaster, field = "rain")
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
extract_time_string <- function(filename, starting_char ="s") {
  matches <- regmatches(filename, regexpr(paste0(starting_char,"\\d{4}\\d{3}\\d{6}"), filename))
  date_string <- sub("s", "", matches) # Remove the 's' prefix
  converted_time <- parseTimeGOES(date_string)
  return(converted_time)
}
# Test
# time_string <- "2022 239 23 00 31 9"
# goesTime <- parseTimeGOES("20222392300319")
# Plot rain data by day
rainPlots <- function(rainDF){
  # Clean  up globals
  Total_in <- Second <- NULL
  # Create months
  rainDF$Second <- lubridate::minute(rainDF$second)
  rainPlot <- ggplot2::ggplot(rainDF, ggplot2::aes(x = rainDF[,1], y = Total_in, color = Second)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Total Rainfall Per Second Waterholes: 2001 - 2021", x = "Time", y = "Total Rainfall (in)") +  ggplot2::theme(plot.title =  ggplot2::element_text(hjust = 0.5))
  rainPlot
  ggplot2::ggsave("second-rainfall-waterholes.png", plot = rainPlot, width = 4.5)
}

# rainDF <- z
# rainPlots(rainDF)

## Clip and save rasterized rainfall
resizeImagery <- function(imagery, outline, targetRaster, method = "near"){
  # outRaster <- terra::resample(imagery, targetRaster)
  reproject <- terra::project(imagery, targetRaster, method = method)
  clipRaster <- terra::crop(reproject, outline, ext = F, mask = T)
  # Select the first layer - the RRQPE
  #rainfall <- clipRaster[[1]]
  return(clipRaster)
}

compare_rainfall <- function(gauge, goes, gauge_coords){
  # Read in the data
  if(inherits(gauge, "character")){
    gauge <- data.table::fread(gauge)
  }else if(!inherits(gauge, "data.table")){
    gauge <- data.table::as.data.table(gauge) # make it a data.table
  }
  if(inherits(goes, "character")){
    goes <- terra::rast(goes)
  }
  # Gauge coordinates
  if(inherits(gauge_coords, "character")){
    if(file.exists(gauge_coords)){
      gauge_coords <- terra::vect(gauge_coords)
    }else{
      cat("Could not find", gauge_coords, "\n Using example gauges\n")
      gauge_coords <- terra::vect(file.path(model()@watershedPath, "rain_gauges_waterholes.shp"))
    }
  }
  # Get gauge time
  gauge_time <- get_start_end_time(gauge, time_col = "Time_minute", data_col = "Total_in")
  # Get timezone of gauges
  timezone <- attr(gauge_time[[1]], "tzone")
  # Get goes time
  goes_time <- get_start_end_time(goes, timezone = timezone)
  specific_gauges <- c("WATER_1", "WATER_2", "WATER_G")
  time_col <- "Time_minute"
  # Select only the time and gauge columns - static column selection
  # browser()
  gauge_select <- gauge[,c("Time_minute","WATER_1", "WATER_2", "WATER_G")]
  gauge_select[, Time := as.POSIXct(floor(as.numeric(Time_minute) / 600) * 600,
                                        origin = "1970-01-01", tz = "UTC")]
  # in_mm - assumes inches
  in_mm <- 25.4
  gauge_mm <- gauge_select[, .(WATER_1_gauge = sum(WATER_1, na.rm = TRUE)*in_mm,
                            WATER_2_gauge = sum(WATER_2, na.rm = TRUE)*in_mm,
                            WATER_G_gauge = sum(WATER_G, na.rm = TRUE)*in_mm),
                            by = Time]

  # Extract values in GOES Rainfall Estimate
  goes_time_values <- names(goes)
  if(get_crs(goes) != "epsg:4269"){
    goes <- terra::project(goes, "epsg:4269")
  }
  goes_values <- terra::extract(goes, gauge_coords, ID = F, raw = F)
  goes_mm <- as.data.frame(t(goes_values))*(10/60) # mm/hr *1hr/60mins * 10 mins = mm
  goes_mm$Time <- as.POSIXct(goes_time_values, tz = "UTC")
  names(goes_mm) <- c(specific_gauges, "Time")

  # Merge the two
  merged_rain <- merge(gauge_mm, goes_mm, by = "Time", all.x = T, all.y = T)
  # Fill NA
  merged_rain[is.na(merged_rain)] <- 0
  # Drop all of the rows that contains zeros at the end
  last_idx <- max(which(rowSums(merged_rain[,2:ncol(merged_rain)] == 0) < 6))
  merged_rain <- merged_rain[1:last_idx,]
  # Get date
  #browser()
  date_of <- substr(gauge_select$Time_minute[1], 1, 10)
  cat("Correlation Matrix for:", date_of, "\n")
  print(cor(merged_rain[,-1]))
  return(merged_rain)
}

plot_rainfall_comparison <- function(rain_df, date, store = T, outpath = "", gauge_prefix = "WATER"){
  # Clean up variables
  Time_minute <- discharge <- Total_in <- NULL
  # Widen the data of rainfall with the melt, which will take the gauge columns and
  # Create categories under the "Gauge" column and keep the values in a new column called
  # "Rain_depths" and preserve the "Time_minute" column
  # Find gauges with "Water"
  gauges <- grep(gauge_prefix, colnames(rain_df), ignore.case = T, value = T)
  meltedDF <- data.table::melt(data.table::as.data.table(rain_df),
                               id.vars = "Time",
                               measure.vars = gauges,
                               variable.name = "Gauge",
                               value.name = "Rain_mm")
  # Create two groups of gauges?
  # Split the data.frame into two by gauge
  labels <- c("WATER_1","WATER_2","WATER_G")
  gauge_df <- meltedDF[Gauge %in% c("WATER_1_gauge", "WATER_2_gauge", "WATER_G_gauge")]
  goes_df <- meltedDF[Gauge %in% labels]
  # Determine how many gauges there are
  gauge_plot <- ggplot2::ggplot(gauge_df, mapping = ggplot2::aes(x = Time, y = Rain_mm, fill = Gauge)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = paste("Rainfall for Gauges:", date),
                  x = "", y = "Measured Rainfall (mm)") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = c(1, 1),               # Moves legend to top-right
                   legend.justification = c(1, 1),          # Aligns top-right corner of legend box
                   legend.background = ggplot2::element_rect(fill = "white", color = "black"),  # Adds visible border
                   legend.margin = ggplot2::margin(2, 2, 2, 2),
                   legend.title.align = 0.5) +       # Ensures padding inside legend box)+  # Add background for clarity +
    ggplot2::scale_fill_viridis_d(labels = labels)

  goes_plot <- ggplot2::ggplot(goes_df, mapping = ggplot2::aes(x = Time, y = Rain_mm, fill = Gauge)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = paste("Rainfall for GOES:", date),
                  x = "", y = "Measured Rainfall (mm)") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = c(1, 1),               # Moves legend to top-right
                   legend.justification = c(1, 1),          # Aligns top-right corner of legend box
                   legend.background = ggplot2::element_rect(fill = "white", color = "black"),  # Adds visible border
                   legend.margin = ggplot2::margin(2, 2, 2, 2),
                   legend.title.align = 0.5) +       # Ensures padding inside legend box)+  # Add background for clarity +
    ggplot2::scale_fill_viridis_d(labels = labels)

  # Combine the two graphs
  combined_plot <- gridExtra::grid.arrange(gauge_plot, goes_plot)
  # store the graph for the date
  if(store){
    outputPlot <- paste0("rain_comparison_", date ,".png")
    ggplot2::ggsave(file.path(outpath, outputPlot), plot = combined_plot, width = 6.5)
  }
}
# Dates
# goes_dates <- c("2021-07-22", "2021-07-23", "2021-09-01",
#                 "2021-09-11", "2022-07-05", "2022-07-15",
#                 "2022-07-24", "2022-07-28", "2022-07-29",
#                 "2022-07-30", "2022-08-27", "2022-09-14")
# Correlate rainfall
download_rain <- function(date, rainfall_method = "goes"){
  ModelFolder <- paste0("Results/Rainfall/", date,"-GOES")
  #WatershedElements <- model()@watershedPath
  if(!file.exists(ModelFolder)){
    dir.create(ModelFolder)
  }
  # Check if goes file exists
  rain_file <- file.path(ModelFolder, paste0(date, "-goes.tif"))
  if(!file.exists(rain_file)){
    rain_file <- suppressWarnings(rainfallCreation(ModelFolder, WatershedElements,
                                                   date = date, method = rainfall_method,
                                                   overwrite = T))
  }

  rain_discharge <- dischargeCreate(date = date, ModelFolder, WatershedElements,
                                   discharge = T)

  combined_rain <- compare_rainfall(rain_discharge, rain_file,
                   gauge_coords = file.path(WatershedElements, "rain_gauges_waterholes.shp"))
  data.table::fwrite(combined_rain, file.path(ModelFolder, "rain-goes-compare.csv"))
  return(combined_rain)
}
# Get all of the rain
#goes_rain_all <- lapply(goes_dates, download_rain)
rainfall_plot_comparison <- function(folder,  date = "2021-07-22", method1 = "spatial", method2 = "goes"){

  goes_file <- rainfallCreation(folder, model()@watershedPath, date = date, method = method2, overwrite = F)
  # Spatially distributed map
  spatial_file <- rainfallCreation(folder, model()@watershedPath, date = date, method = method1, overwrite = F)
  # Create two graphs
  rain_df <- data.table::fread(spatial_file)[,c("time","WATER-1", "WATER-2", "WATER-G")]
  # Janky way to load in the voronoi shapefile
  rain_regions <- terra::vect(filePresent("voronoi.shp", model()@watershedPath))
  if(get_crs(rain_regions) != get_crs(goes_file)){
    rain_regions <- terra::project(rain_regions, get_crs(goes_file))
  }
  gauges <- rain_df[,c("WATER-1", "WATER-2", "WATER-G")]*25.4
  rain_surface <- do.call(c, apply(gauges, MARGIN = 1, FUN = rasterizeRainfall, rain_regions, terra::rast(goes_file)[[1]]))
  # Add up all the rain surfaces
  spatial_rain_total <- sum(rain_surface)
  units <- "Rain (mm)"
  caption <- paste0("Rainfall from gauges over ", max(rain_df$time)," minutes.")

  # Create template for goes rain
  goes_rain_total <- sum(terra::rast(goes_file)/6)
  # Get global minimums
  # Get global min and max for both rasters
  # vals <- terra::unique(c(terra::values(spatial_rain_total), terra::values(goes_rain_total)))
  # vals <- round(sort(terra::unique(vals)))  # sort and remove duplicates
  # breaks <- c(vals, max(vals) + 1)  # right-inclusive intervals
  # col_pal <- viridis::viridis(length(vals))  # one color per value
  grDevices::png(filename = file.path(folder, paste0("rainfall-comparison-", date, ".png")),
      width = 5.67, height = 4.5, units = "in", res = 300, family = "serif")

  # Tight layout: shrink top & bottom margins (mar[1]=bottom, mar[3]=top)
  graphics::par(mfrow = c(1, 2), mar = c(2, 3, 2, 2), oma = c(0, 0, 0, 0), family = "serif")

  # Plot 1
  terra::plot(spatial_rain_total,
              main = "Gauges Rainfall (mm)",
              cex = 0.9)

  # Plot 2
  terra::plot(goes_rain_total,
              main = "Satellite Rainfall (mm)",
              cex = 0.9)

  dev.off()
  return("Finished Creating Plot")
}

get_total_rainfall_goes <- function(goes_file){
  goes <- terra::rast(goes_file)
  cell_size <- terra::cellSize(goes, unit = "m") #m^2
  # Sum all the rainfall
  mm_to_m <- 1/100
  goes_sum <- sum(goes, na.rm = T) * mm_to_m /6 * cell_size
  m3_ft3 <- 35.315
  rain_fall <- sumCells(goes_sum) *m3_ft3 # ft3
  # Extract rainfall at rain gauge points
  return(rain_fall)
}
