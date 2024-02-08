# Functions to process Rainfall Data
# Rainfall can come in 2 non-spatial varieties: constant and time-dependent
# library(readxl)
# library(tidyverse)
# library(purrr)
# library(lubridate)
# Constant rainfall
# Total rain fall / Rainfall Duration = Rainfall per timestep
# 1 in / 20 min = .05 in/min

# Time dependent rainfall
# The rainfall rate depends on the time.


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
rainfallTotalRain <- function(rainfall_file, level = "day", gauges = c("WATER-1", "WATER-2", "WATER-G"),  write = T){
  rainfall_file <- file.path(rainfall_file, "USGS-GCMRC rain-gauge data WY 2000_2021.xlsx")
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
    # create vector of differences
    differences <- c(0, diff(rainSelected[[x]][[2]]))
    # If differences are greater than 100 - set equal to 0
    differences[differences > 100] <- 0

    rainSelected[[x]][[2]] <- differences  # Assign the rainfall to the increment recorded
    rainSelected[[x]] <- rainSelected[[x]] |>
      mutate(date_time = `Date and time`, rain_in = `TOTAL Cumulative Rain (in)`) |>
      select(date_time, rain_in) |> # reorganize the list for a particular gauge
      aggregate(rain_in ~ floor_date(date_time, unit = level), FUN = sum)
      #mutate(month = lubridate::month(date_time, label = T)) # Create monthly column
  }

  rainDay <- plyr::join_all(rainSelected, by = "floor_date(date_time, unit = level)", type = "left")
  names(rainDay) <- c(paste0("Time_",level), gauges)
  rainDay[is.na(rainDay)] <- 0 # Fill NA values with zero
  #rainDay[(rainDay >= 100)] <- 0 # take take of extraneous values

  ## !Filter the date out if desired
  # if(!is.null(date)){
  #   date <- ymd(date)
  #   rainDay <- rainfallForEvent(rainDF = rainDay, eventDate = date)
  # }
  # Sum the rainfall for a particular time-step
  rainDay$Total_in <- rowSums(rainDay[gauges])
  outDF <- rainDay
  outputName <- paste0("rain-data-", level, ".csv")
  if(write){
    utils::write.csv(outDF, file = outputName, row.names = F)
  }
  return(outDF)
}

# Test total rain
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
  filteredDF <- rainDF |>
    filter(lubridate::day(rainDF[,1]) == eventDate) |> # filters recorded rainfall by given date "YYYY-MM-DD"
    filter(row_number() <= n()-1)
  return(filteredDF)
}

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

# Function that sums the rainfall between to times
cumulativeRain <- function(rainDF, left, right){
  # Function assumes two column rain data frame: time(minutes) | total rainfall
  SelectRain <- rainDF |>
    dplyr::filter(between(time, left, right))
  # assumes first column is rain_duration normalized to minutes
  return(sum(SelectRain[,2])) # return the rainfall of second column - assumed total
}

# Test - no test currently

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
# voronoi_shape <- watershed_voronoi <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\waterholes_voronoi.shp)"
# rainAtGauges <- c(1,2,3)
# rainfallRaster <- terra::rast(r"(C:\Thesis\Arid-Land-Hydrology\R\Example\WatershedElements\filled_dem.tif)")
# test <- rasterizeRainfall(voronoi_shape = watershed_voronoi, rainfallRaster = rainfallRaster, rainAtGauges = rainAtGauges)

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


