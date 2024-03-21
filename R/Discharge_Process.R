# Functions to process the discharge data
# library(dplyr)
# library(lubridate)
# library(gridExtra)
# library(readr)
# library(purrr)
# library(ggplot2)
# #install.packages("ggtext")
# library(ggtext)
# #install.packages("zoo")
# library(zoo)

##-------------------- Discharge Creation
# Function that checks and/or creates discharge for given date
dischargeCreate <- function(date, ModelFolder, WatershedElements, rain_file, discharge = F, store = T){
  # Load in the filtered rainfall file
  rainFiltered_file <- file.path(ModelFolder, paste0("rain-data-", date,".csv"))
  rain_discharge_file <- file.path(ModelFolder, "rain-discharge.csv")
  if(discharge){
    print("Processing discharge data...")
    if(file.exists(rain_discharge_file) & file.exists(rainFiltered_file)){
      print("Found discharge data")
      return(rain_discharge <- readr::read_csv(rain_discharge_file, show_col_types = F))
    }else {
      print("Creating discharge data")
      # Check if discharge present on day - returns the date or optional date
      date <- lubridate::date(discharge_present(WatershedElements, date))[1] # returns discharge date or next day (first     entry)
      print(date)
      # Load in stream data from Waterholes - GCMRC
      dischargeDataPath <- file.path(WatershedElements, "Waterholes_Stream_gcmrc20231127132459.tsv") #- sloppy
      print("discharge path complete")
      # Calculate the daily discharge for given date
      dischargeDF <- dailyDischarge(discharge_file_path = dischargeDataPath,
                                    discharge_date = date,
                                    save_location = ModelFolder,
                                    saveGraphs = store)
      print('rain filtered')
      # Create
      rainFiltered <- rainfallFilter(date, ModelFolder, WatershedElements, overwrite = F)
      print('combining discharge')
      # Combine the rainfall and discharge into a single .csv file
      rain_discharge <- rainfall_discharge_combine(rainfallDF = rainFiltered,
                                                   dischargeDF,
                                                   outpath = ModelFolder,
                                                   store = T)
      return(rain_discharge)
    }
  }else{
    rain_discharge <- readr::read_csv(rain_file, show_col_types = F) |>
      dplyr::select(time, Total_in) |>
      dplyr::add_row(Total_in = 0,
                     time = 0,
                     .before = 1) |>
      dplyr::arrange(time)
    # write rain-discharge into model folder
    readr::write_csv(x = rain_discharge, file = rain_discharge_file)
    return(rain_discharge)
  }
}

# Test - not currently
#z <- dischargeCreate(date, ModelFolder, WatershedElements, discharge = T)
#
#
#
# ##----------------------discharge totals
# # Function that takes rainfall data from a watersheds gauges and gathers total rainfall
# dischargeTotal <- function(discharge_file, write = F){
#     stream_data <- readr::read_tsv(discharge_file, show_col_types = FALSE)
#
#     # Filter data with recorded discharge values
#     observable_discharge <- as.data.frame(stream_data) |>
#       mutate(date_time = stream_data$`time (MST)`, height = stream_data$`Gage Height(ft)-GCMRC-GCLT1`, discharge = stream_data$`Discharge(cfs)-GCMRC-GCLT1`, temp_c = stream_data$`Air Temperature(°C)-GCMRC-GCLT1`) |>
#       select(date_time, height, discharge, temp_c) |>
#       mutate(temp_c = ifelse(temp_c < -50, NA, temp_c)) |> # deal with values less then possible (-999)
#       mutate(temp_c = na.approx(temp_c)) |>
#       filter(discharge > 0) # filter out all the discharge greater than 0
#   if(write){
#     write_csv(observable_discharge, file = "observable-discharge.csv")
#   }
#   return(observable_discharge)
# }
# # Test
# # discharge_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data\Waterholes_Stream_gcmrc20231127132459.tsv)"
# # dischargeDF <- dischargeTotal(discharge_file = discharge_file, write = F)
#
# # Function that resamples the data to different time scales - mean approximation
# # date_time, height, discharge, temp_c
# dischargeResample <- function(dischargeDF, units = "day", write = F){
#   resampledDF <- dischargeDF |>
#     aggregate(cbind(discharge, temp_c) ~ floor_date(date_time, unit = units), FUN = max) |>
#     set_names(units, "maxDischarge", "temp_c") # want the peaks
#   if(write){
#     filename <- paste0("max-discharge-per-", units, ".csv")
#     write_csv(resampledDF, filename)
#   }
#   return(resampledDF)
# }
#
# # Test
# # discharge_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data\Waterholes_Stream_gcmrc20231127132459.tsv)"
# # dischargeDF <- dischargeTotal(discharge_file = discharge_file, write = F)
# # units <- "day"
# # resampled <- dischargeResample(dischargeDF = dischargeDF, units = units, write = T)
# # date <- as.Date("2012-07-15 UTC")
#
# # Create plot of discharge values
# # ggplot(resampled) +
# #   geom_point(aes(x = day, y = maxDischarge)) +
# #   geom_point(aes(x = resampled[day %in% date, 1], y = resampled[day %in% date, 2]), color = "red") +
# #   labs(title = "Waterholes Daily Peak Discharge: 2001-2022", x = "Year", y = "Maximum Discharge (ft\U00B3/s)") +
# #   theme(plot.title = element)
#
#
#
# # Create a function to look up if discharge is recorded on a particular date
# discharge_present <- function(data_folder, date){
#   # Currently only works for 1 downloaded gauge - does select by stream gauge
#   stream_data <- file.path(data_folder, "Waterholes_Stream_gcmrc20231127132459.tsv")
#
#   if(!file.exists(stream_data)){ # check file location
#     stop(paste0("Could not locate discharge data in ", data_folder))
#   }else{
#     print("Located discharge data.")
#   }
#
#   # Use the discharge - total function to reorganize the stream gauges by day
#   # then compare if input date - before or after exists
#   dischargeDF <- suppressWarnings(dischargeTotal(stream_data, write = F)) # dataframe with recorded discharge values (>0)
#
#   # use the dischargeResample function to obtain the highest discharge for a each day
#   dischargeDaily <- dischargeResample(dischargeDF, units = "day") # dataframe with discharge per day
#
#   # Assuming the input date is in YYYY/MM/DD
#   #date <- lubridate::ymd(date) # convert date
#   date <- lubridate::parse_date_time(date, "ymd")
#   date_after <- date + days(1) # day after
#   date_before <- date + days(-1)
#   discharge_dates <- c()
#   # Check search date, before, and after
#   if(date %in% dischargeDaily$day){
#     print(paste0("Discharge recorded on ", date))
#     discharge_dates <- append(discharge_dates, date)
#   }
#   if(date_after %in% dischargeDaily$day){
#     print(paste0("Discharge recorded on ", date_after))
#     discharge_dates <- append(discharge_dates, date_after)
#   }
#   if(is.null(discharge_dates)){
#     stop(paste0("No recorded discharge found on ", date, " or ", date_after))
#   }
#   return(discharge_dates)
# }
#
# # Example - folder location
#
# # data_folder <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleData)"
# # rain_data <- file.path(data_folder, "USGS-GCMRC rain-gauge data WY 2000_2021.xlsx")
# # stream_data <- file.path(data_folder, "Waterholes_Stream_gcmrc20231127132459.tsv")
# # sheet <- "WATER-G"
# # date <- "6/8/2002"
# # discharge_present(data_folder = data_folder, date = date)
#
# # Function that creates combined discharge and rainfall output
#
# rainfall_discharge_combine <- function(rainfallDF, dischargeDF, outpath, store = T, trim = F){
#   # # Create zeros within the data create a sequence of time from the rainfall DF
#   # time_seq <- data.frame(Time_minute = seq(rainfallDF$Time_minute[1], tail(rainfallDF$Time_minute,1), by = "min"))
#   # # Change gauge names
#   colnames(rainfallDF) <- stringr::str_replace_all(colnames(rainfallDF), "-", "_")
#   #colnames(rainfallDF) <- stringr::str_replace_all(colnames(rainfallDF), "[.]", "_")
#   # # This step combines the rainfall and the discharge data into 1 dataframe - dirty
#   # rain_seq <- time_seq |>
#   #   dplyr::left_join(rainfallDF, by = "Time_minute") |>
#   #   tidyr::replace_na(list( time = 0))
#   # # This step combines the rainfall and the discharge data into 1 dataframe - dirty
#
#   # Get the first time recorded for the discharge and rainfall
#   discharge_start <- dischargeDF$`time (MST)`[1]
#   rain_start <- rainfallDF$Time_minute[1]
#
#   if(rain_start < discharge_start){
#     time_diff <- as.numeric(difftime(discharge_start, rain_start, units = "mins"))
#     # Append discharge time at the end of rainfall
#     rainfallDF <- rainfallDF |>
#       dplyr::add_row(Time_minute = discharge_start,
#                      time = time_diff,
#                      WATER_1 = 0, WATER_2 = 0, WATER_G = 0, Total_in = 0,
#                      .before = 1) |>
#       dplyr::arrange(Time_minute)
#   }
#
#   rain_discharge <- dplyr::full_join(rainfallDF, dischargeDF, by = join_by("Time_minute" == "time (MST)"))
#   # interpolate between values
#   # Selectable columns
#   cols <- c("Time_minute", "Total_in", "discharge", "height")
#
#   # Filter columns
#   rain_discharge <- rain_discharge |>
#     select(cols) |>
#     arrange(rain_discharge$`Time-minute`) |>
#     mutate(time = (as.numeric(rain_discharge$`Time_minute` - base::min(rain_discharge$`Time_minute`)
#                              ) / 60) + 1)
#
#   #return(rain_discharge)
#   # Perform linear approximation on discharge and height values
#   rain_discharge$discharge <- round(na.approx(rain_discharge$discharge, na.rm = F), 4)
#   rain_discharge$height <- round(na.approx(rain_discharge$height, na.rm = F), 2)
#   #return(rain_discharge)
#   # Replace NA values with 0s
#   rain_discharge[is.na(rain_discharge)] <- as.integer(0)
#
#   # Add a line of zeros at the beginning
#   rain_discharge <-  rain_discharge |>
#       dplyr::add_row(Time_minute = c(rain_discharge[1,1] - minutes(1)),
#                         Total_in = 0,
#                        discharge = 0,
#                           height = 0,
#                             time = 0, .before = 1) |>
#                           arrange(time) |>
#                       mutate(difftime = c(0, diff(time)))
#
#   # Cut off values if rainfall occurs after discharge recedes
#   #which(rain_discharge$difftime > 60)[1])){
#   index <- which(rain_discharge$difftime > 60)[1] # grab first time hour jump
#   if(rain_discharge[index, ]$discharge < 1 & trim){ # remove last entries if little discharge
#       rain_discharge <- rain_discharge[1:(index-1),] # remove end points
#    }
#
#   # Interpolate between discharge amounts
#   # discharge <- approx(rain_discharge$discharge, method = "linear")
#   # xapprox <- approx(x$discharge, method = "linear")
#   # Writes the values into csv
#   if(store){
#     filename <- file.path(outpath, "rain-discharge.csv")
#     readr::write_csv(x = rain_discharge, file = filename)
#     # Read csv
#     #x <- readr::read_csv(r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel\Demo_Test\2022-07-15\rain-discharge.csv)")
#   }
#   return(rain_discharge)
# }
# # Test - see rain-discharge set-up
# # Rain
#
# # Plot rainfall - discharge
plot_rainfall_discharge <- function(rain_discharge_DF, date, store = T, outpath = ""){
  # Change the x-axis from time to Time_minute
  rain_plot <- ggplot2::ggplot(rain_discharge_DF) +
    ggplot2::geom_col(mapping = aes(x = Time_minute, y = Total_in)) +
    ggplot2::labs(title = paste("Rainfall at Waterholes Watershed, AZ:", date), x = "", y = "Measured Rainfall (in)") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


  discharge_plot <- ggplot2::ggplot(rain_discharge_DF) +
    ggplot2::geom_line(mapping = aes(x = Time_minute, y = discharge))  +
    #labs(title = paste("Discharge at Waterholes Watershed, AZ:", date), x = "", y = bquote("Measured Discharge" (ft^3/s))) +
    ggplot2::labs(title = paste("Discharge at Waterholes Watershed, AZ:", date), x = "", y = paste0("Measured Discharge ft\u00b3/s)")) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5))

    # Combine the two graphs
    combined_plot <- gridExtra::grid.arrange(discharge_plot, rain_plot)
    # store the graph for the date
    if(store){
      outputPlot <- paste0("discharge_rain_", date ,".png")
      ggplot2::ggsave(file.path(outpath, outputPlot), plot = combined_plot, width = 6)
    }
}
#
# # Test function
# # rain_discharge <- "rain-discharge.csv"
# # rain_discharge_DF <- readr::read_csv(file = file.path(ModelFolder, rain_discharge))
# # # date <- "2007-07-23"
# # plot_rainfall_discharge(rain_discharge_DF, date = date, store = F)
#
#
# # Filters discharge data set for a particular date, optional - create graphic
dailyDischarge <- function(discharge_file_path, discharge_date, save_location, saveGraphs = T){
  discharge <- discharge_diff <- height <- NULL
  # Read data into R
  stream_data <- readr::read_tsv(discharge_file_path, show_col_types = F)
  # sFilter data with recorded discharge value
  discharge_per_day <- as.data.frame(stream_data) |>
    dplyr::mutate(date = lubridate::ymd_hms(stream_data$`time (MST)`, quiet = T), discharge = stream_data$`Discharge(cfs)-GCMRC-GCLT1`, height = stream_data$`Gage Height(ft)-GCMRC-GCLT1`) |>

    dplyr::filter(lubridate::date(date) == discharge_date) |> # filters out discharge for date "YYYY-MM-DD"
    dplyr::mutate(discharge_diff = round(c(diff(discharge),0),3)) |> # Calculates difference in discharge per time step
    dplyr::filter(discharge > 0 | discharge_diff > 0) # selects locations with measured discharge and point before discharge occur


  # Scale the height data for a particular storm to zero.
  discharge_per_day$height <- round(discharge_per_day$height - min(discharge_per_day$height), 3)

  # Determine the length of discharge
  time_diff <- round(as.double(difftime(data.table::last(discharge_per_day$time), discharge_per_day$time[1], units = "mins")), 3) # number of minutes for particular discharge

  # Discharge graph
  discharge_plot <- ggplot2::ggplot(discharge_per_day) +
    ggplot2::geom_line(ggplot2::aes(x = hms::as_hms(date), y = discharge)) +
    ggplot2::labs(title = paste("Discharge at Waterholes Watershed, AZ:", discharge_date), x = "Time", y = paste0("Measured Discharge ft\u00b3/s)")) + ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # Height graph
  height_plot <- ggplot2::ggplot(discharge_per_day) +
    ggplot2::geom_line(ggplot2::aes(x = hms::as_hms(date), y = height)) +
    ggplot2::labs(title = paste("Stage Height at Waterholes Watershed, AZ:", discharge_date), x = "Time", y = "Measured Height (ft)") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # Combine the two graphs
  combined_plot <- gridExtra::grid.arrange(discharge_plot, height_plot)
  # store the graph for the date
  if(saveGraphs){
    outputPlot <- paste0("discharge_height_plot_",discharge_date,".png")
    ggplot2::ggsave(file.path(save_location, outputPlot), plot = combined_plot, width = 6)
  }
  # store the data as an output
  outputData <- paste0("discharge-data-", discharge_date, ".csv")
  readr::write_csv(discharge_per_day, file = file.path(save_location, outputData))

  return(discharge_per_day)
}
#
# # x <- read_csv(outputData)
#
# # Test
# # discharge_file_path <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data\Waterholes_Stream_gcmrc20231127132459.tsv)"
# # discharge_date <- "2007-07-23"
# # #outPath <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Outputs\Model-2012-07-15\Rainfall-Constant)"
# # outPath <- r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel\Taylor_Model)"
# # x <- dailyDischarge(discharge_file_path = discharge_file_path, discharge_date = discharge_date, save_location = outPath, saveGraphs = T)
# ##------------------------------
# Function: Filter all the days that have discharge, create csv of dates
days_of_discharge <- function(discharge_csv){
  days_table <- readr::read_csv(discharge_csv)
  days <- as.vector(days_table[,1])
  utils::write.csv(days, file = "days-of-disharge.csv")
}
#
# # Test
# # Location of maximum discharge data
# # discharge_csv <- r"(C:\Thesis\Arid-Land-Hydrology\R\max-discharge-per-day.csv)"



