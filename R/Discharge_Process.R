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

dischargeCreate <- function(date, ModelFolder, WatershedElements, rain_file = NULL, discharge = F, store = T, discharge_file_path = "waterholes_discharge_2001_2024.tsv"){
  time <- Total_in <- NULL
  # Load in the filtered rainfall file
  rainFiltered_file <- file.path(ModelFolder, paste0("rain-data-", date,".csv"))
  rain_discharge_file <- file.path(ModelFolder, "rain-discharge.csv")

  print("Processing discharge data...")
  if(discharge){
    if(file.exists(rain_discharge_file) & file.exists(rainFiltered_file)){
      print("Found discharge data")
      return(rain_discharge <- readr::read_csv(rain_discharge_file, show_col_types = F))
    }else{
      print("Creating discharge data")
      # Check if discharge present on day - returns the date or optional date
      #date <- lubridate::date(discharge_present(WatershedElements, date))[1] # returns discharge date or next day (first     entry)
      #print(date)
      # Load in stream data from Waterholes - GCMRC
      dischargeDataPath <- file.path(WatershedElements, discharge_file_path) #- sloppy
      # Calculate the daily discharge for given date
      dischargeDF <- dailyDischarge(discharge_file_path = dischargeDataPath,
                                    discharge_date = date,
                                    save_location = ModelFolder,
                                    saveGraphs = store)
      if(is.null(dischargeDF)){
        return(cat("No discharge found for", date,"\n"))
      }
      # Create filtered rainfall
      rainFiltered <- rainfallFilter(date, ModelFolder, WatershedElements, overwrite = F)
      if(is.null(rainFiltered)){
        return(cat("No rainfall found for", date,"\n"))
      }
      # Combine the rainfall and discharge into a single .csv file
      rain_discharge <- rainfall_discharge_combine(rainfallDF = rainFiltered,
                                                   dischargeDF,
                                                   outpath = ModelFolder,
                                                   store = store,
                                                   date = date)
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
dischargeTotal <- function(discharge_file, write = F, WatershedElements = ""){
    print("Calculating total discharge values")
    out_discharge <- file.path(WatershedElements, "observable-discharge.csv")
    if(file.exists(out_discharge)){
      observable_discharge <- utils::read.csv(out_discharge)
      return(as.data.frame(observable_discharge))
    }
    date_time <- height <- discharge <- temp_c <- stream <-  NULL
    stream <- data.table::fread(discharge_file) # reads tsv and csv files

    #stream_data <- readr::read_tsv(discharge_file, show_col_types = FALSE)
    # Filter data with recorded discharge values- case sensitive
    timeIndex <- stringMatch(stream, guessName = "time", string = F)
    heightIndex <- stringMatch(stream, guessName = "Height", string = F)
    dischIndex <- stringMatch(stream, guessName = "Discharge", string = F)
    #tempIndex <- stringMatch(stream, guessName = "temp", string = F)

    #filt <- zoo::na.approx(stream[, ..tempIndex]) # filter out NA values
    # print(length(filter))
    # print(length(stream[, ..tempIndex]))
    #stream[, tempIndex := zoo::na.approx(tempIndex)]
    #stream[,tempIndex:=zoo::na.approx(stream[tempIndex])] # filter out NA values

    observable_discharge <- as.data.frame(stream) |>
      dplyr::mutate(date_time = stream[, ..timeIndex],
                    height = stream[, ..heightIndex],
                    discharge = stream[, ..dischIndex]) |>
      dplyr::select(date_time, height, discharge) |>
      #dplyr::mutate(temp_c = ifelse(temp_c < -50, NA, temp_c)) |> # deal with values less then possible (-999)
      dplyr::filter(discharge > 0) # filter out all the discharge greater than 0

  if(write){
    data.table::fwrite(observable_discharge, file = file.path(WatershedElements, "observable-discharge.csv"))
  }
  return(as.data.frame(observable_discharge))
}
# # Test
# # discharge_file <- r"(C:\Thesis\Arid-Land-Hydrology\Data\Waterhole\Temporal_Data\Waterholes_Stream_gcmrc20231127132459.tsv)"
# # dischargeDF <- dischargeTotal(discharge_file = discharge_file, write = F)
#
# # Function that resamples the data to different time scales - mean approximation
# # date_time, height, discharge, temp_c
dischargeResample <- function(dischargeDF, ModelFolder = NULL, units = "day", write = F){
  # Mutate columns

  resampledDF <- dischargeDF |>
    stats::aggregate(discharge ~ lubridate::floor_date(date_time, unit = units), FUN = max) |>
    purrr::set_names(units, "maxDischarge") # want the peaks
  if(write){
    filename <- paste0("max-discharge-per-", units, ".csv")
    readr::write_csv(resampledDF, file.path(ModelFolder, filename))
  }
  return(resampledDF)
}
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
## ----------------------------------------------
# # Create a function to look up if discharge is recorded on a particular date
discharge_present <- function(data_folder, date, ModelFolder = NULL, discharge_name = "observable-discharge.csv", write = T){
  # Currently only works for 1 downloaded gauge - does select by stream gauge
  stream_data <- file.path(data_folder, discharge_name)
  if(!file.exists(stream_data)){ # check file location
    stop(paste0("Could not locate discharge data in ", data_folder))
  }else{
    print("Located discharge data.")
  }

  # Use the discharge - total function to reorganize the stream gauges by day
  # then compare if input date - before or after exists
  #print(stream_data)
  dischargeDF <- suppressWarnings(dischargeTotal(stream_data, write = write, WatershedElements = data_folder)) # dataframe with recorded discharge values (>0)

  # use the dischargeResample function to obtain the highest discharge for a each day
  dischargeDaily <- dischargeResample(dischargeDF, units = "day") # dataframe with discharge per day

  # Assuming the input date is in YYYY/MM/DD
  #date <- lubridate::ymd(date) # convert date
  date <- lubridate::parse_date_time(date, "ymd")
  date_after <- date + lubridate::days(1) # day after
  date_before <- date + lubridate::days(-1)
  discharge_dates <- c()
  # Check search date, before, and after
  if(date %in% dischargeDaily$day){
    print(paste0("Discharge recorded on ", date))
    discharge_dates <- append(discharge_dates, date)
  }
  if(date_after %in% dischargeDaily$day){
    print(paste0("Discharge recorded on ", date_after))
    discharge_dates <- append(discharge_dates, date_after)
  }
  if(is.null(discharge_dates)){
    stop(paste0("No recorded discharge found on ", date, " or ", date_after))
  }
  return(discharge_dates)
}
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
rainfall_discharge_combine <- function(rainfallDF, dischargeDF, outpath, store = T, trim = T, date = NULL){
  # # Create zeros within the data create a sequence of time from the rainfall DF
  # time_seq <- data.frame(Time_minute = seq(rainfallDF$Time_minute[1], tail(rainfallDF$Time_minute,1), by = "min"))
  # # Change gauge names
  time <- Time_minute <- NULL
  colnames(rainfallDF) <- stringr::str_replace_all(colnames(rainfallDF), "-", "_")
  colnames(rainfallDF) <- stringr::str_replace_all(colnames(rainfallDF), "[.]", "_")
  # # This step combines the rainfall and the discharge data into 1 dataframe - dirty
  # rain_seq <- time_seq |>
  #   dplyr::left_join(rainfallDF, by = "Time_minute") |>
  #   tidyr::replace_na(list( time = 0))
  # # This step combines the rainfall and the discharge data into 1 dataframe - dirty

  # Get the first time recorded for the discharge and rainfall
  discharge_start <- dischargeDF$date_time[1]
  rain_start <- rainfallDF$Time_minute[1]

  if(rain_start < discharge_start){
    time_diff <- as.numeric(difftime(discharge_start, rain_start, units = "mins"))
    # Append discharge time at the end of rainfall
    rainfallDF <- rainfallDF |>
      dplyr::add_row(Time_minute = discharge_start,
                     time = time_diff,
                     WATER_1 = 0, WATER_2 = 0, WATER_G = 0, Total_in = 0, # little hard coded
                     .before = 1) |>
      dplyr::arrange(Time_minute)
  }

  rain_discharge <- dplyr::full_join(rainfallDF, dischargeDF, by = dplyr::join_by("Time_minute" == "date_time"))
  # interpolate between values
  # Selectable columns
  # determine columns
  rain_cols <- grep("WATER", names(rain_discharge), value = T)
  cols <- c("Time_minute", "Total_in", "discharge", "height", rain_cols) # add column for other gauges

  # Filter columns
  rain_discharge <- rain_discharge |>
    dplyr::select(cols) |>
    dplyr::arrange(rain_discharge$Time_minute)

  #return(rain_discharge)
  # Perform linear approximation on discharge and height values
  rain_discharge$discharge <- round(zoo::na.approx(rain_discharge$discharge, na.rm = F), 4)
  rain_discharge$height <- round(zoo::na.approx(rain_discharge$height, na.rm = F), 2)
  #return(rain_discharge)
  # Replace NA values with 0s
  rain_discharge[is.na(rain_discharge)] <- as.integer(0)

  # Add a line of zeros at the beginning
  rain_discharge <-  rain_discharge |>
    dplyr::mutate(time = (as.numeric(rain_discharge$Time_minute - base::min(rain_discharge$Time_minute)) / 60) + 1) |>
      dplyr::add_row(Time_minute = c(rain_discharge[1,1] - lubridate::minutes(1)),
                        Total_in = 0,
                       discharge = 0,
                          height = 0,
                            time = 0, .before = 1) |>
                      dplyr::arrange(Time_minute) |>
                      dplyr::mutate(difftime = c(0, diff(time)))

  # Replace NA values with 0s
  rain_discharge[is.na(rain_discharge)] <- as.integer(0)
  # Cut off values if rainfall occurs after discharge recedes
  #which(rain_discharge$difftime > 60)[1])){
  index <- tail(which(rain_discharge$discharge > 0), 1) + 1
  #index <- which(rain_discharge$difftime > 60)[1] # grab first time hour jump
  if(index < nrow(rain_discharge) & trim){ # remove last entries if little discharge
      rain_discharge <- rain_discharge[1:index,] # remove end points
   }
  # Writes the values into csv
  if(store){
    filename <- file.path(outpath, paste0("rain-discharge-",date,".csv"))
    filename <- file.path(outpath, paste0("rain-discharge.csv"))
    readr::write_csv(x = rain_discharge, file = filename)
    # Read csv
    #x <- readr::read_csv(r"(C:\Thesis\Arid-Land-Hydrology\R\Example\SampleModel\Demo_Test\2022-07-15\rain-discharge.csv)")
  }
  return(rain_discharge)
}
# # Test - see rain-discharge set-up
# # Rain
#
# # # Plot rainfall - discharge
plot_rainfall_discharge <- function(rain_discharge_DF, date, store = T, outpath = "", gauge_prefix = "WATER"){
  # Clean up variables
  Time_minute <- discharge <- Total_in <- NULL
  # Widen the data of rainfall with the melt, which will take the gauge columns and
  # Create categories under the "Gauge" column and keep the values in a new column called
  # "Rain_depths" and preserve the "Time_minute" column
  data.table::setDT(rain_discharge_DF)

  # Find gauges with "Water"
  gauges <- grep(gauge_prefix, colnames(rain_discharge_DF), ignore.case = T, value = T)
  meltedDF <- data.table::melt(data.table::as.data.table(rain_discharge_DF),
                               id.vars = "Time_minute",
                               measure.vars = gauges,
                               variable.name = "Gauge",
                               value.name = "Rain_in")

  # Determine how malny gauges there are
  rain_plot <- ggplot2::ggplot(meltedDF, mapping = ggplot2::aes(x = Time_minute, y = Rain_in, fill = Gauge)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = paste("Rainfall at Waterholes Watershed, AZ:", date),
                  x = "", y = "Measured Rainfall (in)") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = c(1, 1),               # Moves legend to top-right
                   legend.justification = c(1, 1),          # Aligns top-right corner of legend box
                   legend.background = ggplot2::element_rect(fill = "white", color = "black"),  # Adds visible border
                   legend.margin = ggplot2::margin(2, 2, 2, 2),
                   legend.title.align = 0.5) +       # Ensures padding inside legend box)+  # Add background for clarity +
    ggplot2::scale_fill_viridis_d()


  discharge_plot <- ggplot2::ggplot(rain_discharge_DF) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = Time_minute, y = discharge))  +
    #labs(title = paste("Discharge at Waterholes Watershed, AZ:", date), x = "", y = bquote("Measured Discharge" (ft^3/s))) +
    ggplot2::labs(title = paste("Discharge at Waterholes Watershed, AZ:", date), x = "", y = paste0("Measured Discharge (ft\u00b3/s)")) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    # Combine the two graphs
    combined_plot <- gridExtra::grid.arrange(discharge_plot, rain_plot)

    # Create additional combined plot with discharge

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
  stream_data <- data.table::fread(discharge_file_path)
  # find colum with time
  # time <- stringMatch(stream_data, "time")
  # time_col <- stream_data[,..time][[1]]
  # convertTime <- lubridate::ymd_hms(time_col)
  # stream_data[, time_date := as.POSIXct(time_col, format = "%Y-%m-%d %H:%M:%S")]
  # as.POSIXct(stream_data[,1], format ="%Y-%m-%d %H:%M:%S")
  #
  # sFilter data with recorded discharge value
  # Determine column names in dataset
  #timeIndex <- which(sapply(stream_data, inherits, "POSIXct"))
  timeIndex <- grep("time", colnames(stream_data), value = T)[[1]]
  dischargeIndex <-  grep("Discharge", colnames(stream_data), value = T)[[1]]
  heightIndex <- grep("Height", colnames(stream_data), value = T)[[1]]
  cols <- c(timeIndex, dischargeIndex, heightIndex)
  date_filtered <- stream_data[as.Date(get(timeIndex)) == as.Date(discharge_date)]
  date_filtered <- date_filtered[,..cols]
  # shifted_dis <- data.table::shift(date_filtered[[dischargeIndex]], type ="lead", fill = 0)
  # date_filtered$difference <- shifted_dis - date_filtered[[dischargeIndex]]
  # end_of_discharge <- date_filtered[get(dischargeIndex) != 0, .I[.N]] + 1
  # end_of_discharge <- date_filtered[date_filtered[[dischargeIndex]] != 0, .I][.N]
  end_of_discharge <- tail(which(date_filtered[[dischargeIndex]] != 0), 1) + 1 # when discharge becomes zero
  begin_discharge <- head(which(date_filtered[[dischargeIndex]] != 0), 1) - 1 # when discharge starts as zero
  if(length(end_of_discharge)==0){
    return(NULL)
  }
  if(end_of_discharge < nrow(date_filtered)){
    # drop rows after last index
    date_filtered <- date_filtered[1:end_of_discharge]
  }
  if(begin_discharge > 1){
    date_filtered <- date_filtered[begin_discharge:nrow(date_filtered)]
  }
  # Shift the discharge
  #time_index <- grep(date_key, colnames(dt), value = T) # Returns first column name with "time"
  #date_vector <- unique(as.Date(dt[[time_index]]))
  # discharge_per_day <- stream_data |>
  #   dplyr::mutate(date_time = stream_data[, ..timeIndex],
  #                 height = stream_data[, ..heightIndex],
  #                 discharge = stream_data[, ..dischargeIndex]) |>
  #   dplyr::select(date_time, height, discharge)
    #dplyr::mutate(temp_c = ifelse(temp_c < -50, NA, temp_c)) |> # deal with values less then possible (-999)
    #dplyr::filter(discharge > 0)

  # discharge_per_day <- as.data.frame(stream_data) |>
  #   dplyr::mutate(date = lubridate::ymd_hms(stream_data$`time (MST)`, quiet = T), discharge = stream_data$`Discharge(cfs)-GCMRC-GCLT1`, height = stream_data$`Gage Height(ft)-GCMRC-GCLT1`) |>
  #   dplyr::filter(lubridate::date(date) == discharge_date) |> # filters out discharge for date "YYYY-MM-DD"
  #   dplyr::mutate(discharge_diff = round(c(diff(discharge),0),3)) |> # Calculates difference in discharge per time step
  #   dplyr::filter(discharge > 0 | discharge_diff > 0) # selects locations with measured discharge and point before discharge occur

  discharge_per_day <- date_filtered
  # Change column names
  cols <- c("date_time", "discharge", "height")
  names(discharge_per_day) <- cols
  # Scale the height data for a particular storm to zero.
  discharge_per_day$height <- round(discharge_per_day$height - min(discharge_per_day$height), 3)

  # Determine the length of discharge
  time_diff <- round(as.double(difftime(data.table::last(discharge_per_day$time), discharge_per_day$time[1], units = "mins")), 3) # number of minutes for particular discharge

  # Discharge graph
  discharge_plot <- ggplot2::ggplot(discharge_per_day) +
    ggplot2::geom_line(ggplot2::aes(x = date_time, y = discharge)) +
    ggplot2::labs(title = paste("Discharge at Waterholes Watershed, AZ:", discharge_date), x = "Time", y = paste0("Measured Discharge (ft\u00b3/s)")) + ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # Height graph
  height_plot <- ggplot2::ggplot(discharge_per_day) +
    ggplot2::geom_line(ggplot2::aes(x = date_time, y = height)) +
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

## ------------------------ Height -> discharge
stream_gauge_discharge <- function(height, time_elapsed, units = "cm", cellsize = NULL, raster = NULL){
  if(!is.null(raster)){
    cellsize <- grid_size(raster)
  }

  # Height adjustments
  height <- as.numeric(height)
  length_diff <- length(height) - length(time_elapsed)

  if(length_diff == 1){
    # Add extra zero to time-steps
    time_elapsed <- as.numeric(c(Inf, time_elapsed))
  }
  # height_shifted <- data.table::shift(height, n = 1, fill = 0)
  # bind_heights <- rbind(height, height_shifted)
  # averageHeight <- colMeans(bind_heights)

  if(units == "cm"){
    cm_to_m <- .01 # conversion factor - Conversion to m
    m3_to_ft3 <- 35.3147
    Q <- (height * cm_to_m * cellsize^2* m3_to_ft3) / time_elapsed
  }
  return(round(Q,3))
}

##----------------------------- Volume of discharge
# Function that takes discharge data and integrates for volume
dischargeVolume <- function(dischargeDF, time_col = "time"){
  time_col <- stringMatch(dischargeDF, time_col)
  # discharge <- stringMatch(dischargeDF, "discharge")
  meanQ <- zoo::rollapply(dischargeDF$discharge, width = 2, FUN = mean, by = 1, align = "left")
  #datetime <- as.POSIXct(dischargeDF[,time], format = "%Y-%m-%d %H:%M:%S")
  diffTime <- as.numeric(diff(dischargeDF[[time_col]]), units = "secs")
  volumePerTimestep <- diffTime * meanQ # volume per time (s * cfs) = ft3
  volume <- sum(volumePerTimestep, na.rm = T) # volume in ft^3
  #date <- lubridate::date(dischargeDF[,time][[1]])
  #volume <- data.table::data.table(date = date, volume_cfs = sum(volumePerTimestep, na.rm = T)[[1]])
  #data.table::fwrite(volume, file.path(ModelFolder, "observed-discharge-volume.csv"))
  return(volume)
 }

#' Discharge-Rainfall Comparisons
#' Determine the discharge amounts, lengths, and rainfall amounts for a set of given dates
#' Function is not very flexible in current state.
#'
#' @param discharge_file table containing a list of discharge dates
#' @param ModelFolder Path to save location
#' @param WatershedElements Path to location of saved stream data and rainfall data
#' @param store T/F If TRUE, saves elements to ModelFolder
#'
#' @return Returns and saves csv file into Model Folder. Optional saves recorded
#' discharge and rainfall events as excel spreadsheet.
#' @export
#'
#' @examples \dontrun{
#' # Change this to output folder
#' discharge_file <- "C:/path_to_discharge/discharge_file.csv"
#' ModelFolder <- r"(C:/Thesis/Arid-Land-Hydrology/R/Example/SampleModel)"
#' WatershedElements <- file.path("inst/extdata/DemoElements") # demo elements
#' discharge_events <- discharge_rainfall_events(discharge_file, ModelFolder,
#'                                       WatershedElements, store = T)
#' }
discharge_rainfall_events <- function(discharge_file, ModelFolder, WatershedElements = model()@watershedPath, store = F){
  # Assumes observable discharge file is in location
  out_excel <- file.path(ModelFolder, "DischargeEvents.xlsx")
  if(file.exists(out_excel)){
    cat("Reading previously created DischargeEvents.xlsx")
    sheets <- readxl::excel_sheets(out_excel)
    sheets_list <- lapply(sheets, function(sheet) {
      readxl::read_xlsx(out_excel, sheet = sheet)
    })
    names(sheets_list) <- sheets
    dates_filtered <- sheets_list
  }else{
  # Discharge file needs to just be observable discharge dates...
  dates <- get_dates(discharge_file)
  # Create discharge events from those dates
  discharge_events <- lapply(dates, dischargeCreate, ModelFolder, WatershedElements, discharge = T, store = store)
  # Assign names to list
  names(discharge_events) <- dates
  # Filter out null dates
  dates_filtered <- remove_nulls(discharge_events)
  # If saved
  if(store){
    # Save stuff
    wb <- openxlsx::createWorkbook()
    for (sheet_name in names(dates_filtered)){
      openxlsx::addWorksheet(wb, sheet_name)# Add a worksheet
      openxlsx::writeData(wb, sheet_name, dates_filtered[[sheet_name]])
      }# Write data to the sheet
    openxlsx::saveWorkbook(wb, out_excel, overwrite = TRUE)
    cat("Workbook saved as 'DischargeEvents.xlsx'.\n")
  }
  }
  analysis <- lapply(dates_filtered, event_analysis)
  # Create dataframe/data.table from the list
  df <- do.call(rbind, lapply(analysis, as.data.frame))
  # Save the dataframe
  utils::write.csv(x = df, file = file.path(ModelFolder, "input-list.csv"))
  #df <- data.frame(date = names(dates_filtered), duration = )
  return(df)
}

# Determines the very specific qualities of a dataframe - not-flexible
event_analysis <- function(dataframe, key_gauge = "water"){
  if(is.character(dataframe)){
    dataframe <- data.table::fread(dataframe)
  }
  # If a single discharge occurs remove it
  if(length(unique(dataframe$discharge)) < 4){
    cat("Less than 4 discharge events recorded.\n")
    return(NULL)
  }
  discharge_duration <- duration(dataframe$discharge, dataframe$Time_minute)
  discharge_max <- max(dataframe$discharge) #cfs
  rainfall_duration <- duration(dataframe$Total_in, dataframe$Time_minute)
  if(length(rainfall_duration) == 0){
    rainfall_duration <- 0
  }
  rain_total <- sum(dataframe$Total_in) # all rainfall - should be all rainfall for an event
  # Determine the gauge rainfall
  gauges <- stringMatch(dataframe, key_gauge, multi = T)
  gauge_rainfall <- data.frame(t(colSums(dataframe[,gauges])))
  event_duration <- max(dataframe$time)
  lag_time <- lag_time(dataframe)# first instance of rainfall and peak discharge
  if(length(lag_time) == 0){
    lag_time <- NA
  }
  #rainfallIntensity()
  discharge_volume <- dischargeVolume(dischargeDF = dataframe) # ft^3
  rainfall_volume <- rainfallVolume(dataframe, method = "split") # ft3??
  # Get date
  cal_date <- unique(as.Date(dataframe$Time_minute))[[1]]
  out_df <- data.frame(Date = cal_date,
                      event_duration_min = event_duration,
                      rainfall_duration_min = rainfall_duration,
                      rain_total_in = rain_total,
                      rainfall_vol_ft3 = rainfall_volume,
                      rainfall_vol_kilo_ft3 = rainfall_volume/1000^3,
                      max_discharge_ft3s = discharge_max,
                      discharge_volume_ft3 = discharge_volume,
                      discharge_duration_min = discharge_duration,
                      lag_time_min = lag_time
                      )
  out_df <- cbind(out_df, gauge_rainfall)
  return(out_df)
}

# Calculate durations of recorded data assumes trailing and leading zeros
duration <- function(data_col, time_col, units = "mins"){
  # recombined
  df <- data.frame(time_col, data_col)
  start_index <- head(which(df$data_col != 0),1) # first non-zero
  end_index <- tail(which(df$data_col != 0),1) # last non-zero
  # Computer duration in minutes
  time_elapsed <- as.numeric(df$time_col[end_index] - df$time_col[start_index], units = units)
  return(time_elapsed)
}

lag_time <- function(dataframe,units = "mins"){ # assumes dataframe has Time_min, discharge, Total_in
  rain_start <- dataframe[head(which(dataframe$Total_in != 0),1),]$Time_minute
  discharge_peak <- dataframe[which(dataframe$discharge == max(dataframe$discharge)),]$Time_minute
  time_elapsed <- as.numeric(discharge_peak - rain_start, units = units)
  return(time_elapsed)
}

rainfallVolume <- function(dataframe, method = "total", rain_area_file = file.path(model()@watershedPath, "voronoi.shp")){
  # Check if path or dataframe
  if(is.character(dataframe)){
    dataframe <- data.table::fread(dataframe)
  }
  if(is.character(rain_area_file)){
    rain_area_file <- terra::vect(rain_area_file)
  }
  if(method == "total"){
    total <- stringMatch(dataframe, "total")
    rainfall_depth <- sum(dataframe[[total]]) / 12 # inches/12 = ft
    # Determine rainfall area
    m2_to_ft2 <- 10.7639
    watershed_area <- sum(terra::expanse(rain_area_file, unit = "m") * m2_to_ft2)
    rain_volume <- rainfall_depth * watershed_area
    return(watershed_area)
  }
  if(method == "split"){
    gauges <- stringMatch(dataframe, "Water", multi = T)
    rainfall_depth <- dataframe[,gauges] / 12 # inches/12 = ft
    # Standardize the names: replace underscores with hyphens
    names(rainfall_depth) <- gsub("_", "-", names(rainfall_depth))
    # Determine rainfall area
    m2_to_ft2 <- 10.7639
    # Matching the mis-ordered data from dataframe into order of shapefile table
    gauge_idx <- grep(names(rain_area_file), "gauge", ignore.case = T)
    gauge_names <- c(terra::values(rain_area_file[,gauge_idx]))[[1]] # convert to ordered character vector
    order_rainfall <- colSums(rainfall_depth[,gauge_names])
    # Combine the rainfall depth and watershed area
    watershed_area <- terra::expanse(rain_area_file, unit = "m") * m2_to_ft2
    rain_volume <- sum(order_rainfall * watershed_area)
    return(rain_volume)
  }
}

# Correlation matrix
correlate <- function(df){
  corre <- cor(df[,2:ncol(df)])
  corrplot::corrplot(corre, "square")
  return(corre)
}

# Plot two variables with the linear square regression line
plot_corre <- function(df, col1 = "rainfall_vol", col2 = "discharge_duration", factors = NULL){
  # Find the x column
  col1 <- stringMatch(df, col1)
  col2 <- stringMatch(df, col2)
  # drop na rows
  df1 <- na.omit(df1)
  # Correlate the two variables
  x <- df1[[col1]]
  y <- df1[[col2]]
  # Determine the range of x
  xlimit <- range(x)[2] *.85
  ylimit <- range(y)[1] * 1.1
  if(ylimit < 0){
    ylimit <- range(y)[1]*.9
  }
  coeff <- lm(y~0+x) # linear model coefficients
  rsquared <- round(summary(coeff)$r.squared,3)
  yint <- 0
  slope <- coeff$coefficients[[1]]
  p <- ggplot2::ggplot(df1, mapping = ggplot2::aes(x = get(col1), y = get(col2))) +
    ggplot2::geom_point() +
    ggplot2::labs(x = beautify(col1), y = beautify(col2), title = paste(beautify(col1),"versus", beautify(col2), ": Waterholes")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::geom_abline(intercept = yint, slope = slope, col = "blue") +
    ggplot2::annotate("text", x = xlimit, y = ylimit, label = paste("Correlation coefficient:",rsquared))
  #p + ggplot2::geom_smooth(method = "lm", se = F)
  return(p)
}

plot_rainfall_line_dual_axis <- function(combined_dt, primary_col = "discharge", secondary_col = "Cumulative_rainfall", date = "", store = TRUE, outpath = "") {

  # Ensure data.table format
  data.table::setDT(combined_dt)
  # Total rainfall column
  total_column <- grep("Total", names(combined_dt), value = T)
  # Cumulative sum column
  combined_dt[, paste0("Cumulative_rainfall") := cumsum(.SD[[1]]), .SDcols = total_column]
  # Scale factor to align axes
  max_primary <- max(combined_dt[[primary_col]], na.rm = TRUE)
  max_secondary <- max(combined_dt[[secondary_col]], na.rm = TRUE)
  scale_factor <- if (max_secondary == 0) 1 else max_primary / max_secondary * 0.5

  # Plot
  joint_plot <- ggplot2::ggplot(combined_dt, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = get(primary_col)), color = "blue", size = .65) +
    ggplot2::geom_line(ggplot2::aes(y = get(secondary_col) * scale_factor), color = "red", size = .75, linetype = "dashed") +
    ggplot2::scale_y_continuous(
      name = paste("Measured Discharge (ft\u00b3/s)"),
      sec.axis = ggplot2::sec_axis(~ . / scale_factor, name = paste("Cumulative Rainfall (in)"))
    ) +
    ggplot2::labs(
      title = paste("Rainfall vs. Discharge:", date),
      x = "Time (minutes)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.title.y.left = ggplot2::element_text(color = "blue"),
      axis.text.y.left = ggplot2::element_text(color = "blue"),
      axis.title.y.right = ggplot2::element_text(color = "red"),
      axis.text.y.right = ggplot2::element_text(color = "red")
    )

  if (store) {
    file_name <- paste0("rainfall_line_dual_axis_", date, ".png")
    ggplot2::ggsave(file.path(outpath, file_name), plot = joint_plot, width = 8, height = 5)
  }

  return(joint_plot)
}
