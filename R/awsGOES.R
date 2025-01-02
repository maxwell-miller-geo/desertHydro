# AWS CLI interface with GOES Series

get_GOES_Rainfall <- function(date = "2022-07-15", region = "us-east-1", ModelFolder, WatershedElements = model()@watershedPath){
  ModelFolder <- "C:/PackageDev/desertHydro/Results/example/"
  # Calculate the day of the year
  date <- as.Date(date)
  year <- as.numeric(strftime(date, format = "%Y"))
  day_of_year <- as.numeric(strftime(date, format = "%j"))
  # 1) Compare date string to operational capacity of satellites
  # Error handle if range is out of reach
  # Goes 18 Opeation between End of 2022 to 2025
  if(year > 2023 & year < 2025){
    bucket <- "noaa-goes18"
    # Goes 17 Operational Between Feb 2019 to Oct 2023
  }else if(year > 2020 & year < 2023){
    bucket <- "noaa-goes17"
    # Goes 16 Operational between end of 2019 - by day (e.g. 334) - 2025
  }else if(year > 2020 & year < 2025){
    bucket <- "noaa-goes16"
  }else{
    cat("The date:", date, "is out of bounds of GOES AWS records. Check to ensure input date is in format
        YYYY-MM-DD")
    return(NULL)
  }
  # No access key required
  Sys.setenv("AWS_ACCESS_KEY_ID" = "",
             "AWS_SECRET_ACCESS_KEY" = "",
             "AWS_DEFAULT_REGION" = region) # Replace with the region of the bucket if known.)
  # Primary bucket called - ABI-L2-RRQPEF


  # 3) Convert given date into day of year
  # Date-times are given from Greenich time
  # Convention -- OR_ABI-L2-RRGQPEF-M6-G16-sYYYYDDDHHMMSSS-eYYYYDDHHMMSSS-cYYYYDDHHMMSSS.nc
  # Format for observation start/end & file creation times:
  #   YYYY (4-digit year)
  # DDD (3-digit Julian day)
  # HHMM (4-digit hour/minutes in UTC)
  # SSS (3-digit seconds to tenth of a second)
  # 4) Determine range of times that are necessary
  # Grab rainfall or discharge data
  table <- file.path(ModelFolder, "rain-discharge.csv")
  times <- get_start_end_time(table)

  # Convert times into UTC
  times_format <- format(times, tz = "UTC")
  start <- times_format$start[[1]]
  end <- times_format$end[[1]]
  number_day_start <-  as.numeric(strftime(as.Date(start), format = "%j"))
  number_day_end <- as.numeric(strftime(as.Date(end), format = "%j"))
  hour_start <- strftime(start, format = "%H")
  last_hour <- strftime(end, format = "%H")
  # 5) Retrieve and save data to session or to files
  # 6) Crop data to a given region or spatial extension
  # 7) Convert from mm to in and create raster stack of rainfall
  folder_prefix <- paste0("ABI-L2-RRQPEF/", year, "/",number_day_start)
  files <- aws.s3::get_bucket(bucket, prefix = folder_prefix, region = region, use_https = TRUE, use_signed_url = FALSE)
  # Get full file name
  file_name <- tail(strsplit(files[1]$Contents$Key, "/")[[1]],1)
  loc <- aws.s3::save_object(files[[1]], file = file.path("Results/", file_name))
  x <- get_object(files[[1]]) # loads first file locally
  # Parsing the date string ---

  # Determine spatial resolution - change
  model_dem <- terra::rast(file.path(WatershedElements, "model_dem.tif")) # Bad practice - should at least be Model Folder
  # Crop and resize
  resize_rain <- resizeImagery(r, model_dem, model_dem)
  # Convert object from nc to spatial format of model

  # Stack layers in correct order

  }
