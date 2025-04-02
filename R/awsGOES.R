# AWS CLI interface with GOES Series

 get_GOES_Rainfall <- function(ModelFolder, date = "2021-07-22", region = "us-east-1",
                              WatershedElements = model()@watershedPath, remove = T, all = F, hours_adj = 1){
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
    cat("The date:", as.Date(date), "is out of bounds of GOES AWS records. Check to ensure input date is in format
        YYYY-MM-DD")
    return(NULL)
  }
  cat("Using AWS bucket", bucket,"\n")
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
  if(!file.exists(table)){
    rain <- rainfallFilter(date, ModelFolder, WatershedElements)
    table <- dischargeCreate(date, ModelFolder,WatershedElements, rain_file = rain,discharge = T)
  }
  times <- get_start_end_time(table)
  # Search two hours earlier
  times$start <- times$start - 60*60* hours_adj#(secs*minutes*hours = 2 hours)
  if(all | is.null(times)){
    start <- as.POSIXct(paste(date, "00:00"), tz = "MST")
    end <- start + 23 *60 *60
    times <- data.frame(start = start, end = end)
  }
  # Convert times into UTC
  times_format <- format(times, tz = "UTC")
  # Determine starting and ending time - could be adjust for no observations
  start <- times_format$start[[1]]
  end <- times_format$end[[1]]
  # Determine start day
  number_day_start <-  as.numeric(strftime(as.Date(start), format = "%j"))
  number_day_end <- as.numeric(strftime(as.Date(end), format = "%j"))
  # Determine hours
  hour_start <- as.numeric(strftime(start, format = "%H"))
  hour_end <- as.numeric(strftime(end, format = "%H"))

  # Different days according to UTC time
  cat("Retrieving rainfall from AWS\n")
  # Determine number of days
  days <- seq(number_day_start, number_day_end)
  ordered_rain_paths <- lapply(days, FUN = retrieve_and_order_GOES, days, hour_start, hour_end, bucket, pattern, region, year, ModelFolder)
  # Remove NA's if present
  ordered_files <- unlist(ordered_rain_paths)
  # browser()
  stack <- create_GOES_raster(ordered_files)
  # Load in boundary
  model_dem <- terra::rast(file.path(WatershedElements, "model_dem.tif")) # Bad practice - should at least be Model Folder
  # Crop and resize
  resize_rain <- resizeImagery(imagery = stack, outline = model_dem, targetRaster = model_dem)
  # Convert mm/h into in/hr
  # rain_in <- resize_rain/25.4
  # Convert object from nc to spatial format of model
  # Save stacked raster for model
  rain_file <- file.path(ModelFolder, paste0(date,"-goes.tif"))
  terra::writeRaster(resize_rain, rain_file, overwrite = T)
  cat("Created GOES Rain file in", ModelFolder, "\n")
  # Remove raw files
  if(remove){
    cat("Removing raw files\n")
    lapply(ordered_files, file.remove)
  }
  return(rain_file)
}

hour_sequence <- function(hour_start, hour_end, days, day){
  #browser()
  if(hour_start >= hour_end & day == 1){ # added equals
    hours <- seq(hour_start, 23) # Sequences first day to the end
  }else if(hour_start >= hour_end & day != 1){
    # Second to nth day ideally
    if(day == length(days)){ # If last day
      hours <- seq(0, hour_end)
    }else if(day != length(days)){ # If middle day
      hours <- seq(0, 23) # for middle days grab all hours of data
    }
  }else if(hour_start < hour_end & length(days) == 1){ # if only one day
    hours <- seq(hour_start, hour_end)
  }
  cat("Processing day", day, "of", length(days), "\n")
  return(as.numeric(sprintf("%02d", hours)))
}


create_GOES_raster <- function(ordered_files, start ="", end = ""){
  # Load in files - should come in ordered...
  name <- extract_time_string(ordered_files[[1]])
  first <- terra::rast(ordered_files[[1]])[[1]] # select rainfall
  names(first) <- name
  raster_list <- lapply(ordered_files, FUN = function(x){
    r <- terra::rast(x)[[1]]
    names(r) <- extract_time_string(x)
    return(r)
  })
  stacked_raster <- terra::rast(raster_list)
  return(stacked_raster)
}

retrieve_and_order_GOES <- function(day, days, hour_start, hour_end, bucket, pattern, region, year, ModelFolder){
  x <- which(days == day)
  day_of <- day
  year_day <- paste0(year,"/",day,"/")
  # Pattern to obtain rainfall from a given day
  pattern <- paste0("ABI-L2-RRQPEF/", year_day)
  # Files names for a given day
  files <- aws.s3::get_bucket(bucket, prefix = pattern, region = region, use_https = TRUE, use_signed_url = FALSE)
  # Hour pattern for a given day
  hours <- hour_sequence(hour_start, hour_end, days, day = x)
  hour_pattern <- paste0("(",paste(sprintf("%02d", hours), collapse = "|"),")")
  # Select files with matching strings
  regex <- paste0("s",year,day_of,hour_pattern)
  names <- lapply(1:length(files), function(x){files[[x]]$Key}) # grab necessary buckets
  idxs <- grep(regex, names)
  # Catch any NULLS
  if(length(idxs) == 0){
    cat("Unable to use GOES-18 or GOES-17: Searching GOES-16\n")
    bucket <- "noaa-goes16"
    # Files names for a given day
    files <- aws.s3::get_bucket(bucket, prefix = pattern, region = region, use_https = TRUE, use_signed_url = FALSE)
    names <- lapply(1:length(files), function(x){files[[x]]$Key}) # grab necessary buckets
    idxs <- grep(regex, names)
    if(length(idxs) == 0){
      cat("No GOES Data found for year:", year, "day:", day_of,"\n")
      return(NA)
    }
  }
  # Select files for range of hours
  selected_files <- files[idxs]
  # Save necessary files
  save_files <- lapply(selected_files, function(x){
    path <- file.path(ModelFolder, tail(strsplit(x$Key, "/")[[1]],1))
    if(!file.exists(path)){
      aws.s3::save_object(x, file = path) # other get it when needed
    }
    return(path)
  })
  # Sort the selected files by time
  ordered_files <- order_by_time(unlist(save_files, use.names = F))
  return(ordered_files)
}
