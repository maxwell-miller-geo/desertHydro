# Script to run via Monsoon
# Blank function that will just be run once inside of monsoon

monsoon <- function(ModelFolder = NULL){

  if(is.null(ModelFolder)){
    ModelFolder <- tempdir()
  }
  # Model Class
  model1 <- model(ModelFolder = ModelFolder) # default model characteristics
  model1@date <- "2022-07-24"
  model1@simulation_length <- 15

  a <- arid_model(model1@ModelFolder,
                  model1@watershedPath,
                  date = model1@date,
                  demFile = model1@demFile,
                  boundary = model1@boundary,
                  landCoverFile = model1@landCoverFile,
                  LandCoverCharacteristics = model1@LandCoverCharacteristics,
                  key = model1@key,
                  impervious = model1@impervious,
                  rainfall_method = model1@rainMethod,
                  store = model1@store,
                  gif = model1@gif,
                  discharge = model1@discharge,
                  time_step = model1@timeStep,
                  simulation_length = model1@simulation_length,
                  overwrite = model1@overwrite,
                  write = model1@write,
                  restartModel = model1@restartModel
                  )

  gifs <- desertHydro::gifCreation(model1@ModelFolder, model1@rainMethod, date = model1@date,
                                   discharge = model1@discharge, saveGraph = T)
  # Watershed Elements folder
  # WatershedElements <- dirname(system.file("extdata", "dem.tif", package = "desertHydro"))
  #
  # # Necessary variables
  # demFile <- "dem.tif"
  # boundary <- "waterholes_shape.shp" # Name of shapefile for outline
  # landCoverFile <- "soils.shp" # name of soils data
  # LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx"
  # key <- "MUSYM" # key is present in both the landcoverfile and the land cover characteristics
  #
  # # Date options
  # date <- "2022-07-24" # Optional - if dates present must be in YYYY-MM-DD format
  # rainFile <- "USGS_Rain_2001_2021.xlsx"
  # rainFile <- "USGS_Rain_2022.xlsx"
  # dischargeFile <- "example_discharge.csv"
  # discharge <-  T # If TRUE, expects discharge present in file
  # ## If rainfall method is synthetic - uses 1/2 inch - 15 minute storm
  # rainfall_method <- "gauges" # Synthetic, Gauges, Spatial
  # length <- 10 # grid cell length - assumes uniform grid
  #
  # time_step <- .5 # time step in minutes
  # simulation_length <-  NA # time length in minutes
  # store <-  T
  # impervious <- T # no infiltration
  # overwrite <- T # overwrite watershed elements and model folder
  # write <- T # create graphs and write outputs to model folder
  # restartModel <- T # If model is paused mid-run, can be restarted with same inputs
  # gif <- F # If TRUE, gif's will be created automatically. Can be created at the end
  #
  # # Check if the necessary folders are present
  # foldersToCheck <- c(WatershedElements)
  # folders <- sapply(foldersToCheck, FUN = file.exists)
  # if(!all(folders)){
  #   print(paste0("All folders not found. List of folders not found."))
  #   print(folders)
  # }
  # # Check if the necessary files are present
  # files <- c(demFile, boundary, landCoverFile,
  #                   LandCoverCharacteristics, rainFile, dischargeFile)
  # file_paths <- paste0(WatershedElements, "/", files)
  # file_check(file_paths)

  # filesCheck <- sapply(file.path(WatershedElements, filesToCheck),
  #                      FUN = file.exists)
  # if(!all(filesCheck)){
  #   print("All files are not found. Here is a list of the files present.")
  #   print(filesCheck)
  # }else{
  #   print("All files found.")
  # }

  # a <- arid_model(ModelFolder,
  #           WatershedElements,
  #           date = date,
  #           demFile = demFile,
  #           boundary = boundary,
  #           landCoverFile = landCoverFile,
  #           LandCoverCharacteristics = LandCoverCharacteristics,
  #           key = key,
  #           impervious = impervious,
  #           rainfall_method = rainfall_method,
  #           store = store,
  #           gif = gif,
  #           discharge = discharge,
  #           time_step = time_step,
  #           simulation_length = simulation_length,
  #           overwrite = overwrite,
  #           write = write,
  #           restartModel = restartModel
  # )
  gifs <- desertHydro::gifCreation(ModelFolder, rainfall_method, date = date,
                                    discharge = discharge, saveGraph = T)
  return(ModelFolder)
}


# Monsoon Variables
# Inputs - List of variables that can be modified for each model run
# 1) Date - 7-24-2023 or NA (defaults example rainfall)
# 2) Rainfall method - None, gauge weighted, gauge average, gauge spatial, goes (5)
# 2a) Average quantity of rainfall, maximum intensity, total duration
# 3) Time step - length of default time step used
# 4) Land Cover type - soils or geo soils
# 4a) Land cover modified - slope adjustment factor, geo soils adjustment,

# Outputs - Variables that will be compared to
# 1) Peak discharge
# 2) Water Volume
# 3) Nash-Sutcliffe, Kling-Gupta
# 4) Peak timing
# 5) Optional - recession curve

