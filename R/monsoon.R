# Script to run via Monsoon
# Blank function that will just be run once inside of monsoon

monsoon <- function(ModelFolder = NULL){

  if(is.null(ModelFolder)){
    ModelFolder <- tempdir()
  }
  WatershedElements <- dirname(system.file("extdata", "dem.tif", package = "desertHydro"))

  # Necessary variables
  demFile <- "dem.tif"
  boundary <- "waterholes_shape.shp" # Name of shapefile for outline
  landCoverFile <- "soils.shp" # name of soils data
  LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx"
  key <- "MUSYM" # key is present in both the landcoverfile and the land cover characteristics

  # Date options
  date <- "2018-07-13" # Optional - if dates present must be in YYYY-MM-DD format
  rainFile <- "USGS_Rain_2001_2021.xlsx"
  dischargeFile <- "observable-discharge.csv"
  discharge = T # If TRUE, expects discharge present in file
  # If rainfall method is synthetic - uses 1/2 inch - 15 minute storm
  rainfall_method <- "gauges" # Synthetic, Gauges, Spatial
  length <- 10 # grid cell length - assumes uniform grid
  store = T
  time_step <- .5 # time step in minutes
  simulation_length <-  2 # time length in minutes
  impervious <- T # no infiltration
  overwrite <- T # overwrite watershed elements and model folder
  write <- T # create graphs and write outputs to model folder
  restartModel <- T # If model is paused mid-run, can be restarted with same inputs
  gif <- F # If TRUE, gif's will be created automatically. Can be created at the end

  # Check if the necessary folders are present
  foldersToCheck <- c(WatershedElements)
  folders <- sapply(foldersToCheck, FUN = file.exists)
  if(!all(folders)){
    print(paste0("All folders not found. List of folders not found."))
    print(folders)
  }
  # Check if the necessary files are present
  filesToCheck <- c(demFile, boundary, landCoverFile,
                    LandCoverCharacteristics, rainFile, dischargeFile)
  filesCheck <- sapply(file.path(WatershedElements, filesToCheck),
                       FUN = file.exists)
  if(!all(filesCheck)){
    print("All files are not found. Here is a list of the files present.")
    print(filesCheck)
  }else{
    print("All files found.")
  }
  a <- arid_model(ModelFolder,
            WatershedElements,
            date = date,
            demFile = demFile,
            boundary = boundary,
            landCoverFile = landCoverFile,
            LandCoverCharacteristics = LandCoverCharacteristics,
            key = key,
            impervious = impervious,
            rainfall_method = rainfall_method,
            store = store,
            gif = gif,
            discharge = discharge,
            time_step = time_step,
            simulation_length = simulation_length,
            overwrite = overwrite,
            write = write,
            restartModel = restartModel
  )
  return(a)
  gifs <- desertHydro::gifCreation(ModelFolder, rainfall_method,
                                    discharge = discharge, saveGraph = T)
  return(ModelFolder)
}
