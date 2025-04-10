# Script to run via Monsoon
# Blank function that will just be run once inside of monsoon

# monsoon <- function(ModelFolder = NULL,
#                     time = NaN,
#                     date = "2021-07-22",
#                     dem = "dem-test.tif",
#                     rain_method = "gauges",
#                     overwrite = T,
#                     courant = 0.8,
#                     impervious = T,
#                     restart = F,
#                     gif = F,
#                     land_cover = "nlcd_characteristics.xlsx"){
#' Complete Model Runs
#'
#' Function that allows multiple model runs and has the ability to work on
#' cluster nodes. Note: The functionality of the script is based upon original work
#' and is not flexible enough for everyday use.
#'
#' @param ModelFolder Path to folder to put output files
#' @param date Date string of event in format "YYYY-MM-DD", default
#' @param model_type Sets infiltration and size parameters, default = "default"
#' Other options include:
#' "l-runoff" = large scale model with no infiltration,
#' "s-runoff" = small scale model with no infiltration,
#' "l-infiltration" = large scale model with infiltration,
#' "s-infiltration" = small scale model with infiltration
#' @param rain_method type of rainfall to be used. default = "gauges" from gauge
#' data found within the Watershed folder path.
#' Other options include:
#' "spatial" = spatially distributed rainfall based on gauge placements
#' "goes" = satellite derived rainfall estimates obtained from AWS servers for current
#' spatial extent of model
#' @param depth_adjusted Adjust the depth. Default "slope" method
#' @param surface_method Methods for adjusting the surface roughness. Defaults
#' "soils+stream+geo"
#' @param infiltration_method Methods for infiltration. Adjusts the infiltration
#' and chains with the method of infiltration. Often Green-Ampt method. Defaults
#' "soils+geo+green"
#' @param initial_soil_conditions Soil Saturation initially. Defaults "normal"
#' @param rain_adj % change in rainfall. Default 1 (100%) of recorded values
#' @param surface_adj % change in surface roughness Default 1 (100%) of recorded values
#' @param infiltration_adj % change in infiltration Default 1 (100%) of recorded values
#' @param courant the distance information travels within a mesh network. Values
#' should range between 0.1 and 1.0. The smaller the Courant number, the shorter
#' the time steps will be, which leads to more numerical stability. Default value is
#' 0.8; however, research has shown that for arid regions Courant values as low as
#' 0.1 are needed. The smaller the Courant number the longer simulations will take.
#' @param time Default NaN, which will calculate the length of the simulation based
#' upon the discharge and rainfall input data. Setting time (minutes) to an explicit value
#' will have the simulation run for only that amount of time. Helpful in testing
#' parameters without performing an entire rainfall simulation.
#' @param overwrite Default NA, will change depending on which model_type was selected
#' @param ... Additional parameters (advanced)
#'
#' @return returns path to Model Folder
#' @export
#'
#' @examples \dontrun{
#' # Change this to output folder
#' ModelFolder <- "Rainfall-2021-07-22"
#' model_output <- monsoon(ModelFolder = ModelFolder,
#'                         date = "2021-07-22",
#'                         model_type = "l-runoff",
#'                         rain_method ="gauges",
#'                         courant = 0.3)
#' }
monsoon <- function(ModelFolder = NULL,
                    date = "2021-07-22",
                    model_type = "default",
                    rain_method = "gauges",
                    depth_adjusted = "slope",
                    surface_method = "soils+stream+geo",
                    infiltration_method = "soils+geo+green",
                    initial_soil_conditions = "normal",
                    rain_adj = 1,
                    surface_adj = 1,
                    infiltration_adj = 1,
                    courant = 0.8,
                    time = NaN,
                    overwrite = NA,
                    velocity_method = "darcys",
                    debug = F,
                    ...){

  if(is.null(ModelFolder)){
    ModelFolder <- tempdir()
  }
  if(!file.exists(ModelFolder)){
    dir.create(ModelFolder)
  }
  if(model_type == "default"){
    model1 <- infil_model(ModelFolder)
    model1@key <- "MUSYM"
  }
  if(model_type == "l-runoff"){
    model1 <- infil_model(ModelFolder)
    model1@key <- "MUSYM"
    model1@impervious <- T
  }
  if(model_type == "s-runoff"){
    model1 <- small_impervious(ModelFolder)
    model1@impervious <- T
    model1@key <- "MUSYM"
  }
  if(model_type == "s-infil"){
    model1 <- small_infiltration(ModelFolder)
    model1@key <- "MUSYM"
  }

  # surface_method = surface_method,
  # infiltration_method = infiltration_method,
  # rain_adj = rain_adj,
  # surface_adj = surface_adj,
  # infiltration_adj = infiltration_adj,
  # Create log file - optional
  # my_log <- file(file.path(ModelFolder, "log.txt"))
  # sink(my_log, append = TRUE, type = "output") # Writing console output to log file
  # on.exit(sink())
  # Insert function to mess with the default options through additional inputs
  # Added stuff - should be passed as arguments

  model1@date <- date
  model1@rainMethod <- rain_method
  model1@courant <- courant

  if(!is.nan(time)){
    model1@simulation_length <- time
  }
  if(!is.na(overwrite)){
    model1@overwrite <- overwrite
    if(model1@overwrite == T){
      model1@restartModel <- F
    }
  }
  model1@gif <- T

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
                  restartModel = model1@restartModel,
                  surface_method = surface_method,
                  infiltration_method = infiltration_method,
                  initial_soil_conditions = initial_soil_conditions,
                  rain_adj = rain_adj,
                  surface_adj = surface_adj,
                  infiltration_adj = infiltration_adj,
                  velocity_method = velocity_method,
                  debug = debug
                  )

  if(model1@gif){
    gifs <- desertHydro::gifCreation(model1@ModelFolder, model1@rainMethod, date = model1@date,
                                     discharge = model1@discharge, saveGraph = T)
  }

  # Output directory
  outputs <- file.path(ModelFolder, paste0("Outputs-", basename(ModelFolder)))

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
                    "Starting_Soil_Characteristics.csv")
  # Copy files to Output folder, if they exists
  lapply(file.path(ModelFolder, output_files), FUN = copy_if_exists, outputs)

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

