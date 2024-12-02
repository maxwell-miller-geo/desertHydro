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
monsoon <- function(ModelFolder = NULL, date = "2021-07-22", model_type = "default", rain_method = "gauges", courant = 0.8, time = NaN, overwrite = NA,...){
  if(is.null(ModelFolder)){
    ModelFolder <- tempdir()
  }
  if(model_type == "default"){
    model1 <- infil_model(ModelFolder)
    model1@key <- "MUSYM"
  }
  if(model_type == "s-runoff"){
    model1 <- small_impervious(ModelFolder)
  }
  if(model_type == "s-infil"){
    model1 <- small_infiltration(ModelFolder)
  }

  # Insert function to mess with the default options through additional inputs

  #return(list(...))
  # Model Class
  # model1 <- model(ModelFolder = ModelFolder) # default model characteristics
  # model1@overwrite <- overwrite
  # model1@date <- date
  # model1@simulation_length <- time
  # model1@demFile <-  dem
  # model1@boundary <- desertHydro::polygonize(model1@demFile, model1@watershedPath)
  # model1@rainMethod <- rain_method
  # model1@courant <- courant
  # model1@gif <- gif
  # model1@restartModel <- restart
  # # Model variation - NLCD
  # model1@LandCoverCharacteristics <- land_cover
  # model1@landCoverFile <- "waterholes_LC.tif"
  # model1@key <- "ID"
  # model1@impervious <- impervious
  # Added stuff
  model1@date <- date
  model1@rainMethod <- rain_method
  model1@courant <- courant

  if(!is.nan(time)){
    model1@simulation_length <- time
  }
  if(!is.na(overwrite)){
    model1@overwrite <- overwrite
  }
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
  if(model1@gif){
    gifs <- desertHydro::gifCreation(model1@ModelFolder, model1@rainMethod, date = model1@date,
                                     discharge = model1@discharge, saveGraph = T)
  }
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

