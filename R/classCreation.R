# Create reference class for input watershed elements
check_model <- function(object){
  errors <- character()
  if(object@timeStep > 1){
    msg <- paste("The default time step increment is to large.")
    errors <- c(errors, msg)
  }
  if(!file.exists(object@watershedPath)){
    msg <- paste("The watershed elements path does not exist.")
    errors <- c(errors, msg)
  }
  if(file.exists(object@watershedPath)){
    files <- c(object@demFile, object@boundary, object@landCoverFile,
               object@LandCoverCharacteristics, object@rainFile, object@dischargeFile)
    file_paths <- paste0(object@watershedPath, "/", files)
    if(!is.null(file_check(file_paths))){
      msg <- paste("Please check file paths")
      errors <- c(errors, msg)
    }
  }
  if(length(errors) == 0) TRUE else errors
}

model <- methods::setClass("model",
           slots = c(
                  ModelFolder = "character",
                  date = "character",
                  rainMethod = "character",
                  timeStep = "numeric", # in minutes
                  landCover = "character",
                  landCoverMod = "character",
                  watershedPath = "character",
                  demFile = "character",
                  boundary = "character",
                  landCoverFile = "character",
                  LandCoverCharacteristics = "character",
                  key = "character",
                  rainFile = "character",
                  dischargeFile = "character",
                  length = "numeric",
                  simulation_length = "numeric",
                  discharge = "logical",
                  store = "logical",
                  impervious = "logical",
                  overwrite = "logical",
                  write = "logical",
                  restartModel = "logical",
                  gif = "logical",
                  courant = "numeric",
                  soilstack = "character"
                  ),

            prototype = list(ModelFolder = NA_character_,
                             date = NA_character_,
                             rainMethod = "gauges",
                             timeStep = 1,
                             landCover = "NLCD",
                             landCoverMod = NA_character_,
            watershedPath = dirname(system.file("extdata", "dem.tif", package = "desertHydro")),
                             demFile = "dem.tif",
                             boundary = "waterholes_shape.shp",
                             landCoverFile = "soils.shp",
            LandCoverCharacteristics = "LandCoverCharacteristics_Soils.xlsx",
                             key = "MUSYM",
                            rainFile = "USGS_Rain_2022.xlsx",
                            dischargeFile = "example_discharge.csv",
                            length = NULL,
                            simulation_length = NaN,
                            discharge = T,
                            store = T,
                            impervious = T,
                            overwrite = T,
                            write = T,
                            restartModel = T,
                            gif = F,
                            courant = 0.8,
                            soilstack = "model_soil_stack.tif"
                            ),

            validity = check_model)

# setMethod("path", "model", function(watershedPath, demFile, boundary, landCoverFile, LandCoverCharacteristics, rainFile, dischargeFile){
#   files_to_adjust <- c(demFile, boundary, landCoverFile, LandCoverCharacteristics, rainFile, dischargeFile)
#   lapply(files_to_adjust, file.path(watershedPath, files_to_adjust))
# }
# )

# Modify model object to certain type
infil_model <- function(ModelFolder){
  object <- model()
  object@ModelFolder <- ModelFolder
  object@LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx"
  object@key <- "MUSYM"
  object@impervious <- F
  return(object)
}

# Modify model object to small scale impervious
small_impervious <- function(ModelFolder){
  object <- model()
  object@ModelFolder <- ModelFolder
  object@LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx"
  object@key <- "MUSYM"
  object@impervious <- T
  object@demFile <- "dem-test.tif"
  object@overwrite <- F
  object@restartModel <- F
  object@boundary <- desertHydro::polygonize(object@demFile, object@watershedPath)
  return(object)
}

# Modify model to small scale with infiltration
small_infiltration <- function(ModelFolder){
  object <- model()
  object@ModelFolder <- ModelFolder
  object@LandCoverCharacteristics <- "LandCoverCharacteristics_Soils.xlsx"
  object@key <- "MUSYM"
  object@impervious <- F
  object@demFile <- "dem-test.tif"
  object@overwrite <- F
  object@restartModel <- F
  object@boundary <- desertHydro::polygonize(object@demFile, object@watershedPath)
  object@soilstack <- file.path(ModelFolder, object@soilstack)
  return(object)
}
