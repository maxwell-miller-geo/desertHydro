# Run monsoon batch files
require(desertHydro)
require(devtools)
# Expect multiple arguments from command line
args <- commandArgs(trailingOnly = T)
# Expected argument order
date <- as.character(args[1]) # date
model_type <- as.character(args[2]) # infiltration methods
rain_method <- as.character(args[3]) # rain method
surface_method <- as.character(args[4]) # surface roughness method
depth_adjusted <- "slope"

if(is.na(surface_method)){
  surface_method <- "soils+stream+geo"
}
infiltration_method <- as.character(args[5]) # infiltration method
if(is.na(infiltration_method)){
  infiltration_method <- "soils+geo+green"
}
# Additional methods
rain_adj <- as.numeric(args[6]) # rainfall adjustment
surface_adj <- as.numeric(args[7]) # surface adjustment percent
infiltration_adj <- as.numeric(args[8]) # infiltration method percent adj
initial_soil_conditions <- "normal"

modifiers <- list(rain_adj =  rain_adj,
                  surface_method = surface_method,
                  surface_adj = surface_adj,
                  infiltration_method = infiltration_method,
                  infiltration_adj = infiltration_adj)

# courant <- as.numeric(args[4])
#run <- paste0("st",substr(Sys.time(), 1, 10), "-h", substr(Sys.time(), 12, 13),"-")
run <- paste0(date, "-", model_type, "-", rain_method, "-adj-", rain_adj, "-",
              surface_method, "-adj-", surface_adj, "-", infiltration_method,
              "-adj-", infiltration_adj)
# Hard coded right now
time <- NaN
overwrite <- T
courant <- 0.8
folderName <- paste0(run, date,"-", rain_method, "-", model_type, "-c-", courant)

desertHydro:::monsoon(folderName,
                      date = date,
                      model_type = model_type,
                      rain_method = rain_method,
                      surface_method = surface_method,
                      infiltration_method = infiltration_method,
                      depth_adjusted = depth_adjusted,
                      rain_adj = rain_adj,
                      surface_adj = surface_adj,
                      infiltration_adj = infiltration_adj,
                      overwrite = overwrite,
                      courant = courant,
                      time = time)
