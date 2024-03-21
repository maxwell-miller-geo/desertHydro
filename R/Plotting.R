# Functions that automate plotting
##----------------------------------
# Function for creating the discharge plot
# dischargeEstimate <- function(surfaceRaster, surfaceVelocity, shapefile, gridsize = 10, height = .01){
#   #x_sections_path <- file.path(ModelElements, "Cross_Section_lines.shp")
#   cross_section <- terra::vect(shapefile) # bring vector into R
#   # # Extract the height from the surface stack
#   surface_Height <- terra::extract(surfaceStorage, cross_section) # surface height in cm
#   surface_velocity <- terra::extract(surfaceVelocity, cross_section) # velocity at given time (m/s)
#   cm_to_m2 <- .01 * 10 # conversion factor - Conversion to m time grid size
#   surface_discharge <- (as.numeric(surface_Height[,3:ncol(surface_Height)]* cm_to_m2) * surface_velocity[,3:ncol(surface_velocity)])
#   xvalues <- as.numeric(colnames(surface_Height[,3:ncol(surface_Height)]))
#   estimated <- data.frame(time = xvalues, predDis = as.numeric(as.vector(surface_discharge[1,])))
#   return
# }

## Function that takes a raster stack, resamples, and produces a melted dataframe
meltStack <- function(rasterStack, resample = 1, timevalues = xvalues){ # assumes terra raster stack in latitude/longitude
  # Check if it is a filepath or a loaded in raster
  if(is.character(rasterStack)){ # if passed a file string instead of SpatRaster object
    rasterStack <- terra::rast(rasterStack)
  }
  if((dim(rasterStack)[1] * dim(rasterStack)[2]) > 160000){
    resample <- 5
  }
  # Down sample the raster images
  downsampledstack <- terra::aggregate(rasterStack, fact = resample, fun = "mean", na.rm = F) |>
    subset(1:nlyr(rasterStack)) # removes first layer

  # Create names for layers - create names for vector within the model function
  #names(downsampledstack) <- seq(1: nlyr(downsampledstack)) # number of layers
  print(length(names(downsampledstack)))
  print(length(timevalues))
  names(downsampledstack) <- as.numeric(timevalues)
  # Create dataframe
  stackdf <- terra::as.data.frame(downsampledstack, xy = T)

  # Melt the stacked data frame
  meltedDF <- reshape2::melt(stackdf, variable.name = "Time", id = c("x", "y"))

  # Convert "Time" to numeric and remove missing values
  meltedDF$Time <- as.numeric(as.character(meltedDF$Time))
  meltedDF <- na.omit(meltedDF)
  return(meltedDF)
}

# Test the meltStack functionality
# test_file <- r"(C:\R-EES-680\final-project-maxwell-miller-geo\Data\Models-7-28-22\3in_rainfall_constant\Surface_Storage.tif)"
# testStack <- terra::rast(test_file)
# meltedStack <- meltStack(rasterStack = testStack)

# Create an animation stack
animateStack <- function(meltedDF, title = "", units = "", caption = ""){
  requireNamespace("ggplot2")
  x <- y <- value <- Time <- NULL # binding local variables
  animated <- ggplot2::ggplot(meltedDF, ggplot2::aes(x = x, y = y, fill = value, frame = Time)) +
              ggplot2::geom_raster() +
              ggplot2::scale_fill_viridis_c(direction = -1, option = "viridis") +
              ggplot2::ggtitle("{title} - Time {round(frame_time)} (minutes)") +
              ggplot2::xlab("Longitude") +
              ggplot2::ylab("Latitude") +
              ggplot2::labs(fill = units, caption = caption) +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
              ggplot2::theme_dark() +
              gganimate::transition_time(Time)
  return(animated)
}
