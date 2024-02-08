# Functions that automate plotting


## Function that takes a raster stack, resamples, and produces a melted dataframe

meltStack <- function(rasterStack, resample = 5, timevalues = xvalues){ # assumes terra raster stack in latitude/longitude
  # Check if it is a filepath or a loaded in raster
  if(is.character(rasterStack)){ # if passed a file string instead of SpatRaster object
    rasterStack <- terra::rast(rasterStack)
  }
  # Down sample the raster images
  downsampledstack <- terra::aggregate(rasterStack, fact = resample, fun = "mean", na.rm = F) |> 
    subset(2:nlyr(rasterStack)) # removes first layer
  
  # Create names for layers - create names for vector within the model function 
  #names(downsampledstack) <- seq(1: nlyr(downsampledstack)) # number of layers
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
  animated <- ggplot(meltedDF, aes(x = x, y = y, fill = value, frame = Time)) +
              geom_raster() +
              scale_fill_viridis_c(direction = -1, option = "viridis") +
              ggtitle("{title} - Time {round(frame_time)} (minutes)") +
              xlab("Longitude") + 
              ylab("Latitude") + 
              labs(fill = units, caption = caption) + 
              theme(plot.title = element_text(hjust = 0.5)) +
              theme_dark() + 
              transition_time(Time)
  return(animated)
}
