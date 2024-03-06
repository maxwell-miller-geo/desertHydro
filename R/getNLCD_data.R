# Set your working directory to the /data folder included in the repository using setwd().
# Install the following packages for this tutorial:
# install.packages(c("FedData", "rasterVis", "raster","sp", "rgdal", "reshape2", "treemapify", "ggplot2", "kableExtra", "animation", "scales"))
# library(FedData)
# library(rasterVis)
# library(raster)
# library(sp)
# library(rgdal)
# library(reshape2)
# library(treemapify)
# library(ggplot2)
# library(kableExtra)
# library(animation)
# library(scales)
#
# setwd('C:\\Thesis\\Arid-Land-Hydrology')
# # Load a single raster using the raster() function This is the
# r<-raster("nlcd_1.tif")
#
# # What does a raster object look like in R? What are the key attributes of this data format?
# r
## class      : RasterLayer
## dimensions : 4311, 2513, 10833543  (nrow, ncol, ncell)
## resolution : 30.1, 29.6  (x, y)
## extent     : 289422, 365063.3, 4417034, 4544639  (xmin, xmax, ymin, ymax)
## crs        : +proj=utm +zone=13 +datum=WGS84 +units=m +no_defs
## source     : C:/Users/ncinglis/Desktop/stbd/nlcd/nlcd_1.tif
## names      : nlcd_1
## values     : 11, 95  (min, max)
# We have seven different years of NLCD data. The stack() function takes as many rasters as you give it and stacks them into one object.
# If they have the same resolution, extent and spatial reference, this is an easy way to conduct operations on multiple layers at the same time.

# We can load in the NLCD series straight into a stack.I do this so often that I wrote a little function that takes a regular expression and creates a raster stack from the files that match it.

# list2stack <- function(x) {
#   z<-list.files(pattern = x)
#   y<-raster::stack(z)
#   return(y)
# }
#
# # If you want to get into regular expressions, there are entire books on it. Just trust me this one works:
# p<-"^nlcd_.*\\.tif$"
# s<-list2stack(p)
#
# #Let's plot this stack and take a look at it.
# plot(s)
