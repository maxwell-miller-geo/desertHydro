% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demProcessing.R
\name{flow_accumlation_wb}
\alias{flow_accumlation_wb}
\title{Hydrological pre-processing workflow for digital elevation models}
\usage{
flow_accumlation_wb(
  dem_file_path,
  ModelFolder,
  watershed_shape_path = NA_character_,
  smooth = T,
  max_dist = 11,
  stream_threshold = NULL,
  carve = 0,
  overwrite = T
)
}
\arguments{
\item{dem_file_path}{path to digital elevation file}

\item{ModelFolder}{Output path to save model outputs}

\item{watershed_shape_path}{Optional shapefile to clip the resulting results}

\item{smooth}{T/F: If TRUE, will smooth stream segements to remove flat areas}

\item{max_dist}{numeric value in meters to breach the digital elevation model}

\item{stream_threshold}{Flow accumulation number (cells) to create channels}

\item{carve}{numeric. Number of meters to carve the channel network down by.}

\item{overwrite}{T/F. If TRUE, the model will overwrite previous files in
Model Folder}
}
\value{
SpatRaster of the modified digital elevation model
}
\description{
Hydrological pre-processing workflow for digital elevation models
}
\examples{
ModelFolder <- tempdir() # Create a temporary directory to save files into
}
