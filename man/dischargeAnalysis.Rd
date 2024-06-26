% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postAnalysis.R
\name{dischargeAnalysis}
\alias{dischargeAnalysis}
\title{Discharge Analysis after desertHydro simulation}
\usage{
dischargeAnalysis(
  ModelFolder,
  WatershedElements,
  time_step,
  simulation_length,
  discharge = F,
  store = T,
  date = NULL
)
}
\arguments{
\item{ModelFolder}{Model Folder with saved outputs}

\item{WatershedElements}{Watershed Folder with Watershed components. Assumes that}

\item{time_step}{Time in minutes}

\item{simulation_length}{Length of simulation in minutes}

\item{discharge}{T/F: Default TRUE: If TRUE, will look for observed discharge
hydrographs}

\item{store}{T/F: Default TRUE: If TRUE, will store created plots in
ModelFolder}

\item{date}{string. Optional plotting setting to add date to title and output
file.}
}
\value{
Returns discharge excel table and plots from model simulation
}
\description{
Discharge Analysis after desertHydro simulation
}
\examples{
\dontrun{
dischargeAnalysis(ModelFolder, WatershedElements, time_step, simulation_length) #See vignette
}

}
