# Regression analysis - PCA
#install.packages("FactoMineR")
#install.packages("corrplot")
library(corrplot)
library(FactoMineR)
library(tidyverse)
#library(stats)
# Function that combines two datasets by time 
combineData <- function(xDF, yDF, write = F, jointype = "left"){
  x <- read_csv(xDF)
  y <- read_csv(yDF)
  if(jointype == "left"){
    combine <- left_join(x, y)
    combine[is.na(combine)] <- 0
  }
  if(jointype == "right"){
    combine <- right_join(x, y)
    combine[is.na(combine)] <- 0
  }
  
  # if(is.array(combine$temp_c)){
  #   combine$temp_c <- approx(combine$temp_c)
  # }
  # Fill in missing discharges with zeros
  
  if(write){
    filename <- paste0("rain-discharge-combined.csv")
    write_csv(combine, filename)
  }
  return(combine)
}
# Test - load in example .csv files
# Day test
rainDay <- "rain-data-hour.csv" # in root directory
streamDay <- "max-discharge-per-hour.csv" # in root dir.

xDF <- rainDay
yDF <- streamDay

combinedDF <- combineData(xDF, yDF, write = F, jointype = "right") # input for regression analysis

# Calculate the correlation of the data.frame

# Filter out some columns
dfFiltered <- combinedDF[,c(-1)]
cor.df <- cor(dfFiltered)
corrplot(cor.df, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#ggsave("Correlation-plot.png", plot = p)

# PCA Analysis
day_PCA <- PCA(dfFiltered) # I guess remove the time variable

