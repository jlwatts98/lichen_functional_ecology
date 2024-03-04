##### Header: Call this at the beginning of every script #####

# Clear workspace
rm(list = ls())
graphics.off()

# hello world
# Source custom functions
source("functions.R")

# libraries
# visualization
library(ggplot2)
library(ggpubr)

# spatial
library(raster)
library(terra)
library(sp)
library(sf)
library(stars)
library(fasterize)
library(ncdf4)
library(rasterVis)

# temporal
library(lubridate)

# ecophys
library(photobiology)
library(meteoland)

# utility
library(dplyr)
library(plyr)
library(tidyverse)
library(readr)
library(data.table)

# correlations
library(grid)
library(PerformanceAnalytics)
library(corrplot)
library(psych)





