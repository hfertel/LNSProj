## ---------------------------
##
## Script name: Rastercalculator script 
##
## Purpose of script:
##
## Author: Hannah Fertel
##
## Date Created: 2021-11-17
##
## Copyright (c) Hannah Fertel, 2021
## Email: hmfertel@berkeley.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(data.table)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 

## ---------------------------

library(rgdal)
library(raster)

Slope<-raster("GIS/hist_mosaic")
plot(slope)
