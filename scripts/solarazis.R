## ---------------------------
##
## Script name: Solar azimuth generation 
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
library(solarPos)
library(tidyverse)


jd <- julianDay(2003,10,17,12,30,30,tz=-7)

azi<-read.csv("Data/AziData.csv")
azi1<-azi %>% 
  mutate(jd=julianDay(year = X.1,month = Date,day = X,hour = HR,min = Min,sec = Sec,tz=-8))


azi2<-as.data.frame(solarPosition(azi1$jd,azi1$Center_L_2,azi1$Center_L_1))

azi3<-cbind(azi1,azi2)  

avezen<-mean(azi3$zenith)
aveazi<-mean(azi3$azimuth)
