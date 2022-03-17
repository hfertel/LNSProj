## ---------------------------
##
## Script name: Veg Change Other Analysis Script
##
## Purpose of script:
##
## Author: Hannah Fertel
##
## Date Created: 2022-09-01
##
## Copyright (c) Hannah Fertel, 2022
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

#####Mean Patch Size####

#1948
patches1948<-read.csv("Data/patches1948.csv")

#ok do the weighted mean by group
#create dataframes with just values for each group?

herb48<-patches1948 %>% 
  filter(Prediction=="Herbaceous")
woodland48<-patches1948 %>% 
  filter(Prediction=="Woodland")
Forest48<-patches1948 %>% 
  filter(Prediction=="Forest")
Shrub48<-patches1948 %>% 
  filter(Prediction=="Shrub")

mean1948<-c(weighted.mean(herb48$hectares,herb48$hectares),
            weighted.mean(woodland48$hectares,woodland48$hectares),
            weighted.mean(Forest48$hectares,Forest48$hectares),
            weighted.mean(Shrub48$hectares,Shrub48$hectares))
mean1948

#2014 weighted patch size 
patches2014<-read.csv("Data/patches2014.csv")

herb14<-patches2014 %>% 
  filter(Prediction=="Herbaceous")
woodland14<-patches2014 %>% 
  filter(Prediction=="Woodland")
Forest14<-patches2014 %>% 
  filter(Prediction=="Forest")
Shrub14<-patches2014 %>% 
  filter(Prediction=="Shrub")

mean2014<-as.data.frame(c(weighted.mean(herb14$hectares,herb14$hectares),
            weighted.mean(woodland14$hectares,woodland14$hectares),
            weighted.mean(Forest14$hectares,Forest14$hectares),
            weighted.mean(Shrub14$hectares,Shrub14$hectares)))
mean2014

VegType<-c("Herbaceous","Woodland","Forest","Shrub")

meantable<-cbind(mean2014,mean1948,VegType)

colnames(meantable)<-c("2014","1948", "VegTYpe")


#####modeling of change####

#want to bring in random points shapefile
#then want to bring in rasters of slope, elevation, aspect, and other climate variables 
library(spatstat)
library(terra)
library(sp)
library(sf)


