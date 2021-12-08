## ---------------------------
##
## Script name: Texture for Tif generation script
##
## Purpose of script: using Eitzel et al. 2016's code to generate texture statistics for my tif mosaic 
##
## Author: Hannah Fertel
##
## Date Created: 2022-01-04
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
  

rm(list=ls())
library(raster)
library(glcm)

historical.ras<-raster("GIS/examplerast.tif")
                       
historical.glcm3<-glcm
historical.glcm5<-glcm(historical.ras,window=c(5,5))
historical.glcm7<-glcm(historical.ras,window=c(7,7))

plot(historical.ras)
plot(historical.glcm3,col=gray(seq(0,1,0.1)))
plot(historical.glcm5,col=gray(seq(0,1,0.1)))
plot(historical.glcm7,col=gray(seq(0,1,0.1)))

glcm_mean<-raster(historical.ras)
glcm_var<-raster(historical.ras)
glcm_homog<-raster(historical.ras)
glcm_con<-raster(historical.ras)
glcm_dis<-raster(historical.ras)
glcm_ent<-raster(historical.ras)
glcm_sm<-raster(historical.ras)

plot(glcm_mean, col=gray(seq(0,1,0.1)))

glcm_mean<-setValues(glcm_mean,values(historical.glcm7$glcm_mean)*10000)
writeRaster(glcm_mean, "WC_1948GLCM_7x7_mean.tif", datatype="INT2S", format = "GTiff",overwrite=TRUE)

glcm_var<-setValues(glcm_var,values(historical.glcm7$glcm_variance)*10000)
writeRaster(glcm_var, "WC_1948GLCM_7x7_var.tif", datatype="INT2S", format = "GTiff",overwrite=TRUE)

glcm_homog<-setValues(glcm_homog,values(historical.glcm7$glcm_homogeneity)*10000)
writeRaster(glcm_homog, "WC_1948GLCM_7x7_homog.tif", datatype="INT2S", format = "GTiff",overwrite=TRUE)

glcm_con<-setValues(glcm_con,values(historical.glcm7$glcm_contrast)*10000)
writeRaster(glcm_con, "WC_1948GLCM_7x7_con.tif", datatype="INT2S", format = "GTiff",overwrite=TRUE)

glcm_dis<-setValues(glcm_dis,values(historical.glcm7$glcm_dissimilarity)*10000)
writeRaster(glcm_dis, "WC_1948GLCM_7x7_dis.tif", datatype="INT2S", format = "GTiff",overwrite=TRUE)

glcm_ent<-setValues(glcm_ent,values(historical.glcm7$glcm_entropy)*10000)
writeRaster(glcm_ent, "WC_1948GLCM_7x7_ent.tif", datatype="INT2S", format = "GTiff",overwrite=TRUE)

glcm_sm<-setValues(glcm_sm,values(historical.glcm7$glcm_second_moment)*10000)
writeRaster(glcm_sm, "WC_1948GLCM_7x7_sm.tif", datatype="INT2S", format = "GTiff",overwrite=TRUE)


