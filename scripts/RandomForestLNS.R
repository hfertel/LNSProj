## ---------------------------
##
## Script name: RandomForestLNS
##
## Purpose of script: Random forest classifier for vegetation segments
##
## Author: Hannah Fertel
##
## Date Created: 2022-03-02
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

library(randomForest)
library(sf)
#https://www.edureka.co/blog/random-forest-classifier/


#read in training data
trainingsegs<-read_sf(dsn = "C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/GIS Files/Segments/Trainingsegs21941.shp")

#cleaning
trainsegs1<-as.data.frame(trainingsegs) %>% 
  dplyr::filter(Train_ID %in% c("Woodland","Forest","Herbaceous","Shrub")) %>% 
  dplyr::select(Brightness, GLCM_Ang_2,GLCM_Contr,GLCM_Dissi,GLCM_Entro,GLCM_Hom_1, 
                GLCM_Mean_, GLCM_StdDe, Max_pixel, Min_pixel, Mean_Layer,GLCMcont3,
                quantile25,quantile50,quantile75, Skewness_L,Train_ID)

table(trainsegs1$Train_ID)

# Converting ‘Survived’ to a factor
trainsegs1$Train_ID <- factor(trainsegs1$Train_ID)
# Set a random seed
set.seed(51)
# Training using ‘random forest’ algorithm
RF1<-randomForest(Train_ID ~ .,data=trainsegs1,importance=TRUE,proximity=TRUE)

print(RF1)



#Cross validation for models with sequentially reduced predictors! --would want to do that      

#need to figure out how to apply the random forest model to the rest of the data??!
#load in shapefile w/ all segments and try to apply in the predict function? 


All_Segs<-read_sf(dsn = "C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/GIS Files/Segments/1941_clip.v9.shp")

allsegs2<-as.data.frame(All_Segs) %>% 
  dplyr::select(Brightness, GLCM_Ang_2,GLCM_Contr,GLCM_Dissi,GLCM_Entro,GLCM_Hom_1, 
                GLCM_Mean_, GLCM_StdDe, Max_pixel, Min_pixel, Mean_Layer,GLCMcont3,
                quantile25,quantile50,quantile75, Skewness_L) %>% 
  

predict1<-as.data.frame(predict(RF1, allsegs2))
table(predict1)

#bind predictions to shapefile
All_Segs<-cbind(All_Segs,predict1)


#Will want to cross validate? calculate areas and then compare for modern data set 
               

