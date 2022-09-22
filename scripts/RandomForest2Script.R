## ---------------------------
##
## Script name: RandomForestLNS Attempt 2
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
trainingsegs<-read_sf(dsn = "C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/GIS Files/Segments/Segments1941_8_15_22.shp")

#cleaning
trainsegs1<-as.data.frame(trainingsegs) %>% 
  dplyr::filter(Train_ID2 %in% c("Woodland","Forest","Herbaceous","Shrub")) %>% 
  dplyr::select(Brightness, GLCM_Ang_2,GLCM_Contr,GLCM_Dissi,GLCM_Entro,GLCM_Hom_1, 
                GLCM_Mean_, GLCM_StdDe, Max_pixel, Min_pixel, Mean_Layer,GLCMcont3,
                quantile25,quantile50,quantile75, Skewness_L,Train_ID2)

table(trainsegs1$Train_ID2)

# Converting ‘Survived’ to a factor
trainsegs1$Train_ID2 <- factor(trainsegs1$Train_ID2)
# Set a random seed
set.seed(61)
# Training using ‘random forest’ algorithm
RF1<-randomForest(Train_ID2 ~ .,data=trainsegs1,importance=TRUE,proximity=TRUE)

print(RF1)



#Cross validation for models with sequentially reduced predictors! --would want to do that 

set.seed(647)
msegs <- trainsegs1[1:16]
result <- rfcv(msegs, trainsegs1$Train_ID2, cv.fold=3)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

#figure out which variables are important
varImpPlot(RF1)

varUsed(RF1, by.tree=FALSE,count = TRUE)



mtry <- tuneRF(trainsegs1[1:16],trainsegs1$Train_ID2, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
#get best number of variables for each tree 

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


set.seed(52)
RF2<-randomForest(Train_ID2 ~ .,data=trainsegs1, mtry=4,importance=TRUE,proximity=TRUE)

print(RF2)
#was able to lower error rate somewhat 



#need to figure out how to apply the random forest model to the rest of the data??!
#load in shapefile w/ all segments and try to apply in the predict function? 


All_Segs<-read_sf(dsn = "C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/GIS Files/Segments/StudySegs_41/Segs1941Final.shp")

All_Segs<-as.data.frame(All_Segs) %>% 
  dplyr::select(Brightness, GLCM_Ang_2,GLCM_Contr,GLCM_Dissi,GLCM_Entro,GLCM_Hom_1, 
                GLCM_Mean_, GLCM_StdDe, Max_pixel, Min_pixel, Mean_Layer,GLCMcont3,
                quantile25,quantile50,quantile75, Skewness_L,UID) %>% 
  na.omit()


predict1<-as.data.frame(predict(RF2, All_Segs))
table(predict1)

#bind predictions to shapefile
All_Segs<-cbind(All_Segs,predict1)


#export shapefile or as a table? 

#need to assign FID or some unique identifier to join to polygons in Arc 

write.csv(All_Segs,"C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/GIS Files/SegsPredict41.csv", row.names = FALSE)

#Will want to cross validate? calculate areas and then compare for modern data set 


#####Modern Data ####
#read in training data
trainingsegsmod<-read_sf(dsn = "C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/GIS Files/Segments/Segments2014_8_15_22.shp")

#cleaning
trainsegsmod1<-as.data.frame(trainingsegsmod) %>% 
  dplyr::filter(TrainID2 %in% c("Woodland","Forest","Herbaceous","Shrub")) %>% 
  dplyr::select(Brightness, GLCM_Ang_2,GLCM_Contr,GLCM_Dissi,GLCM_Entro,GLCM_Hom_1, 
                GLCM_Mean_, GLCM_StdDe, Max_pixel, Min_pixel, Mean_Layer,GLCMcont3,
                quantile25,quantile50,quantile75, Skewness_L,TrainID2)

table(trainsegsmod1$TrainID2)

# Converting ‘Survived’ to a factor
trainsegsmod1$TrainID2 <- factor(trainsegsmod1$TrainID2)

# Set a random seed
set.seed(63)

# Training using ‘random forest’ algorithm
RF3<-randomForest(TrainID2 ~ .,data=trainsegsmod1,importance=TRUE,proximity=TRUE)

print(RF3)

#best number of variables 

mtry <- tuneRF(trainsegsmod1[1:16],trainsegsmod1$TrainID, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
#get best number of variables for each tree 

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)


set.seed(52)
RF4<-randomForest(TrainID2 ~ .,data=trainsegsmod1, mtry=best.m,importance=TRUE,proximity=TRUE)

print(RF4)

All_SegsMod<-read_sf(dsn = "C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/GIS Files/Segments/studysegs_14/Grayscale_2014Clip_final.v3.shp")

allsegsmod2<-as.data.frame(All_SegsMod) %>% 
  dplyr::select(Brightness, GLCM_Ang_2,GLCM_Contr,GLCM_Dissi,GLCM_Entro,GLCM_Hom_1, 
                GLCM_Mean_, GLCM_StdDe, Max_pixel, Min_pixel, Mean_Layer,GLCMcont3,
                quantile25,quantile50,quantile75, Skewness_L,UID)


predict2<-as.data.frame(predict(RF4, allsegsmod2))
table(predict2)

#bind predictions to shapefile
allsegsmod2<-cbind(allsegsmod2,predict2)


#export shapefile or as a table? 

#need to assign FID or some unique identifier to join to polygons in Arc 

#write.csv(allsegsmod2,"C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/GIS Files/SegsPredict14.csv", row.names = FALSE)


#This is the winner...need to remember that the Pik value is the OOB accuracy/total proportion
sqrt((.02*.0176-.0176^2)/74 + (.645*.0040-.0040^2)/324+(.32*.0019-.0019^2)/164)


