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
library(rgdal)
library(tidyverse)

setwd("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/Analysis/LNSProj/")
randompoints<-st_read("GIS/RandomPtsNonForest.shp")
sloperast<-terra::rast("GIS/slope_lns/w001001.adf")
terra:: plot(sloperast)

points_slope<-terra::extract(sloperast,vect(randompoints))
randompoints$slope<-points_slope[,-1]

demrast<-terra::rast("GIS/DEMLNS.tif")
points_elev<-terra::extract(demrast,vect(randompoints))
randompoints$elev<-points_elev[,-1]

aspect<-terrain(demrast, v='aspect',unit='degrees')
points_aspect<-terra::extract(aspect,vect(randompoints))
randompoints$asp<-points_aspect[,-1]

#TPI of points using DEM values above?? 

####CWD and AET DONT NEED TO RUN AGAIN####

#read in each decade of CWD data
#ok want to have a loop/lapply that goes through each file for the decade
#then saves as a raster, then extracts for the points and appends to large 
#alternative to create raster brick, then get average and extract to each point

#need to resample to correct resolution?

dectestrast<-terra::rast("Data/CWDData/cwd1949dec.asc")
crs(dectestrast)
crs(dectestrast)<-"EPSG:3310"
crs(dectestrast)
TAPoints<-st_transform(randompoints,crs="EPSG:3310")
TAPoints
points_cwd<-extract(dectestrast,vect(TAPoints),xy=TRUE)

setwd("Data/CWDData/")
filelist_cwd <- list.files(pattern = "*.asc")

extractpoints= function(
  filename
){
  testraster<-terra::rast(filename)
  print(paste0("workingonfile",filename))
  crs(testraster)<-"EPSG:3310"
  names(testraster)="cwd"
  datestamp=gsub(x=filename,pattern="\\.asc",replacement = "")
  points_cwd<-terra::extract(testraster,vect(TAPoints),xy=TRUE)
points_cwd$datestamp=datestamp
return=points_cwd}

combo_points_cwd<-do.call("rbind",
                          lapply(X=filelist_cwd,FUN = extractpoints))

#combo_pts_cwd5060<-combo_points_cwd
#combo_pts_cwd7080<-combo_points_cwd
#combo_pts_cwd9020<-combo_points_cwd
#combo_pts_cwd201013<-combo_points_cwd

# the files are way to big, I may want to do it off of the external hard drive on Monday 
#alternatively can do 2 decades at a time and write rds after then delete unzipped folders...
#ok saved 50-80s as RDS files, just need to save 90s-2014 also and run above!!
#completed saving all files
#saving the RDS for combining
#saveRDS(combo_pts_cwd201013, file = "combo_pts_cwd201013.rds")

combo_pts_cwd5060<-readRDS("combo_pts_cwd5060.rds")
combo_pts_cwd7080<-readRDS("combo_pts_cwd7080.rds")
combo_pts_cwd9020<-readRDS("combo_pts_cwd9020.rds")
combo_pts_cwd201013<-readRDS("combo_pts_cwd201013.rds")

#combine into mega RDS
allyears_cwd<-rbind(combo_pts_cwd5060,combo_pts_cwd7080,combo_pts_cwd9020,combo_pts_cwd201013)
#save RDS
#saveRDS(allyears_cwd, file = "allyears_cwd.rds")

allyears_cwd<-read_rds("allyears_cwd.rds")

#now just need to get dataset to have year and month, then can manipulate as needed

allyears_cwd<-allyears_cwd %>% 
  separate(datestamp, into =c("cwd","date"),sep="cwd")
testcwd<-head(allyears_cwd)

#testing best way to do this
testcwd<-testcwd %>% 
  separate(datestamp, into =c("remove","date"),sep="cwd") %>% 
  mutate(DateFormat=ym(date)) %>% 
  mutate(Month=month(DateFormat),Year=year(DateFormat))

#Works, great

allyears_cwd2<-allyears_cwd%>% 
  separate(datestamp, into =c("remove","date"),sep="cwd") %>% 
  mutate(DateFormat=ym(date)) %>% 
  mutate(Month=month(DateFormat),Year=year(DateFormat))


#ok have the large dataframe, need to figure out how to summarize into average potentially by point?

#average CWD for each point? any reason you need to group by year first?

averageCWD<-allyears_cwd2 %>% 
  group_by(ID) %>% 
  summarize(Mean=mean(cwd))


#ok just need to join average to original points for analysis


####AET Process *DONT NEED TO RUN AGAIN####


setwd("Data/AETData/")
filelist_aet <- list.files(pattern = "*.asc")
#create TAPoints again
TAPoints<-st_transform(randompoints,crs="EPSG:3310")
TAPoints

extractpoints= function(
  filename
){
  testraster<-terra::rast(filename)
  print(paste0("workingonfile",filename))
  crs(testraster)<-"EPSG:3310"
  names(testraster)="aet"
  datestamp=gsub(x=filename,pattern="\\.asc",replacement = "")
  points_aet<-terra::extract(testraster,vect(TAPoints),xy=TRUE)
  points_aet$datestamp=datestamp
  return=points_aet}

combo_points_aet<-do.call("rbind",
                          lapply(X=filelist_aet,FUN = extractpoints))

#combo_pts_aet5060<-combo_points_aet
#saveRDS(combo_pts_aet5060, file = "combo_pts_aet5060.rds")

#combo_pts_aet7080<-combo_points_aet
#saveRDS(combo_pts_aet7080, file = "combo_pts_aet7080.rds")
#combo_pts_aet9000<-combo_points_aet
#saveRDS(combo_pts_aet9000, file = "combo_pts_aet9000.rds")

#combo_pts_aet1013<-combo_points_aet
#saveRDS(combo_pts_aet1013, file = "combo_pts_aet1013.rds")

#combining AET data 
allyears_aet<-rbind(combo_pts_aet5060,combo_pts_aet7080,combo_pts_aet9000,combo_pts_aet1013)

#rework above code to get simplified all years data with dates for AET data as well 

#allyears_aet<-allyears_aet %>% 
#  separate(datestamp, into =c("aet","date"),sep="aet")
#testcwd<-head(allyears_cwd)

#testing best way to do this
#testcwd<-testcwd %>% 
#  separate(datestamp, into =c("remove","date"),sep="cwd") %>% 
#  mutate(DateFormat=ym(date)) %>% 
#  mutate(Month=month(DateFormat),Year=year(DateFormat))

#Works, great

allyears_aet2<-allyears_aet%>% 
  separate(datestamp, into =c("remove","date"),sep="aet") %>% 
  mutate(DateFormat=ym(date)) %>% 
  mutate(Month=month(DateFormat),Year=year(DateFormat))

#need to delete anything after 2013!

allyears_aet2<-allyears_aet2 %>% 
  filter(Year<2014)

#get average
averageaet<-allyears_aet2 %>% 
  group_by(ID) %>% 
  summarize(Mean=mean(aet))


#####reading in and processing AET and CWD Data####

#reading in CWD data
allyears_cwd<-read_rds("allyears_cwd.rds")

#getting date format
allyears_cwd2<-allyears_cwd%>% 
  separate(datestamp, into =c("remove","date"),sep="cwd") %>% 
  mutate(DateFormat=ym(date)) %>% 
  mutate(Month=month(DateFormat),Year=year(DateFormat))

#getting average value
#remove NAs?
averageCWD<-allyears_cwd2 %>% 
  group_by(ID) %>% 
  summarize(MeanCWD=mean(cwd))

#reading in AET data

combo_pts_aet5060<-read_rds("combo_pts_aet5060.rds")
combo_pts_aet7080<-read_rds("combo_pts_aet7080.rds")
combo_pts_aet9000<-read_rds("combo_pts_aet9000.rds")
combo_pts_aet1013<-read_rds("combo_pts_aet1013.rds")

#combining AET data 
allyears_aet<-rbind(combo_pts_aet5060,combo_pts_aet7080,combo_pts_aet9000,combo_pts_aet1013)

allyears_aet2<-allyears_aet%>% 
  separate(datestamp, into =c("remove","date"),sep="aet") %>% 
  mutate(DateFormat=ym(date)) %>% 
  mutate(Month=month(DateFormat),Year=year(DateFormat))

#need to delete anything after 2013!

allyears_aet2<-allyears_aet2 %>% 
  filter(Year<2014)

#get average
#remove NAs?
averageaet<-allyears_aet2 %>% 
  group_by(ID) %>% 
  summarize(MeanAET=mean(aet))


#ok then, need to join both of above to random points  

randompoints2<-cbind(randompoints,averageaet,averageCWD)


#16 NA points...I guess we'll exclude from analysis






####modeling####

#something like this

randompoints2<-randompoints2 %>% 
  mutate(woody=Tocat)

randompoints2$woody<-ifelse(randompoints2$woody%in% c("Forest","Woodland"),1,0)
#change nulls to 0s for the fire history data
randompoints2$exFire[is.na(randompoints2$exFire)] <- 0
randompoints2$numFires[is.na(randompoints2$numFires)] <- 0
#prepping the data for the modeling

randompoints3<-na.omit(randompoints2)
#testing correlation of variables

checkcorr<-as.data.frame(randompoints3) %>% 
  dplyr::select(slope,elev,asp,MeanCWD,MeanAET)

cor(checkcorr)


#mean AET and Mean CWD are highly correlated--maybe would try to just use 1 of them.

#looking at some histograms of the data



library(lme4)
mod1<-glm(woody~fromCat+scale(slope)+scale(elev)+scale(asp)+scale(MeanAET)+numFires+exFire,data=randompoints2,family="binomial")
summary(mod1)

'
#checking interactions of data

'
#interaction terms in model?
mod2<-glm(woody~fromCat+scale(slope)+scale(elev)+scale(asp)+scale(MeanAET)+fromCat*scale(slope)+fromCat*scale(elev)+fromCat*scale(asp),data=randompoints2,family="binomial")
summary(mod2)


#adding spatial random effects to model? 




#checking residuals
pR2 = 1 - mod1$deviance / mod1$null.deviance


#checking predictions and misclassification of model
randompoints3$predict <- predict(mod1, type = "response")





#####part2:Model of all types of change#####
#ok I want to do a different model with the same process as above looking at possible drivers of change across the landscape
#to do this I will generate 10,000 random points and extract values (2,000 for validation of model) from the rasters
#I will bring in the study area dataset, then generate 12,000 random points, then join from each raster
#I will also need to bring in the Fires dataset to spatial join
#this will require running the above CWD/AET function again and re-downloading all of that data :/  

setwd("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/Analysis/LNSProj/")
#reading in random points w/ minimum 250m distance
#need to add fire data, and veg change data
rp2<- sf::st_read("C:/Users/hmfertel.CAMPUS/Documents/ArcGIS/Projects/LNSProj/LNSProj.gdb", layer = "allvegRandomPts")


sloperast<-terra::rast("GIS/slope_lns/w001001.adf")

points_slope<-terra::extract(sloperast,vect(rp2))
rp2$slope<-points_slope[,-1]

demrast<-terra::rast("GIS/DEMLNS.tif")
points_elev<-terra::extract(demrast,vect(rp2))
rp2$elev<-points_elev[,-1]

aspect<-terrain(demrast, v='aspect',unit='degrees')
points_aspect<-terra::extract(aspect,vect(rp2))
rp2$asp<-points_aspect[,-1]



####CWD and AET for new points--don't need to run again####
#need to transform 
TAPAVoints<-st_transform(rp2,crs="EPSG:3310")
#resample?

#CWD Points
setwd("D:\\BackUpFiles/LNS_Proj/AETCWD/CWD")

filelist_cwd <- list.files(pattern = "*.asc")

extractpoints= function(
  filename
){
  testraster<-terra::rast(filename)
  print(paste0("workingonfile",filename))
  crs(testraster)<-"EPSG:3310"
  names(testraster)="cwd"
  datestamp=gsub(x=filename,pattern="\\.asc",replacement = "")
  points_cwd<-terra::extract(testraster,vect(TAPAVoints),xy=TRUE)
  points_cwd$datestamp=datestamp
  return=points_cwd}

combo_points_cwd<-do.call("rbind",
                          lapply(X=filelist_cwd,FUN = extractpoints))


saveRDS(combo_points_cwd, file = "combo_pts_cwd2ALL.rds")

#AET points
setwd("D:\\BackUpFiles/LNS_Proj/AETCWD/AET")
filelist_aet <- list.files(pattern = "*.asc")


extractpoints= function(
  filename
){
  testraster<-terra::rast(filename)
  print(paste0("workingonfile",filename))
  crs(testraster)<-"EPSG:3310"
  names(testraster)="aet"
  datestamp=gsub(x=filename,pattern="\\.asc",replacement = "")
  points_aet<-terra::extract(testraster,vect(TAPAVoints),xy=TRUE)
  points_aet$datestamp=datestamp
  return=points_aet}

combo_points_aet<-do.call("rbind",
                          lapply(X=filelist_aet,FUN = extractpoints))

saveRDS(combo_points_aet, file = "combo_pts_aet2ALL.rds")


#####reading in new CWD and AET data#####

setwd("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/Analysis/LNSProj/")
allyears_cwd<-read_rds("combo_pts_cwd2ALL.rds")
allyears_aet<-read_rds("combo_pts_aet2ALL.rds")

library(lubridate)

#getting date format
allyears_cwd2<-allyears_cwd%>% 
  separate(datestamp, into =c("remove","date"),sep="cwd") %>% 
  mutate(DateFormat=ym(date)) %>% 
  mutate(Month=month(DateFormat),Year=year(DateFormat))

allyears_aet2<-allyears_aet%>% 
  separate(datestamp, into =c("remove","date"),sep="aet") %>% 
  mutate(DateFormat=ym(date)) %>% 
  mutate(Month=month(DateFormat),Year=year(DateFormat))

#join aet and CWD data

CWDAET<-merge(x=allyears_cwd2,y=allyears_aet2, by=c('ID','date'))

#getting average value
#remove NAs?

CWDAET<-CWDAET %>% 
  filter(Year.x<2014)


averageclim<-CWDAET %>% 
  group_by(ID) %>% 
  summarize(MeanCWD=mean(cwd),MeanAET=mean(aet))


#ok then, need to join both of above to random points, rp2  
#16 NA points...I guess we'll exclude from analysis

rp2.1<-merge(x=rp2,y=averageclim, by='ID')

#remove NAS from climate data
rp2.1<-na.omit(rp2.1)

#if two categories are the same, will be 0 for change, if different, will be a 1
rp3<-rp2.1 %>% 
  mutate(Changed=ifelse(ToCat==FromCat,0,1))


#prepping the data for the modeling:omit NAs 
#slecting only 5,000 values 
rp4<-rp3[1:6500,]

#####function to change appearance of ctree plot below####
#https://luisdva.github.io/rstats/Plotting-conditional-inference-trees-in-R/
# custom barplot function to alter the appearance of terminal nodes
# modified from code provided by Achim Zeileis to the R-help mailing list 
# source the function and assign it as a grapcon_generator object
node_barplot2 <- function(ctreeobj,
                          col = "black",
                          fill = c("blue", "white"),
                          beside = NULL,
                          ymax = NULL,
                          ylines = NULL,
                          widths = 1,
                          gap = NULL,
                          reverse = NULL,
                          id = TRUE)
{
  getMaxPred <- function(x) {
    mp <- max(x$prediction)
    mpl <- ifelse(x$terminal, 0, getMaxPred(x$left))
    mpr <- ifelse(x$terminal, 0, getMaxPred(x$right))
    return(max(c(mp, mpl, mpr)))
  }
  
  y <- response(ctreeobj)[[1]]
  
  if(is.factor(y) || class(y) == "was_ordered") {
    ylevels <- levels(y)
    if(is.null(beside)) beside <- if(length(ylevels) < 3) FALSE else TRUE
    if(is.null(ymax)) ymax <- if(beside) 1.1 else 1
    if(is.null(gap)) gap <- if(beside) 0.1 else 0
  } else {
    if(is.null(beside)) beside <- FALSE
    if(is.null(ymax)) ymax <- getMaxPred(ctreeobj @ tree) * 1.1
    ylevels <- seq(along = ctreeobj @ tree$prediction)
    if(length(ylevels) < 2) ylevels <- ""
    if(is.null(gap)) gap <- 1
  }
  if(is.null(reverse)) reverse <- !beside
  if(is.null(fill)) fill <- gray.colors(length(ylevels))
  if(is.null(ylines)) ylines <- if(beside) c(3, 4) else c(1.5, 2.5)
  
  ### panel function for barplots in nodes
  rval <- function(node) {
    
    ## parameter setup
    pred <- node$prediction
    if(reverse) {
      pred <- rev(pred)
      ylevels <- rev(ylevels)
    }
    np <- length(pred)
    nc <- if(beside) np else 1
    
    fill <- rep(fill, length.out = np)
    widths <- rep(widths, length.out = nc)
    col <- rep(col, length.out = nc)
    ylines <- rep(ylines, length.out = 2)
    
    gap <- gap * sum(widths)
    yscale <- c(0, ymax)
    xscale <- c(0, sum(widths) + (nc+1)*gap)
    
    top_vp <- viewport(layout = grid.layout(nrow = 2, ncol = 3,
                                            widths = unit(c(ylines[1], 1, ylines[2]), c("lines", "null", "lines")),
                                            heights = unit(c(3.5,3.5), c("lines", "null"))),
                       width = unit(1, "npc"),
                       height = unit(1, "npc") - unit(2, "lines"),
                       name = paste("node_barplot", node$nodeID, sep = ""))
    
    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = "white", col = 0))
    
    ## main title
    top <- viewport(layout.pos.col=2, layout.pos.row=1)
    pushViewport(top)
    mainlab <- paste(ifelse(id, paste("Node", node$nodeID,"\n", "(n = "), "n = "),
                     sum(node$weights), ifelse(id, ")", ""), sep = "")
    grid.text(mainlab)
    popViewport()
    
    plot <- viewport(layout.pos.col=2, layout.pos.row=2,
                     xscale=xscale, yscale=yscale,
                     name = paste("node_barplot", node$nodeID, "plot",
                                  sep = ""))
    
    pushViewport(plot)
    
    if(beside) {
      xcenter <- cumsum(widths+gap) - widths/2
      for (i in 1:np) {
        grid.rect(x = xcenter[i], y = 0, height = pred[i],
                  width = widths[i],
                  just = c("center", "bottom"), default.units = "native",
                  gp = gpar(col = col[i], fill = fill[i]))
      }
      if(length(xcenter) > 1) grid.xaxis(at = xcenter, label = FALSE)
      grid.text(ylevels, x = xcenter, y = unit(-1, "lines"),
                default.units = "native",
                just= c("center","bottom"),
                check.overlap = TRUE)
      grid.yaxis()
    } else {
      ycenter <- cumsum(pred) - pred
      
      for (i in 1:np) {
        grid.rect(x = xscale[2]/2, y = ycenter[i], height = min(pred[i], ymax - ycenter[i]),
                  width = widths[1],
                  just = c("center", "bottom"), default.units = "native",
                  gp = gpar(col = col[i], fill = fill[i]))
      }
      
      grid.yaxis(at = round(1 - pred[i], digits = 2), main = FALSE)
    }
    
    grid.rect(gp = gpar(fill = "transparent"))
    upViewport(2)
  }
  
  return(rval)
}
class(node_barplot2) <- "grapcon_generator"

# custom function by user "agstudy"
# draws a white circle with the node name and the number of obs.
innerWeights <- function(node){
  grid.circle(r=0.36,gp = gpar(fill = "White",col="White"))
  mainlab <- paste( node$psplit$variableName, "\n(n = ")
  mainlab <- paste(mainlab, sum(node$weights),")" , sep = "")
  grid.text(mainlab,gp = gpar(col='black'))
}

#####CART Analysis ####
rp4<-rp4 %>% 
  mutate(aspectcat=case_when(asp >= 337.5 ~ 'North',
                             asp >= 292.5 ~ 'Northwest',
                             asp >= 247.5 ~ 'West',
                             asp >= 202.5 ~ 'Southwest',
                             asp >= 157.5 ~ 'South',
                             asp >= 112.5 ~ 'Southeast',
                             asp >= 67.5 ~ 'East',
                             asp >= 22.5 ~ 'Northeast',
                             asp < 22.5 ~ 'North'))
#Bernal et al. 2021 just used 4 aspect cats...
#We converted aspect to a categorical variable with breakpoints at 0°/360°, 90°, 180°, and 270° to correspond to northeast-facing, southeast-facing, southwest-facing, and northwest-facing slopes, respectively.

rp4<-rp4 %>% 
  mutate(aspectcat2=case_when(asp <= 90 ~ 'Northeast',
                              asp <= 180 ~ 'Southeast',
                              asp <= 270 ~ 'Southwest',
                              asp < 360 ~ 'Northwest'))
                             



rp4$aspectcat=as.factor(rp4$aspectcat)
rp4$aspectcat2=as.factor(rp4$aspectcat2)
rp4$Burned=as.factor(rp4$ExFire)
rp4$Changedfact=as.factor(rp4$Changed)
#Look at CART analysis for 2 sub-groups of different points
#1) is points that were forest in 1948
#2) points that were not forest in 1948
#run analysis to see what led to changes to or from forest type 

rp4F<-rp4 %>% 
  filter(FromCat=="Forest")
rp4NF<-rp4 %>% 
  filter(FromCat!="Forest")

#was forest, either remained or changed--what variables were important?
#could also add variable of what it was identifed as in 2014, to show what it was lost to?

library(rpart)
library(party)
library(partykit)

##Party Trees!##

#Analysis 1: was forest#
tree1<-ctree(Changedfact~slope+aspectcat+elev+NumFires+MeanAET, data=rp4F)
plot(tree1)
#Binary burned or didn't
treeB<-ctree(Changedfact~slope+aspectcat+elev+Burned+MeanAET, data=rp4F, controls= ctree_control (testtype = c("Bonferroni"), mincriterion = 0.95))
plot(treeB)
#looking at just the 4 categories of aspect
treeB2<-ctree(Changedfact~slope+aspectcat2+elev+Burned+MeanAET, data=rp4F, controls= ctree_control (testtype = c("Bonferroni"), mincriterion = 0.95))
plot(treeB2)
#plot sourced from blog to make prettier ctree plot
plot (treeB2,inner_panel=innerWeights,
      terminal_panel=node_barplot2,
      tp_args = list(ylines = c(2, 4))) # this arg. modifies the spacing between barplots

#add time since fire??

##Analysis 2:was not forest, became forest or didn't#

#get binary variable for became forest or not
rp4NF<-rp4NF %>% 
  mutate(encroach=as.factor(ifelse(ToCat %in% "Forest",1,0)))
#need to assign ordinal value to from cat
rp4NF<-rp4NF %>% 
  mutate(code=as.factor(FromCat))

#want to convert aspect to a categorical variable

tree2<-ctree(encroach~code+aspectcat+elev+NumFires+MeanAET, data=rp4NF)
plot(tree2)  
tree2B<-ctree(encroach~code+aspectcat+elev+Burned+MeanAET, data=rp4NF, controls= ctree_control (testtype = c("Bonferroni"), mincriterion = 0.95))
plot(tree2B)
tree2B2<-ctree(encroach~code+aspectcat2+elev+Burned+MeanAET, data=rp4NF, controls= ctree_control (testtype = c("Bonferroni"), mincriterion = 0.95))
plot(tree2B2, type="simple") 
plot(tree2B2)
ggsave("EncroachTree.jpeg",plot=last_plot(), device="jpeg", path="Figures/", dpi=300)

table(predict(tree2B), rp4NF$encroach)



tree3<-ctree(Changedfact~as.factor(FromCat)+slope+aspectcat+elev+Burned+MeanAET, data=rp4)
plot(party3)


#how to interpret/evaluate this?
#transform aspect into categories

#plot sourced from blog to make prettier ctree plot
plot (tree2B2,inner_panel=innerWeights,
      terminal_panel=node_barplot2,
      tp_args = list(ylines = c(2, 4))) # this arg. modifies the spacing between barplots

##Random Forest

library(randomForest)
fit1 <- randomForest(Changed~slope+asp+elev+NumFires+ExFire, data=rp4F)
print(fit1) # view results
importance(fit1) # importance of each predictor

fit2 <- randomForest(encroach~slope+asp+elev+NumFires+ExFire, data=rp4NF)
print(fit2) # view results
importance(fit2)



##RPART

library(rpart)

party1<-rpart(Changed~slope+aspectcat+elev+NumFires+MeanAET, data=rp4F,method="class",control=rpart.control(cp=.01))#default is anova 

party1A<-rpart(Changed~slope+aspectcat+elev+NumFires+MeanAET, data=rp4F,control=rpart.control(cp=.02))#with ANOVA(since we're looking at class change, I don't think we need this)

#can change complexity parameter;what do you think is biologically significant 
#can cut off where it gets too complicated
#if too simple, can go back down with cp 
plot(party1) #generates blank tree
text(party1)
library(rpart.plot)
rpart.plot(party1)
rpart.plot(party1A)
#same but just experienced fire rather than number of fires
party1.2<-rpart(Changed~slope+aspectcat+elev+Burned+MeanAET, data=rp4F,method="class",control=rpart.control(cp=.02))
rpart.plot(party1.2)
party1.2A<-rpart(Changed~slope+aspectcat+elev+Burned+MeanAET, data=rp4F,control=rpart.control(cp=.02))
rpart.plot(party1.2A)
plotcp(party1)
printcp(party1)

#party2<-rpart(encroach~FromCat+slope+aspectcat+elev+NumFires+MeanAET, data=rp4NF,method="class")
party2<-rpart(encroach~FromCat+slope+aspectcat+elev+NumFires+MeanAET, data=rp4NF,method="class",control=rpart.control(cp=.02))
rpart.plot(party2)
party2B<-rpart(encroach~FromCat+slope+aspectcat+elev+Burned+MeanAET, data=rp4NF,method="class",control=rpart.control(cp=.02))
rpart.plot(party2B)
party2B2<-rpart(encroach~FromCat+slope+aspectcat2+elev+Burned+MeanAET, data=rp4NF,method="class",control=rpart.control(cp=.01))
rpart.plot(party2B2)
party2BA<-rpart(encroach~FromCat+slope+aspectcat+elev+Burned+MeanAET, data=rp4NF,control=rpart.control(cp=.02))
rpart.plot(party2BA)

#general trends are what's important/what to focus on in discussion 

#look at all of the points and what changed at all with class
party3<-rpart(Changedfact~FromCat+slope+aspectcat+elev+Burned+MeanAET, data=rp4)
rpart.plot(party3)
#not super interesting....


#might want to look again at specifically points that were not forest

#####logistic model of change part2####
#alternative model to see where there was change--what contributed to change and directionality of change??
#testing correlation of variables

checkcorr<-as.data.frame(rp4) %>% 
  dplyr::select(slope,elev,asp,MeanCWD,MeanAET)

cor(checkcorr)

#mean AET and Mean CWD are highly correlated--maybe would try to just use 1 of them.

#looking at some histograms of the data

#Model just looking at what might lead to change?
library(lme4)
mod1<-glm(Changed~FromCat+scale(slope)+scale(elev)+scale(asp)+NumFires+ExFire,data=rp4,family="binomial")
summary(mod1)

#model looking at what was forest, then how what might have contributed to it changing or not
mod1F<-glm(Changed~scale(slope)+scale(elev)+scale(asp)+NumFires+ExFire,data=rp4F,family="binomial")


#model looking at was not forest, and if it became forest, what contributed to that?
mod1NF<-glm(encroach~FromCat+scale(slope)+scale(elev)+scale(asp)+NumFires+ExFire,data=rp4NF,family="binomial")
summary(mod1NF)


# bayesian model?
bmodel1<-brm(data=rp4,
             family=binomial,
             Changed~scale(slope)+scale(elev)+scale(asp)+NumFires+ExFire+(1|FromCat))



#checking interactions of data
#adding spatial random effects to model? 

#checking residuals
pR2 = 1 - mod1$deviance / mod1$null.deviance


#checking predictions and misclassification of model
rp4$predict <- predict(mod1, type = "response")




#####Misc Stuff####
cwd_rasters <- rast(filelist_temp)
#clip raster to study area, otherwise it's too big?
studyarea<-st_read("GIS/studyarea.shp")

plot(cwd_rasters) #no crs apparently
plot(studyarea)
#set crs for raster stack
crs(cwd_rasters)<-"+proj=utm +zone=10N"
plot(cwd_rasters)
#myextent<-readOGR("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/Analysis/LNSProj/GIS/","studyarea.shp")
#myextent<-spTRansform(myExtent, CRS(proj4string(cwd_rasters)))
cropcwd<-crop(cwd_rasters,ext(studyarea))
avecwd<-mean(cwd_raste)