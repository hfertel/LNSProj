## ---------------------------
##
## Script name: VTM Veg Script
##
## Purpose of script: Analysis of Weislander Data 
##
## Author: Hannah Fertel
##
## Date Created: 2021-11-30
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


VTMPlot<-read.delim("Data/vtm-plotdata-plot.txt")
VTMTrees<-read.delim("Data/vtm-plotdata-trees.txt")
VTMShrub<-read.delim("Data/vtm-plotdata-brush.txt")

VTMflatbrush<-read.delim("Data/vtm-plotdata-flatBrush.txt")
VTMflattrees<-read.delim("Data/vtm-plotdata-flatTrees.txt")

#how are folks interpreting this data?? 

#select only plots for each of the above that are within the study area

#create a dataframe of these plots based on arcmap selection
studyareaplots<-read.csv("Data/StudyAreaVTM.csv")

saplotvector<-studyareaplots$PLOTKEY

SAPlots<-VTMPlot %>% 
  dplyr::filter(PLOTKEY %in% saplotvector) %>% 
  as_tibble()
SATrees<-VTMTrees %>% 
  dplyr::filter(PLOTKEY %in% saplotvector)%>% 
  as_tibble()
SAShrub<-VTMShrub%>% 
  dplyr::filter(PLOTKEY %in% saplotvector)%>% 
  as_tibble()
SAflatbrush<-VTMflatbrush%>% 
  dplyr::filter(PLOTKEY %in% saplotvector)%>% 
  as_tibble()
SAflattrees<-VTMflattrees%>% 
  dplyr::filter(PLOTKEY %in% saplotvector)%>% 
  as_tibble()

#figure out which ones are in the data to select on the map as well and see what we're working with 
#could pull into R and select by these 86 plots? 

library(sf)
library(mapview)
library(sf)
library(rgdal)
library(maptools)
library(st)
library(nngeo)
library(sp)
library(rgeos)
vtmpoints<- st_read("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/LNS Veg Project/GIS Files/vtm-plots/vtm-plots.shp")

vtmpoints1<-vtmpoints %>% 
  dplyr::filter(PLOTKEY %in% SAflattrees$PLOTKEY )

plot(vtmpoints1)

mapview(vtmpoints1)

#not all of the plots seem to be in the data...

length( unique(studyareaplots$PLOTKEY))


#Need to code combination dataset of all the tree, shrub, herb data
#relative covers?
#categories are hardwood, conifer, high shrub, low shrub, herb/grass

#only want to look at SAShrub and SATrees and simplify these data to get to the goal dataset.

#I can start with the trees one by categorizing trees as hardwoods or conifers and getting counts for each plot for each?
#what to do about trees <4" dbh (recorded in brush rather than trees)

#master df for plots once I simplify tree and shrub data

MasterSAplots<- SAPlots %>% 
  select(PLOTKEY,SLOPE_PERCENT,ELEVATION)


#find out all the species I have for trees, and categorize by hardwoods vs conifers

SATrees
length( unique(SATrees$genus)) #7genus
unique(SATrees$genus)
# pseudotsuga, pinus, tsuga, sequoia are conifer
#quercus, arbutus, acer are hardwood

SATrees$TreeType<-ifelse(SATrees$genus %in% c("quercus","arbutus","acer"),"Hardwood","Conifer")#creating new column to summarize with

#including different diam classes
SATrees1<-SATrees %>% 
  group_by(PLOTKEY,TreeType) %>% 
  summarise(Tot_stems=sum(TOTAL),diamc1=sum(DIAM_CLASS_4_11),diamc2=sum(DIAM_CLASS_12_23),diamc3=sum(DIAM_CLASS_24_35),diam1=sum(DIAM_CLASS_36_))
#now will want to pivot wider with one conifer and one hardwood column with tot. numbers of stems 

SATrees2<-SATrees1 %>% 
  select(Tot_stems,TreeType)

SATrees2<-pivot_wider(SATrees2,
                      names_from = TreeType,
                      values_from= Tot_stems,
                      values_fill = 0)

#can get a TPA value for both types by multiplying by 5 because each plot is .2 acres

SATrees2<-SATrees2 %>% 
  mutate(ConiferTPA=Conifer*5,HardwoodTPA=Hardwood*5)

#join to master plots

MasterSAplots1<-left_join(MasterSAplots,SATrees2, by="PLOTKEY")

MasterSAplots1[is.na(MasterSAplots1)] <- 0 

MasterSAplots1<-MasterSAplots1 %>% 
  mutate(Tot_Trees=Conifer+Hardwood)

#ok I have tree data in there (preliminary) now for shrubs

# 
length( unique(SAShrub$name)) #56 species
unique(SATrees$genus)
#some percentage of that is barren, grass, litter etc.

#read in CSV of types (without breaking apart tall/short shrubs)
types<-read.csv("Data/ShrubSpecies.csv")

SAShrub2<-left_join(x=SAShrub,y=types,by="name") #got to work by writing out names in csv then copying over
#not working to join
write.csv(SAShrub,"Data/SAShrub.csv")

#assign new type for if height i.e. if type = shrub and height >4 ft, "tall shrub", otherwise = type category, then change regular shrub to short shrub

SAShrub2$height<-as.numeric(SAShrub2$height)

SAShrub2$type2<-ifelse(SAShrub2$Type=="Shrub" & SAShrub2$height > 4.00 | SAShrub2$Type=="Tree/Shrub"  & SAShrub2$height > 4.00 ,"Tall Shrub",SAShrub2$Type ) #assigning tall shrub category for over 4 ft 
SAShrub2$type2<-ifelse(SAShrub2$type2=="Shrub","Short Shrub",SAShrub2$type2) #renaming other short shrub

#might want to keep just shrub because not everything has a height...
SAShrub2$type3<-ifelse(SAShrub2$Type=="Tree/Shrub","Shrub",SAShrub2$Type)
SAShrub2$PERCENT<-as.numeric(SAShrub2$PERCENT)

#pivot wider by type
SAShrub3<-SAShrub2 %>%
  select(PLOTKEY,type3,PERCENT) #only select variables of interest

#group to get total cover by type
SAShrub3<-SAShrub3 %>% 
  group_by(PLOTKEY,type3) %>% 
  summarise(Tot_cov=sum(PERCENT))

  
SAShrub4<-pivot_wider(SAShrub3,
                      names_from = type3,
                      values_from= Tot_cov,
                      values_fill = 0) #got total cover 


MasterSAplots2<-left_join(MasterSAplots1,SAShrub4, by="PLOTKEY")
                       
#####temp info#####
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(raster)
library(stars)

z<-nc_open("WRF_T_SFC.nc")
zz<-raster::raster("WRF_T_SFC2.nc")

print(zz)
zz<-nc_open("WRF_T_SFC2.nc")
zz

#trying in ncdf4
time<-ncvar_get(zz,varid="time")
summary(time)

zz$dim$time$units
# units is "seconds since 1970-01-01 00:00:00"

zz$dim$south_north$units
zz$dim$west_east
zz$var$T_SFC$units
zz$var$T_SFC
zz$var$XLAT



#trying with stack
library(raster)
filename<-"WRF_T_SFC2.nc"
zz1<-brick(filename)
zz1
zz2<-zz1$X2015.01.01.00.00.00

#trying in stars package
t_file=system.file("WRF_T_SFC2.nc", package="stars")
temp=read_ncdf("WRF_T_SFC2.nc", regular = c("west_east","north_south"),ignore_bounds = TRUE)
#creates matrix of values--now we just need to take a slice to get values of interest
#what I'm seeing is the temperature at the first hour 

temp_slice=temp[1,2,1]#created slice that is temperature for one grid cell for every hour of the month of interest 
#values are in x and y, so there ARE lat/long...but how to get to it? 
#could extract grid cells of interest if there was an easy way to identify them...
#reset dimensions to lat long rather than west_east etc. 
temp_slice
#ugh IDK 
vtm<-read.csv("Data/vtm-plotdata-trees.csv")



