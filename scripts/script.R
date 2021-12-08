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


#VTMPlot<-read.table("Data/vtm-plotdata-plot.txt",header=TRUE, sep=" ")
#VTMTrees<-read.delim("Data/vtm-plotdata-trees.txt")
#VTMShrub<-read.delim("Data/vtm-plotdata-brush.txt")

#VTMflatbrush<-read.delim("Data/vtm-plotdata-flatBrush.txt")
#VTMflattrees<-read.delim("Data/vtm-plotdata-flatTrees.txt")
#dropping some values, try with CSV data

VTMPlot<-read.csv("Data/VTMData/VTMPlotData.csv")
VTMTrees<-read.csv("Data/VTMData/vtm-plotdata-trees.csv")
VTMShrub<-read.csv("Data/VTMData/vtm-plotdata-brush..csv")






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

MasterSAplots<-MasterSAplots[!duplicated(MasterSAplots$PLOTKEY), ]

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
  summarise(Tot_stems=sum(TOTAL),diam4_11=sum(DIAM_CLASS_4_11),diam12_23=sum(DIAM_CLASS_12_23),diam24_35=sum(DIAM_CLASS_24_35),diamgr36=sum(DIAM_CLASS_36_))
#now will want to pivot wider with one conifer and one hardwood column with tot. numbers of stems 

SATrees2<-SATrees1 %>% 
  select(Tot_stems,TreeType)

SATrees2<-pivot_wider(SATrees2,
                      names_from = TreeType,
                      values_from= Tot_stems,
                      values_fill = 0)

#can get a TPA value for both types by multiplying by 5 because each plot is .2 acres

#another thing to look into is getting basal area for each type by multiplying average diam by forester's constant value and adding up all square feet by tree count
#then I could theoretically get % cover for identifying veg types.  \]

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
#write.csv(SAShrub,"Data/SAShrub.csv")

#assign new type for if height i.e. if type = shrub and height >4 ft, "tall shrub", otherwise = type category, then change regular shrub to short shrub

SAShrub2$height<-as.numeric(SAShrub2$height)

#if I want to differentiate further by height, can add below lines, but not everything has a height...
#SAShrub2$type2<-ifelse(SAShrub2$Type=="Shrub" & SAShrub2$height > 4.00 | SAShrub2$Type=="Tree/Shrub"  & SAShrub2$height > 4.00 ,"Tall Shrub",SAShrub2$Type ) #assigning tall shrub category for over 4 ft 
#SAShrub2$type2<-ifelse(SAShrub2$type2=="Shrub","Short Shrub",SAShrub2$type2) #renaming other short shrub

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

#want to get list of master plots in study area with data to clip mosaic to

masterSAplotsforGIS<-na.omit(MasterSAplots2)

#use above to filter shapefile of plots I've read in into a new shapefile of only study area plots

vtmpoints_reduced<-vtmpoints %>% 
  dplyr::filter(PLOTKEY %in% masterSAplotsforGIS$PLOTKEY )

# want to write out above as shapefile.  

str(vtmpoints_reduced)

st_write(obj = vtmpoints_reduced,"GIS/SAVTMPlots.shp")


#### veg type classification #####
library(vegclust)

MP4Clust<-MasterSAplots2 %>% 
  select(PLOTKEY,ConiferTPA,HardwoodTPA,`Grass/Herb`,Shrub, Tot_Trees)

####k-means function####
#prove that it was ok to use two different methods 
# Everything without trees looks like it is a shrubland...


MP4Clust1<-MP4Clust %>% 
  column_to_rownames(var="PLOTKEY")

MP4Clust1<-as.data.frame(na.omit(scale(MP4Clust1)))


k1<-kmeans(MP4Clust1, 4, iter.max = 10, nstart = 6)

###try to find optimal number of kmeans clusters
set.seed(1234)

fviz_nbclust(MP4Clust1, kmeans, method = "wss")
fviz_nbclust(MP4Clust1, kmeans, method = "silhouette")
#also looking like 4 is best number by both metrics.  

#ok will run with 4 cluster types then 
k1.1<-kmeans(MP4Clust1, 4, nstart = 10)

K1.1results<-as.data.frame(k1.1$cluster)
names<-rownames(K1.1results)
K1.1results$PLOTKEY<-names

mp4clust1.2<-left_join(MP4Clust,K1.1results, by="PLOTKEY") 


#not sure if it's really getting the forest/woodland/grassland distinction...need to see if I should separate out treed plots, or if there really aren't any grassland plots?
#so far results seem to be: shrub dominated, hardwood dominated, mixed with grassland, mixed with shrub, conifer, hardwood
#could be ok? 


#could try running two separate ones on plots with or without trees? 

#running one for only trees

mp4clusttrees<-MasterSAplots2 %>% 
  filter(Tot_Trees>0) %>% 
  select(PLOTKEY,ConiferTPA,Tot_Trees,HardwoodTPA,Tot_Trees)
  
  
mp4clusttrees1<-mp4clusttrees %>% 
  column_to_rownames(var="PLOTKEY")

k2<-kmeans(mp4clusttrees1, 4, iter.max = 10, nstart = 2)

K2results<-as.data.frame(k2$cluster)
names<-rownames(K2results)
K2results$PLOTKEY<-names

mp4clusttrees2<-left_join(mp4clusttrees,K2results, by="PLOTKEY")

#running one for only grassland/shrubland? 
#no plots with only grassland...

#Cluster package Peeples 2011 i.e. as Collins et al. 2016

####PAM Clustering####
pam.res<-pam(MP4Clust1,4)
pamclustgrp<-cbind(na.omit(MP4Clust),cluster=pam.res$cluster)
#ok not sure what's going on here honestly...

#####hierarchical clustering? ####
#we have a smaller data set, with only a few variables and improved computing power.  
#let's do a test with hclust and diana

library(cluster)
library(stats)
library(factoextra)


# methods to assess:figure out which clustering method is best
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(MP4Clust1, method = x)$ac
}

map_dbl(m, ac)
#ward has best coefficient in clustering structure 

#hclust 
#calculate dissimilarity matrix
d<-dist(MP4Clust1,method="euclidean")
hcl<-hclust(d,method="ward.D2")#run
plot(hcl, cex=.6,hang=-1)#plot

sub_grp<-cutree(hcl,k=4)
table(sub_grp)

hclustgrp<-MP4Clust %>%
  na.omit() %>% 
  mutate(cluster=sub_grp)

#agnes
hca<-agnes(MP4Clust1,method="ward")
pltree(hca,cex=.6, hang=-1, main= "Dendogram of agnes")

#cut tree to group agnes clusters into 3 groups
plot(hca,cex=0.6)
rect.hclust(hca,k=4)

sub_grp<-cutree(hca,k=4)
table(sub_grp)

#look at how it assigned 4 groups
agnesmpclust<-MP4Clust %>%
  na.omit() %>% 
  mutate(cluster=sub_grp)

#Diana (divisive, or top-down clustering)
#I think top-down clustering makes sense given out hunch on small # of groups 

hcd<-diana(MP4Clust1)
hcd$dc
pltree(hcd,cex=.6, hang=-1, main= "Dendogram of diana")

fviz_nbclust(MP4Clust1, FUN = hcut, method = "wss")#looking at optimal number of clusters via elbow method
#look like 3 or 4 clusters is ideal
#sihlouette method
fviz_nbclust(MP4Clust1, FUN = hcut, method = "silhouette")
#also ways optimal number is 4

#cut tree to group diana clusters into 4 groups
plot(hcd,cex=0.6)
rect.hclust(hcd,k=4)

sub_grp<-cutree(hcd,k=4)
table(sub_grp)

agnesmpclust<-MP4Clust %>%
  na.omit() %>% 
  mutate(cluster=sub_grp)
#groups still not seeming super intuitive/ecologically based.  Not sure if that's ok or not? 

                       

#####Texture analysis####
library(glcm)

#specify window to generate texture values
#is this necessary if I'm using ecognition? 