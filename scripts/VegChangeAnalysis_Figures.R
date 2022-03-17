## ---------------------------
##
## Script name: Veg Change Figures and ANalysis 
##
## Purpose of script:
##
## Author: Hannah Fertel
##
## Date Created: 2022-08-18
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

####Random Forest Results Figures####
ooberror<-read.csv(file="Data/OOBErrorRF.csv")

#make a side by side table of classification error of the random forest classifier results 

library(gt)
ooberror2<-ooberror %>% 
  mutate("Error Rate(%)"=round(Error*100,2)) %>% 
  dplyr::select(X,Forest,Herbaceous,Woodland,Shrub,Total,"Error Rate(%)") %>%
  rename(Cover=X)
  
  oobtable<-gt(ooberror2) %>%
  tab_row_group(
      label="2014",
      rows=6:10) %>%  
  tab_row_group(
    label="1948",
    rows=1:5) %>% 
    fmt_missing(
      columns = 2:7,
      missing_text = " "
    )

oobtable




#####Confidence interval on proportion of landscape calculation####
veg_prop<-read.csv(file = "Data/ProportionCI_data.csv")
veg_prop$SegsPredict41_ExcelToTable1_Prediction<-veg_prop
veg_prop$OBJECTID..

veg_prop<-rename(veg_prop, Vegetation=SegsPredict41_ExcelToTable1_Prediction)
veg_prop<-rename(veg_prop,Year=OBJECTID..)
veg_prop$Year<-as.factor(veg_prop$Year)

ggplot(data = veg_prop, aes(x=Vegetation, y = Proportion, fill=Year))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Low.CI,ymax=High.CI), width=.1,
                position=position_dodge(.9))

ggsave("Proportionchange.jpeg",plot=last_plot(), device="jpeg", path="Figures/", dpi=300)

proptable
#####veg change raster analysis####
veg_change<-read.csv(file = "Data/Veg_Chang8_18_22.csv")

#will want to make some GGPLOT figures of histograms of the change results 

#idea of histogram with proportions of change?
#Table?
veg_change1<-na.omit(veg_change) 
#maybe want to have another column that's "unchanged" or the "to" category for the chart 
veg_change1$Type_Change_2014<-if_else(veg_change1$Change==0,"Unchanged",veg_change1$To)

veg_change1$Type_Change_2014<-factor(veg_change1$Type_Change_2014,levels=c("Forest","Herbaceous","Shrub","Woodland","Unchanged"))  

#proportion of veg type
ggplot(data=veg_change1, aes(x=From, y=Proportion_group, fill=Type_Change_2014))+
  geom_bar(stat="identity",position="stack")+
  theme_bw()+
  labs(
    y="Proportion",
    x="1948 Predicted Vegetation Type",
    fill="Type Change 2014")+
  scale_fill_brewer(palette="BrBG")
  
ggsave("VegCHange.jpeg",plot=last_plot(), device="jpeg", path="Figures/", dpi=300)

#proportion of total area
ggplot(data=veg_change1, aes(x=From, y=ProportionTot, fill=Type_Change_2014))+
  geom_bar(stat="identity",position="stack")+
  theme_bw()+
  labs(
    y="Proportion of total landscape",
    x="1948 Predicted Vegetation Type",
    fill="Type Change 2014")+
  scale_fill_brewer(palette="BrBG")

ggsave("VegCHange_totallands.jpeg",plot=last_plot(), device="jpeg", path="Figures/", dpi=300)

#