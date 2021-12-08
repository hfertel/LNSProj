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



