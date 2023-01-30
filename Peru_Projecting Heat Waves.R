
#For the assignemnt:
library(raster)  # you also may use the terra package, which is newer version of the raster package
library(sf)
library(tmap)
library(tidyverse)

# stack the urban rasters, stack the heat rasters
# run this script once (current, 2040, 2070, 2100)

#read in data
heat <- stack(map(list.files("Temp", full.names = TRUE), raster))
urban <- stack(map(list.files("Urban", full.names = TRUE), raster))

# heat <- raster("Temperature/Global_HeatIndex_GFDL-ESM2M=ESM2Mlsdkjvdslkv.tif")
# urbanarea <- raster("datalifoisajf.tiff")

popMean <- raster("Population/popmean-2010.tiff")

#read in countries
countries <- st_read("Nations/ne_10m_admin_0_countries_updated_2.shp")
countries


# peru <- countries %>% filter(ADMIN=="Peru"), colors is mean population and x-axis is longitude and y-axis is latitude
peru <- countries[countries$subunit == "Peru", ]

plot(peru)
st_bbox(peru)
peruExtent <- extent(peru %>% as("Spatial"))

##crop showing heat for each year from data frames 1 through 4

heatCrop <- mask(crop(heat, peruExtent), peru)
plot(heatCrop)

popMeanCrop <- mask(crop(popMean, peruExtent), peru)
plot(popMeanCrop)

urbanCrop <- mask(crop(urban, peruExtent), peru)
plot(urbanCrop) #green are the cities, urban is 604 and rural 1


#rasters <- vector vector of rasters
#maps raster, crop, peruextent
##values

# rastlist <- list(heat, popMean, urbanArea)
# croplist %>% purrr::map(~crop(.,peruExtent))
# plot(cropList[[3]])
# plot(rastList[[1]])
# 
# getValues(heatCrop)
# getValues(popMeanCrop)
# 
# heatCrop
# popMeanCrop


##resample
heatCropResample <- resample(heatCrop, popMeanCrop)

heatCropResample
popMeanCrop


tm_shape(raster(heatCropResample, 2)) +tm_raster(n=10, palette= "Blues") +
  tm_shape(peru) + tm_borders() + tm_legend(outside = TRUE) #1 is current year 2 is for 2040 3 is 2070 and 4 is 2100
#showing the heat index from 5 to 55

# reclassify data where we base our policy
heatRast <- heatCropResample
minHeat <- c(values(heatRast) %>% min(na.rm=TRUE)-1) #tell R how to create the intervals
maxHeat <- values(heatRast) %>% max(na.rm=TRUE) + 1
m <- c(minHeat, 40, 0, 40, maxHeat, 1) #min heat, 40, 1, 40, maxheat, 0 for who's not affected by heat waves. Needs to be reclassified to 0 to visualize who's not affected

rclMat <- matrix(m, ncol = 3, byrow = TRUE) #created maxtrix see the lowest and highest temp, and reclassify
heatRC <- reclassify(heatRast, rclMat)
plot(heatRC)

# heatRast[heatRast < 42] <- 0
# heatRast[heatRast >= 42] <- 1
# plot(heatRast)

###map algebra, we see how many people will be affected

heatPop <- heatRC * popMeanCrop #population multiplied by whether there will be a heat wave or not
heatPop
plot(popMeanCrop)
plot(heatRC)
# sum(values(heatPop), na.rm=TRUE) #number of people in Peru going to be affected by heatwave events

# total population (urban and rural) *affected* by heat waves in current year, 2040, 2070 or 2100
cellStats(heatPop, sum) #total population

# reclassify pixels in the urban raster
urbanCropRC <- reclassify(urbanCrop, cbind(id=c(0,1,604), v=c(0,0,1)))
plot(urbanCropRC) #creating Boolean map for urban areas, 1=urban area 0=rural or water

# get urban population affected by heat waves in current year, 2040, 2070 or 2100
heatUrban <- heatRC * urbanCropRC * popMeanCrop
plot(heatUrban)
cellStats(heatUrban, sum)

# rural population affected by heat waves
cellStats(heatPop, sum) - cellStats(heatUrban, sum) #rural areas will be the most affected, this may be to an issue with the urbanization map

#calculate the non-affected, need to edit line 94
#Replace line 75 with 30 to find out amount of population (both urban and rural) that will be affected by a heatwave of 30+ C