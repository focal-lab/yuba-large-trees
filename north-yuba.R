

# load libraries
library(terra)
library(here)
library(sf)
library(dplyr)

# data-dir.txt is currently in C:/GitHub/north-yuba referencing the D: drive
# which holds all the data from the Box
# doesn't work
datadir = readLines("data-dir.txt")


setwd("D:/GIS/Yuba GIS")
# set baseDir
baseDir <- "D:/GIS/Yuba GIS"

# read in raster data
dem <- rast(paste0(baseDir, "/dem.tif"))
sri = rast(paste0(baseDir, "/sri.tif"))


# read in vector data
# files in the NorthYuba_DataRequest.gdb exported into shapefiles in ArcMap

# project boundary
boundary <- st_read(paste0(baseDir, "/NYLR_PRojectBoundary.shp"))
# change to a vector for sf package
boundary_vect = vect(boundary)
# veg layer
veg <- st_read(paste0(baseDir, "/NYLR_ExistingCondition_BaseProjectData.shp"))
# roads
roads <- st_read(paste0(baseDir, "/NYLR_RoadsWithCoreAttributes.shp"))

# doesn't work
#elev_int <- sf::st_intersection(elev, boundary)

# mask to project boundary
dem_mask <- mask(dem, boundary_vect)
plot(elev_mask)
sri_mask <- mask(sri, boundary_vect)
plot(sri_mask)

#filter attributes of interest
# keep these
dfr <- veg %>%
  dplyr::filter(CWHR_Type == "DFR")
mhc <- veg %>%
  dplyr::filter(CWHR_Type == "MHC")
ppn <- veg %>%
  dplyr::filter(CWHR_Type == "PPN")
smc <- veg %>%
  dplyr::filter(CWHR_Type == "SMC")
wfr <- veg %>%
  dplyr::filter(CWHR_Type == "WFR")

veg_final = bind_rows(dfr, mhc, ppn, smc, wfr) 
st_write(veg_final, "veg_final.shp")


# remove these
rd2 = roads %>% filter(OBJECTIVE_ !="D - DECOMMISSION")
rd1 = roads %>% filter(OBJECTIVE_ !="1 - BASIC CUSTODIAL CARE (CLOSED)")

road_final = bind_rows(rd1, rd2) 


#### solar radiation index 


# range of values in sri (32,381 - 135,071)
# what values are considered north and south slopes?
# if filtering watch out for Null values that will be added to the raster




#### dem
# hi, low elevation








#### roads buffer; 100 m min dist, between 100-400m from road

# need to drop the the M geometry first for the buffer
rd_final_noM <- st_zm(rd_final, drop = TRUE, what = "ZM")

# needs corrected
rd_buf <- st_buffer(rd_final_noM, dist = 400)
plot(rd_buf)

# combine the polygons
rd_union <- st_union(rd_buf)
plot(rd_union)



#### extract data

# extract buffer area from dem, sri and veg layers
# 




####### combine/merge the layers






#### add 150 random pts
## watch out for filtered raster's and Null values within our points






# graph data extracted from road buffer
# what is the attribute to graph? How does one see the attributes in a raster?

library(ggplot2)

ggplot(dem, aes(x=Char_group)) + geom_histogram(binwidth=0.5) 
ggplot(rp, aes(x=Elev)) + geom_histogram(1) 
ggplot(rp, aes(x=Char_group)) + geom_histogram(binwidth=0.5) 









