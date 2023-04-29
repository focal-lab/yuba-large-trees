# Purpose: Download DEM for focal area from OpenTopograpy and save
# Also compute slope and save

library(tidyverse)
library(sf)
library(terra)
library(here)
library(elevatr)

# Need to first create an opentopo API key and run this line with the key here. Instructions: https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#Access_OpenTopography_API_with_get_elev_raster()
# set_opentopo_key("key goes here")

# get the root of the local data directory
datadir = readLines("data-dir.txt")

## set up the paths to the files that we will use
# focal area
focal_area_path = file.path(datadir, "/spatial/raw/north_yuba_area.kml") # I couldn't get the layer embedded in the GPKG to open properly in R, so I had to use QGIS to export to KML so we can open it in R

# dem to write
yuba_dem_path = file.path(datadir, "spatial/intermediate/dem.tif")
# slope layer to write
yuba_slope_path = file.path(datadir, "spatial/intermediate/slope_degrees.tif")


## get the overall focal area
focal_area = st_read(focal_area_path)
# transform to CA albers (m coordinates)
focal_area = st_transform(focal_area, 3310)
# buffer it out by 10 km so we can include topographic shading from beyond the focal area
focal_area = st_buffer(focal_area, 10000)

elev = get_elev_raster(focal_area, src = "gl1", clip = "locations", expand = 1000)

writeRaster(elev, yuba_dem_path, overwrite = TRUE)

# In addition, save slope raster
slope <- terrain(elev,v="slope",unit="degrees")
writeRaster(slope, yuba_slope_path, overwrite = TRUE)
