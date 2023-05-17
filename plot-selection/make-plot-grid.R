

# load libraries
library(terra)
library(here)
library(sf)
library(dplyr)

# script
#C:\Users\Korte\Documents\GitHub\north-yuba
# Data
#D:\north-yuba_data\north-yuba_data

# data-dir.txt is currently in C:/GitHub/north-yuba referencing the D: drive
# which holds all the data from the Box
# doesn't work
datadir = readLines(here("data-dir.txt"))


focal_area_path = file.path(datadir, "/spatial/raw/north_yuba_area.kml") # I couldn't get the layer embedded in the GPKG to open properly in R, so I had to use QGIS to export to KML so we can open it in R

# 
# setwd("D:/GIS/Yuba GIS")
# # set baseDir
# baseDir <- "D:/GIS/Yuba GIS"

# read in raster data
dem = rast(file.path(datadir, "spatial/intermediate/dem.tif"))
sri = rast(file.path(datadir, "spatial/intermediate/sri.tif"))
slope = rast(file.path(datadir, "spatial/intermediate/slope_degrees.tif"))
# dem <- rast(paste0(baseDir, "/dem.tif"))
# sri = rast(paste0(baseDir, "/sri.tif"))
# slope = rast(paste0(baseDir, "/slope_degrees.tif"))

# read in vector data
# files in the NorthYuba_DataRequest.gdb exported into shapefiles in ArcMap

# project boundary
boundary <- st_read(focal_area_path)
boundary_transform = st_transform(boundary, 3310)
# change to a vector for sf package
boundary_vect = vect(boundary)
# veg layer
veg_path = file.path(datadir, "/spatial/raw/veg_existing_condition.gpkg")
veg <- st_read(veg_path)

# ^ this layer needed to be exported from the USFS GDB, then converted to Multipolygon using ogr2ogr on the command line. Reference: https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12
# ogr2ogr -f "GPKG" /home/derek/Documents/repo-data-local/north-yuba_data/spatial/raw/veg_existing_condition_fixed.gpkg /home/derek/Documents/repo-data-local/north-yuba_data/spatial/raw/veg_existing_condition.gpkg -nlt "MULTIPOLYGON"


library(gdalUtils)

# roads
roads = file.path(datadir, "/spatial/raw/roads_w_core_attr.gpkg")
roads <- st_read(roads)
roads = st_zm(roads)

road_buf300 = st_buffer(roads, 300) |> st_union()
st_write(road_buf300, file.path(datadir, "temp/roadbuf.gpkg"), delete_dsn = TRUE)

road_buf100 = st_buffer(roads, 100) |> st_union()

candidate_area = st_difference(road_buf300, road_buf100)
st_write(candidate_area, file.path(datadir, "temp/candidate_area.gpkg"), delete_dsn = TRUE)

candidate_area = st_simplify(candidate_area, dTolerance = 10)


# mask to project boundary
dem_mask <- mask(dem, boundary_vect)
plot(dem_mask)
sri_mask <- mask(sri, boundary_vect)
plot(sri_mask)
slope_mask <- mask(slope, boundary_vect)


#filter attributes of interest
# keep these
veg_foc <- veg %>%
  dplyr::filter(CWHR_Type %in% c("DFR", "MHC", "PPN", "SMC", "WFR"))

veg_foc = veg_foc |>
  select(BPU, CWHR_Type, Can_Cov, OS_TREE_DIAMETER_CLASS_1)

veg_foc_union = st_union(veg_foc)
st_write(veg_foc_union, file.path(datadir, "/spatial/raw/veg_existing_condition_focal.gpkg"))


# remove these
rd2 = roads %>% filter(OBJECTIVE_ !="D - DECOMMISSION")
rd1 = roads %>% filter(OBJECTIVE_ !="1 - BASIC CUSTODIAL CARE (CLOSED)")

road_final = bind_rows(rd1, rd2) 



### Make grid and mask to focal area

grid = st_make_grid(boundary_transform, cellsize = 500, what = "centers")
grid = st_as_sf(grid)

grid_foc_index = st_intersects(grid, candidate_area, sparse = FALSE)[,1]
grid_foc = grid[grid_foc_index == TRUE, ]

st_write(grid_foc, file.path(datadir, "temp/grid_foc.gpkg"))


elev_extract = extract(dem, vect(grid_foc))[,2]
grid_foc$elev = elev_extract



#### solar radiation index 


# range of values in sri (32,381 - 135,071)
# what values are considered north and south slopes?
# if filtering watch out for Null values that will be added to the raster




#### dem
# hi, low elevation






#### slope 





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









