
# plot selection criteria 

# load libraries
library(terra)
library(here)
library(sf)
library(dplyr)


datadir = readLines(here("data-dir.txt"))


# read in raster data
dem = rast(file.path(datadir, "spatial/intermediate/dem.tif"))
sri = rast(file.path(datadir, "spatial/intermediate/sri.tif"))
slope = rast(file.path(datadir, "spatial/intermediate/slope_degrees.tif"))

# veg data
veg_path = file.path(datadir, "/spatial/raw/veg_existing_condition_fixed.gpkg")
veg <- st_read(veg_path)


# project boundary
focal_area_path = file.path(datadir, "/spatial/raw/north_yuba_area.kml") 
foca_area <- st_read(focal_area_path)
focal_area_transform = st_transform(focal_area_path, 3310)

# roads
roads = file.path(datadir, "/spatial/raw/roadswithcoreattributes.gpkg")
roads <- st_read(roads)
# removal m geometry required
roads = st_zm(roads)

# select habitats to keep
veg_foc <- veg %>%
  dplyr::filter(CWHR_Type %in% c("DFR", "MHC", "PPN", "SMC", "WFR"))
# select attributes of interest to keep
veg_foc = veg_foc %>%
  select(BPU, CWHR_Type, Can_Cov, OS_TREE_DIAMETER_CLASS_1)

# roads 
# select required columns
roads_select <- roads %>% select("SYSTEM", "OBJECTIVE_MAINT_LEVEL", "ROUTE_STATUS", "SERVICE_LIFE", "geom")
roads_select$OBJECTIVE_MAINT_LEVEL <- as.character(roads_select$OBJECTIVE_MAINT_LEVEL)
# replacing the NA's with text to avoid R deleting rows with any NA's 
roads_select$OBJECTIVE_MAINT_LEVEL[is.na(roads_select$OBJECTIVE_MAINT_LEVEL)] <- "Not Found"

roads_noD = roads_select %>% filter(OBJECTIVE_MAINT_LEVEL !="D - DECOMMISSION")
roads_noD_no_1 = roads_noD %>% filter(OBJECTIVE_MAINT_LEVEL !="1 - BASIC CUSTODIAL CARE (CLOSED)")
# a few more that need to be removed
roads_noDno1_final = roads_noD_no_1 %>% filter(ROUTE_STATUS !="DE - DECOMMISSIONED")

# add buffer to roads; 100m and 300m 
road_buf300 = st_buffer(roads_noDno1_final, 300) |> st_union()
road_buf100 = st_buffer(roads_noDno1_final, 100) |> st_union()

# subtract for our candidate area
candidate_area = st_difference(road_buf300, road_buf100)
# reduce complexity to expedite loading
candidate_area = st_simplify(candidate_area, dTolerance = 10)

# combine vegetation and roads
# clip of set of overlapping features: intersection
veg_roads_intersection = st_intersection(veg_foc, candidate_area, sparse = FALSE)[,1]

# dissolve numerous polygons into one
veg_roads_union = st_union(veg_roads_intersection)


### Make grid, points 200m apart

grid = st_make_grid(boundary_transform, cellsize = 200, what = "centers")
# converting the grid from spatial object to a data frame for manipulation
grid = st_as_sf(grid)

grid_foc_index = st_intersects(grid, candidate_area, sparse = FALSE)[,1]
grid_foc = grid[grid_foc_index == TRUE, ]

# generate 150 random points within 200m grid
random_points_200 <- st_sample(grid_foc, 150, type = "random")


# pair the variables with the points in the grid
elev_extract = extract(dem, vect(grid_foc))[,2]
grid_foc$elev = elev_extract

slope_extract = extract(slope, vect(grid_foc))[,2]
grid_foc$slope = slope_extract

sri_extract = extract(sri, vect(grid_foc))[,2]
grid_foc$sri = sri_extract


library(ggplot2)
ggplot(grid_foc_200, aes(x = elev)) + geom_histogram(binwidth=100, colour="white", fill="blue") +
  labs(x ='Elevation', y='Count', title = 'Grid = 200m') 
ggplot(grid_foc_200, aes(x = slope)) + geom_histogram(binwidth=10, colour="white", fill="blue") +
  labs(x ='Slope (deg)', y='Count', title = 'Grid = 200m') 
ggplot(grid_foc_200, aes(x = sri)) + geom_histogram(binwidth=75000, colour="white", fill="blue") + 
  labs(x ='SRI', y='Count', title = 'Grid = 200m')




# grid 1000m apart

grid2 = st_make_grid(focal_area_transform, cellsize = 1000, what = "centers")
grid2 = st_as_sf(grid2)

grid2_foc_index = st_intersects(grid2, veg_roads_union, sparse = FALSE)[,1]
grid2_foc = grid2[grid2_foc_index == TRUE, ]

# generate 150 random points within 1000m grid
random_points_1000 <- st_sample(grid2_foc, 150, type = "random")

# extract data at points at add into grid_foc dataframe 
elev_extract = extract(dem, vect(grid2_foc))[,2]
grid2_foc$elev = elev_extract

slope_extract = extract(slope, vect(grid2_foc))[,2]
grid2_foc$slope = slope_extract

sri_extract = extract(sri, vect(grid2_foc))[,2]
grid2_foc$sri = sri_extract


ggplot(grid2_foc, aes(x = elev)) + geom_histogram(binwidth=100, colour="white", fill="blue") +
  labs(x ='Elevation', y='Count', title = 'Grid = 1000m') 

ggplot(grid2_foc, aes(x = slope)) + geom_histogram(binwidth=3, colour="white", fill="blue") +
  labs(x ='Slope (deg)', y='Count', title = 'Grid = 1000m') 

ggplot(grid2_foc, aes(x = sri)) + geom_histogram(binwidth=1000, colour="white", fill="blue") + 
  labs(x ='SRI', y='Count', title = 'Grid = 1000m') 


## compare to a grid of the entire project area

### not constrained by roads
grid_all = st_make_grid(focal_area_transform, cellsize = 500, what = "centers")
grid_all = st_as_sf(grid_all)

elev_extract = extract(dem, vect(grid_all))[,2]
grid_all$elev = elev_extract

slope_extract = extract(slope, vect(grid_all))[,2]
grid_all$slope = slope_extract

sri_extract = extract(sri, vect(grid_all))[,2]
grid_all$sri = sri_extract


ggplot(grid_all, aes(x = elev)) + geom_histogram(binwidth=100, colour="white", fill="blue") +
  labs(x ='Elevation', y='Count', title = 'All (n=62,055)') 
ggplot(grid_all, aes(x = slope)) + geom_histogram(binwidth=3, colour="white", fill="blue") +
  labs(x ='Slope (deg)', y='Count', title = 'All (n=62,055)') 
ggplot(grid_all, aes(x = sri)) + geom_histogram(binwidth=1000, colour="white", fill="blue") + 
  labs(x ='SRI', y='Count', title = 'All (n=62,055)') 



