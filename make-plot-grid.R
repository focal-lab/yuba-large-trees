

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

# veg layer
veg_path = file.path(datadir, "/spatial/raw/veg_existing_condition.kml")
veg <- st_read(veg_path)
veg_transform = st_transform(veg, 3310)

# project boundary
focal_area_path = file.path(datadir, "/spatial/raw/north_yuba_area.kml") 
foca_area <- st_read(focal_area_path)
focal_area_transform = st_transform(focal_area_path, 3310)

# roads
roads = file.path(datadir, "/spatial/raw/roadswithcoreattributes.gpkg")
roads <- st_read(roads)
# remove z and m geometries here
roads = st_zm(roads)
# remove these
roads = roads %>% filter(OBJECTIVE_ !="D - DECOMMISSION")
roads = roads %>% filter(OBJECTIVE_ !="1 - BASIC CUSTODIAL CARE (CLOSED)")

# add buffer to roads; 100m and 300m 
road_buf300 = st_buffer(roads, 300) |> st_union()
road_buf100 = st_buffer(roads, 100) |> st_union()

# subtract the two for our candidate area for plots
candidate_area = st_difference(road_buf300, road_buf100)
# not sure what this does
candidate_area = st_simplify(candidate_area, dTolerance = 10)


# filter attributes of interest
# keep these
# not working
veg_foc <- veg %>%
  dplyr::filter(CWHR_Type %in% c("DFR", "MHC", "PPN", "SMC", "WFR"))

veg_foc = veg_foc |>
  select(BPU, CWHR_Type, Can_Cov, OS_TREE_DIAMETER_CLASS_1)

veg_foc_union = st_union(veg_foc)
                
st_write(veg_foc, file.path(datadir, "temp/veg_final.gpkg"), delete_dsn = TRUE)

### Make grid, 200m apart

grid = st_make_grid(boundary_transform, cellsize = 200, what = "centers")
grid = st_as_sf(grid)

grid_foc_index = st_intersects(grid, candidate_area, sparse = FALSE)[,1]
grid_foc = grid[grid_foc_index == TRUE, ]

# extract and pair the grid point data from the rasters to the grid_foc data frame
elev_extract = extract(dem, vect(grid_foc))[,2]
grid_foc$elev = elev_extract

slope_extract = extract(slope, vect(grid_foc))[,2]
grid_foc$slope = slope_extract

sri_extract = extract(sri, vect(grid_foc))[,2]
grid_foc$sri = sri_extract

ggplot(grid_foc_200, aes(x = elev)) + geom_histogram(binwidth=100, colour="white", fill="blue") +
  labs(x ='Elevation', y='Count', title = 'Grid = 200m') 
ggplot(grid_foc_200, aes(x = slope)) + geom_histogram(binwidth=10, colour="white", fill="blue") +
  labs(x ='Slope (deg)', y='Count', title = 'Grid = 200m') 
ggplot(grid_foc_200, aes(x = sri)) + geom_histogram(binwidth=75000, colour="white", fill="blue") + 
  labs(x ='SRI', y='Count', title = 'Grid = 200m')

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





