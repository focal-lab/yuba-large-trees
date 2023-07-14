

# load libraries
library(terra)
library(here)
library(sf)
library(tidyverse)

datadir = readLines(here("data-dir.txt"))

focal_area_path = file.path(datadir, "/spatial/raw/north_yuba_area.kml") # I couldn't get the layer embedded in the GPKG to open properly in R, so I had to use QGIS to export to KML so we can open it in R

# read in raster data
dem = rast(file.path(datadir, "spatial/intermediate/dem.tif"))
sri = rast(file.path(datadir, "spatial/intermediate/sri.tif"))
slope = rast(file.path(datadir, "spatial/intermediate/slope_degrees.tif"))

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


# roads
roads = file.path(datadir, "/spatial/raw/roads_w_core_attr.gpkg")
roads <- st_read(roads)
roads = st_zm(roads)

# roads 
# select required columns
roads <- roads %>% select("SYSTEM", "JURISDICTION", "OPER_MAINT_LEVEL", "OBJECTIVE_MAINT_LEVEL", "ROUTE_STATUS", "SERVICE_LIFE", "geom")
table(roads$OPER_MAINT_LEVEL)

roads = roads |>
  filter((OPER_MAINT_LEVEL %in% c("2 - HIGH CLEARANCE VEHICLES", "3 - SUITABLE FOR PASSENGER CARS", "4 - MODERATE DEGREE OF USER COMFORT", "5 - HIGH DEGREE OF USER COMFORT")) | (JURISDICTION %in% c("C - COUNTY, PARISH, BOROUGH", "SH - STATE HIGHWAY"))) |>
  filter(!(OBJECTIVE_MAINT_LEVEL %in% c("1 - BASIC CUSTODIAL CARE (CLOSED)", "D - DECOMMISSION"))) |>
  filter(!(OPER_MAINT_LEVEL %in% c("0 - NOT MAINTAINED", "1 - BASIC CUSTODIAL CARE (CLOSED)")))

st_write(roads, file.path(datadir, "temp/roads.gpkg"), delete_dsn = TRUE)

road_buf300 = st_buffer(roads, 300) |> st_union()
# st_write(road_buf300, file.path(datadir, "temp/roadbuf.gpkg"), delete_dsn = TRUE)

road_buf100 = st_buffer(roads, 100) |> st_union()

candidate_area = st_difference(road_buf300, road_buf100)
candidate_area = st_simplify(candidate_area, dTolerance = 10)

st_write(candidate_area, file.path(datadir, "temp/candidate_area.gpkg"), delete_dsn = TRUE)

# mask env layers to project boundary
dem_mask <- mask(dem, boundary_vect |> project(crs(dem)))
sri_mask <- mask(sri, boundary_vect |> project(crs(sri)))
slope_mask <- mask(slope, boundary_vect |> project(crs(slope)))


#filter attributes of interest
# keep these
veg_foc <- veg %>%
  dplyr::filter(CWHR_Type %in% c("DFR", "MHC", "PPN", "SMC", "WFR"))

veg_foc = veg_foc |>
  select(BPU, CWHR_Type, Can_Cov, OS_TREE_DIAMETER_CLASS_1)

## find the bad veg polygons
bad_polys = NULL
for(i in 1:nrow(veg_foc)) {
  cat(i, "\n")
  result= try(st_union(veg_foc[i,]))
  if(class(result)[1] == "try-error") {
    bad_polys = c(bad_polys, i)
  }
}

veg_keep = setdiff(1:nrow(veg_foc), bad_polys)

veg_foc = veg_foc[veg_keep,]
veg_foc_union = st_union(veg_foc)
st_write(veg_foc_union, file.path(datadir, "/spatial/raw/veg_existing_condition_focal.gpkg"), delete_dsn = TRUE)

veg_foc_union = st_read(file.path(datadir, "/spatial/raw/veg_existing_condition_focal.gpkg"))


### Intersect road buff with focal veg

foc = st_intersection(veg_foc_union, candidate_area)
st_write(foc, file.path(datadir, "temp/foc.gpkg"), delete_dsn = TRUE)

## define acceptable slope areas: < 60% slope (31 degrees), and nothing with > 60% slope within 120 m

slope_window = focal(slope, w = 5, fun = "max")
slope_acceptable = slope_window < 31
slope_poly = as.polygons(slope_acceptable) |> st_as_sf() |> filter(focal_max == 1) |> st_union()

foc = st_intersection(foc, slope_poly)

## constrain to USFS

usfs = st_read(file.path(datadir, "spatial/raw/S_USA.PADUS_Fee/S_USA.PADUS_Fee.shp")) |> st_transform(st_crs(foc))
usfs = st_intersection(usfs, boundary |> st_transform(st_crs(usfs)))

# buffer in by 100 m
usfs = st_buffer(usfs, -100)
foc = st_intersection(foc, usfs)


### Make grid and mask to focal area

grid = st_make_grid(foc, cellsize = 927, what = "centers")
# grid = st_as_sf(grid)

grid_foc_index = st_intersects(grid, foc, sparse = FALSE)[,1]
grid_foc = grid[grid_foc_index == TRUE]
set.seed(1)
# randomly subsample 120
grid_foc = sample(grid_foc, 120)
grid_foc = st_as_sf(grid_foc)
grid_foc$dummy = "dummy"

st_write(grid_foc, file.path(datadir, "temp/grid_foc.gpkg"), delete_dsn = TRUE)
# st_write(foc, file.path(datadir, "temp/foc.gpkg"), delete_dsn = TRUE)


# elev_extract = extract(dem, vect(grid_foc))[,2]
# grid_foc$elev = elev_extract

# assign IDs
coords = st_coordinates(grid_foc)
grid_foc$easting = coords[,1]
grid_foc$northing = coords[,2]

grid_foc = grid_foc |>
  arrange(easting, northing) |>
  mutate(plot_id = paste0("S", str_pad(1:nrow(grid_foc), side = "left", width = 3, pad = "0"))) |>
  select(plot_id)

st_write(grid_foc, file.path(datadir, "temp/grid_foc.gpkg"), delete_dsn = TRUE)

### Save in requested formats
st_write(grid_foc, file.path(datadir, "selected-plots-for-veg-survey/small-plots_for-iri.kml"), delete_dsn = TRUE)
st_write(grid_foc, file.path(datadir, "selected-plots-for-veg-survey/small-plots_for-iri.shp"), delete_dsn = TRUE)

