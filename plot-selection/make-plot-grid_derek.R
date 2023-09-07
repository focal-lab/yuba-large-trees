

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

#st_write(roads, file.path(datadir, "temp/roads.gpkg"), delete_dsn = TRUE)

road_buf300 = st_buffer(roads, 300) |> st_union()
# st_write(road_buf300, file.path(datadir, "temp/roadbuf.gpkg"), delete_dsn = TRUE)

road_buf100 = st_buffer(roads, 100) |> st_union()

candidate_area = st_difference(road_buf300, road_buf100)
road_candidate_area = st_simplify(candidate_area, dTolerance = 10)

#st_write(candidate_area, file.path(datadir, "temp/candidate_area.gpkg"), delete_dsn = TRUE)

# mask env layers to project boundary
dem_mask <- mask(dem, boundary_vect |> project(crs(dem)))
sri_mask <- mask(sri, boundary_vect |> project(crs(sri)))
slope_mask <- mask(slope, boundary_vect |> project(crs(slope)))
# 
# 
# #filter attributes of interest
# # keep these
# veg_foc <- veg %>%
#   dplyr::filter(CWHR_Type %in% c("DFR", "MHC", "PPN", "SMC", "WFR"))
# 
# veg_foc = veg_foc |>
#   select(BPU, CWHR_Type, Can_Cov, OS_TREE_DIAMETER_CLASS_1)
# 
# ## find the bad veg polygons
# bad_polys = NULL
# for(i in 1:nrow(veg_foc)) {
#   cat(i, "\n")
#   result= try(st_union(veg_foc[i,]))
#   if(class(result)[1] == "try-error") {
#     bad_polys = c(bad_polys, i)
#   }
# }
# 
# veg_keep = setdiff(1:nrow(veg_foc), bad_polys)
# 
# veg_foc = veg_foc[veg_keep,]
# st_write(veg_foc, file.path(datadir, "/spatial/raw/veg_existing_condition_focal.gpkg"), delete_dsn = TRUE)
# 
# veg_foc_union = st_union(veg_foc)
# st_write(veg_foc_union, file.path(datadir, "/spatial/raw/veg_existing_condition_focal_union.gpkg"), delete_dsn = TRUE)

veg_foc_union = st_read(file.path(datadir, "/spatial/raw/veg_existing_condition_focal_union.gpkg"))
veg_foc = st_read(file.path(datadir, "/spatial/raw/veg_existing_condition_focal.gpkg"))

veg_dfwfpp = veg_foc[veg_foc$CWHR_Type %in% c("PPN", "DFR", "WFR"), ]
veg_dfwfpp_union = st_union(veg_dfwfpp)
st_write(veg_dfwfpp_union, file.path(datadir, "/spatial/raw/veg_existing_condition_dfwfpp_union.gpkg"), delete_dsn = TRUE)

veg_pp = veg_foc[veg_foc$CWHR_Type %in% c("PPN"), ]
veg_pp_union = st_union(veg_pp)
st_write(veg_pp_union, file.path(datadir, "/spatial/raw/veg_existing_condition_pp_union.gpkg"), delete_dsn = TRUE)


## define acceptable slope areas: < 60% slope (31 degrees), and nothing with > 60% slope within 120 m

slope_window = focal(slope, w = 5, fun = "max")
slope_acceptable = slope_window < 31
slope_poly = as.polygons(slope_acceptable) |> st_as_sf() |> filter(focal_max == 1) |> st_union()



## constrain to USFS

usfs = st_read(file.path(datadir, "spatial/raw/S_USA.PADUS_Fee/S_USA.PADUS_Fee.shp")) |> st_transform(st_crs(veg_foc_union))
usfs = st_intersection(usfs, boundary |> st_transform(st_crs(usfs)))

# buffer in by 100 m
usfs_buffin = st_buffer(usfs, -100)




### Get focal area based on intersection of all the prepped layers
foc = st_intersection(veg_foc_union, slope_poly) # veg and slope

# for not using the roads criterion and not using FS neg buffer
foc_general = st_intersection(foc, usfs) # forest service

# for the road buffer, and 100 m buffer from usfs boundary
foc = st_intersection(foc, usfs_buffin) # forest service
foc = st_intersection(foc, road_candidate_area)

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
st_write(grid_foc, file.path(datadir, "selected-plots-for-veg-survey/small-plots_for-iri_v!!!.kml"), delete_dsn = TRUE)
st_write(grid_foc, file.path(datadir, "selected-plots-for-veg-survey/small-plots_for-iri_v!!!.shp"), delete_dsn = TRUE)





#### Check how well it covers env space ####
plots = st_read(file.path(datadir, "selected-plots-for-veg-survey/small-plots_for-iri.kml"))

# get veg type
plots = st_intersection(plots |> st_transform(st_crs(veg_foc)), veg_foc)

# get elev
el = terra::extract(dem_mask, plots)[,2]
plots$elev = el

plots$type = "road constrained"

hist(plots$elev)
table(plots$CWHR_Type)



### try it with a grid of pts not constrained to road or fs buffer

grid = st_make_grid(foc_general, cellsize = 500, what = "centers")
# grid = st_as_sf(grid)

grid_foc_index = st_intersects(grid, foc_general, sparse = FALSE)[,1]
grid_foc_general = grid[grid_foc_index == TRUE]
set.seed(1)
# randomly subsample 120
#grid_foc_general = sample(grid_foc_general)
grid_foc_general = st_as_sf(grid_foc_general)
grid_foc_general$dummy = "dummy"

# get veg type
grid_foc_general = st_intersection(grid_foc_general |> st_transform(st_crs(veg_foc)), veg_foc)

# get elev
el = terra::extract(dem_mask, grid_foc_general)[,2]
grid_foc_general$elev = el

grid_foc_general$type = "full landscape"


hist(grid_foc_general$elev)
table(grid_foc_general$CWHR_Type) |> plot()

both = bind_rows(plots, grid_foc_general)


ggplot(both, aes(x = CWHR_Type)) +
  geom_histogram(stat = "count") +
  facet_wrap(~type, scales = "free")

ggplot(both, aes(x = elev)) +
  geom_histogram(bins = 15) +
  facet_wrap(~type, scales = "free")



##### Second iteration: subsample the SMC plots, drop MHC, and add more to PPN, DFR, WFR
set.seed(1)
plots_smc = plots[plots$CWHR_Type == "SMC",]
plots_smc = plots_smc[sample(1:nrow(plots_smc), 40), ]
plots_otherfoc = plots[plots$CWHR_Type %in% c("PPN", "WFR", "DFR"),]


## make grid in only PPN, DFR, WFR
grid = st_make_grid(foc, cellsize = 500, what = "centers")
# grid = st_as_sf(grid)

grid_foc_index = st_intersects(grid, foc, sparse = FALSE)[,1]
grid_foc = grid[grid_foc_index == TRUE]

# filter to the 3 focal types
grid_foc_index = st_intersects(grid_foc, veg_dfwfpp_union, sparse = FALSE)[,1]
grid_foc = grid_foc[grid_foc_index == TRUE]

# get distance from an existing point
existing_pts = bind_rows(plots_smc, plots_otherfoc)

dist = st_distance(grid_foc, existing_pts)
dist = apply(dist, 1, min)

grid_foc = grid_foc[which(dist > 500)]
set.seed(1)
grid_foc = grid_foc[sample(1:length(grid_foc), 16)]

st_write(grid_foc, file.path(datadir, "temp/new_candidate_points.gpkg"), delete_dsn = TRUE)
st_write(existing_pts, file.path(datadir, "temp/existing_keep_points.gpkg"), delete_dsn = TRUE)

grid_foc = st_as_sf(grid_foc) |>
  select(geometry = x)
grid_foc$dummy = "dummy"

## give them a name
grid_foc$Name = paste0("S3", str_pad(1:nrow(grid_foc), width = 2, side = "left", pad = "0"))

v2_pts = bind_rows(grid_foc, existing_pts) |>
  select(Name)

# get their elev, veg type
v2_pts = st_intersection(v2_pts |> st_transform(st_crs(veg_foc)), veg_foc)

# get elev
el = terra::extract(dem_mask, v2_pts)[,2]
v2_pts$elev = el


ggplot(v2_pts, aes(x = CWHR_Type)) +
  geom_histogram(stat = "count")

ggplot(v2_pts, aes(x = elev)) +
  geom_histogram(bins = 15)

v2_pts = v2_pts |>
  select(Name, geometry)













##### Third iteration: Add more PPN
set.seed(1)

## make grid in only PPN, DFR, WFR
grid = st_make_grid(foc, cellsize = 500, what = "centers")
# grid = st_as_sf(grid)

grid_foc_index = st_intersects(grid, foc, sparse = FALSE)[,1]
grid_foc = grid[grid_foc_index == TRUE]

# filter to the 3 focal types
grid_foc_index = st_intersects(grid_foc, veg_pp_union, sparse = FALSE)[,1]
grid_foc = grid_foc[grid_foc_index == TRUE]

# get distance from an existing point
existing_pts = (v2_pts)

dist = st_distance(grid_foc, existing_pts)
dist = apply(dist, 1, min)

grid_foc = grid_foc[which(dist > 500)]
set.seed(1)
grid_foc = grid_foc[sample(1:length(grid_foc), 8)]

st_write(grid_foc, file.path(datadir, "temp/new_candidate_points.gpkg"), delete_dsn = TRUE)
st_write(existing_pts, file.path(datadir, "temp/existing_keep_points.gpkg"), delete_dsn = TRUE)

grid_foc = st_as_sf(grid_foc) |>
  select(geometry = x)
grid_foc$dummy = "dummy"

## give them a name
grid_foc$Name = paste0("S5", str_pad(1:nrow(grid_foc), width = 2, side = "left", pad = "0"))

v2_pts_w_ppn = bind_rows(v2_pts, grid_foc) |>
  select(Name)

# get their elev, veg type
v2_pts_w_ppn = st_intersection(v2_pts_w_ppn |> st_transform(st_crs(veg_foc)), veg_foc)

# get elev
el = terra::extract(dem_mask, v2_pts_w_ppn)[,2]
v2_pts_w_ppn$elev = el


ggplot(v2_pts_w_ppn, aes(x = CWHR_Type)) +
  geom_histogram(stat = "count")

ggplot(v2_pts_w_ppn, aes(x = elev)) +
  geom_histogram(bins = 15)

v2_pts_w_ppn = v2_pts_w_ppn |>
  select(Name, geometry)
















######## Add new backup points
## make grid in only PPN, DFR, WFR
grid = st_make_grid(foc, cellsize = 500, what = "centers")
# grid = st_as_sf(grid)

grid_foc_index = st_intersects(grid, foc, sparse = FALSE)[,1]
grid_foc = grid[grid_foc_index == TRUE]

# get distance from an existing point
existing_pts = v2_pts_w_ppn

dist = st_distance(grid_foc, existing_pts)
dist = apply(dist, 1, min)

grid_foc = grid_foc[which(dist > 500)]
set.seed(1)
grid_foc = grid_foc[sample(1:length(grid_foc), 99)]

grid_foc = st_as_sf(grid_foc) |>
  select(geometry = x)
grid_foc$dummy = "dummy"

## give them a name
grid_foc$Name = paste0("S9", str_pad(1:nrow(grid_foc), width = 2, side = "left", pad = "0"))

v2_pts_w_backup = bind_rows(v2_pts_w_ppn, grid_foc) |>
  select(Name)



### Save in requested formats
st_write(v2_pts_w_backup, file.path(datadir, "selected-plots-for-veg-survey/small-plots_for-iri_v2.kml"), delete_dsn = TRUE)
st_write(v2_pts_w_backup, file.path(datadir, "selected-plots-for-veg-survey/small-plots_for-iri_v2.shp"), delete_dsn = TRUE)

