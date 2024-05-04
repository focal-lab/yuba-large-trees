library(tidyverse)
library(sf)

datadir = readLines("datadir.txt")

d = read_csv(file.path(datadir, "field-tour-data", "field-tour-points.csv"))

d_sf = st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
st_write(d_sf, file.path(datadir, "field-tour-data", "field-tour-points.kml"), delete_dsn = TRUE)
