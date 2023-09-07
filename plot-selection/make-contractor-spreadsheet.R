# load libraries
library(terra)
library(here)
library(sf)
library(tidyverse)

datadir = readLines(here("data-dir.txt"))


ds = st_read(file.path(datadir, "selected-plots-for-veg-survey/small-plots_for-iri_v2.shp")) |> st_transform(4326)

coords = st_coordinates(ds)

ds$lat = coords[,2]
ds$lon = coords[,1]

ds = ds |>
  arrange(Name) |>
  mutate(across(c(lat, lon), ~round(.x, 7)))

st_geometry(ds) = NULL

write_csv(ds, file.path(datadir, "selected-plots-for-veg-survey/small-plots_for-iri_v2.csv"))







ds = st_read(file.path(datadir, "selected-plots-for-veg-survey/large-plots_for-iri.shp")) |> st_transform(4326)

coords = st_coordinates(ds)

ds$lat = coords[,2]
ds$lon = coords[,1]

ds = ds |>
  select(Name = plot_id, lat, lon) |>
  arrange(Name) |>
  mutate(across(c(lat, lon), ~round(.x, 7)))

st_geometry(ds) = NULL

write_csv(ds, file.path(datadir, "selected-plots-for-veg-survey/large-plots_for-iri.csv"))
