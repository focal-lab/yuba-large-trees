# load libraries
library(terra)
library(here)
library(sf)
library(tidyverse)

datadir = readLines(here("data-dir.txt"))

candidates = st_read(file.path(datadir, "spatial/raw/drone-plots/site-centers_NY_v1.gpkg")) |>
  mutate(plot_id_drone = str_sub(simple_plot_id, 4, -1) |> as.numeric())

candidates[is.na(candidates$has_vp_data), "has_vp_data"] = FALSE

drone = read_csv(file.path(datadir, "spatial/raw/drone-plots/ny-completed-drone-plots_2023-07-12.csv")) |>
  mutate(plot_id = as.numeric(plot_id))

candidates[candidates$plot_id_drone %in% drone$plot_id, "drone_collection_completed"] = TRUE
candidates[is.na(candidates$drone_collection_completed), "drone_collection_completed"] = FALSE

future_drone_plots = c(26, 21, 146, 156, 174, 148, 193, 200, 206)

candidates = candidates |>
  filter((drone_collection_completed & !has_vp_data) | plot_id_drone %in% future_drone_plots)

table(candidates$cat_id)

# Plots to remove to get to 24 plots for contractor
plots_remove_1 = c(21, 148, 156, 206, 200) # these were additional selected SMC plots that have neither ground (VP) nor drone data, selected so that we can collect additional drone data there in case we don't get the VP data
plots_remove_2 = c(504, 505, 521, 528, 536,    507, 512, 520, 539, 529) # these are PPN and WFR plots that have drone data and no ground (VP) data, but there are too many of those plots to fly them all


# additional SMC plots we added that will need drone data (hedging against not getting VP data, save for contractor to do last, in case don't get VP data):
# 

candidates$contractor_note = "a normal"
candidates[candidates$cat_id %in% c("01", "02", "03", "04", "06"), "contractor_note"] = "smc, only do if don't get VP data"
candidates[candidates$plot_id_drone %in% plots_remove_1, "contractor_note"] = "smc extras, only do (instead of extra ppn, wfr plots) if don't get VP data"
candidates[candidates$plot_id_drone %in% plots_remove_2, "contractor_note"] = "wfr&ppn extras, only do if *do* get VP data"

st_write(candidates, file.path(datadir, "selected-plots-for-veg-survey/large-plot-list_fulldata.gpkg"), delete_dsn = TRUE)

coords = st_coordinates(candidates)

candidates = candidates |>
  select(plot_id = plot_id_drone, contractor_note, drone_collection_completed) |>
  mutate(lon = coords[,1], lat = coords[,2]) |>
  arrange(contractor_note, plot_id) |>
  mutate(plot_id = paste0("L", str_pad(plot_id, side = "left", pad = "0", width = 3)))

### Save in requested formats
candidates_for_iri = candidates |>
  filter(contractor_note %in% c("a normal", "wfr&ppn extras, only do if *do* get VP data")) |>
  select(plot_id)
st_write(candidates_for_iri, file.path(datadir, "selected-plots-for-veg-survey/large-plots_for-iri.kml"), delete_dsn = TRUE)
st_write(candidates_for_iri, file.path(datadir, "selected-plots-for-veg-survey/large-plots_for-iri.shp"), delete_dsn = TRUE)

## Save in a table
coords = st_coordinates(candidates)

candidates = candidates |>
  select(plot_id = plot_id_drone, contractor_note) |>
  mutate(lon = coords[,1], lat = coords[,2]) |>
  arrange(contractor_note, plot_id) |>
  mutate(plot_id = paste0("L", str_pad(plot_id, side = "left", pad = "0", width = 3)))

st_geometry(candidates) = NULL

write_csv(candidates, file.path(datadir, "selected-plots-for-veg-survey/large-plot-list_for-iri.csv"))


