# Objective: Compute mean tree density across the mixed-conifer plots from the North Yuba study

datadir = readLines("datadir.txt")

library(tidyverse)
library(readxl)

plots = read_excel(file.path(datadir, "survey-data", "iri-tnc-small_plots.xlsx"), sheet = "field_plots")
trees = read_csv(file.path(datadir, "survey-data", "20231222_IRItoOFO_smallplots.csv")) |>
  rename(plot_id = "Plot#",
         tree_num_contractor = "Tree#")


focal_types = c("201 – Douglas-fir",
                "221 – Ponderosa pine",
                "223 – Jeffrey pine / Coulter pine / bigcone Douglas-fir",
                "261 – White fir",
                "371 – California mixed conifer")

plots_foc = plots |>
  filter(forest_type %in% focal_types) |>
  mutate(plot_id_main = str_sub(plot_id, 1, 4))

plots_foc$plot_id
plots_foc$plot_id_main

table(trees$plot_id)

trees_foc = trees |>
  filter(plot_id %in% plots_foc$plot_id_main)

# Look for duplicated trees
trees_minimal = trees_foc |>
  select(plot_id, tree_num_contractor)
dups = duplicated(trees_minimal)
sum(dups)


small_tree_dens = trees_foc |>
  group_by(plot_id) |>
  filter(`DBH (inches)` < 10) |>
  summarize(n_trees = n(),
            ba_tot = sum(3.14 * (`DBH (inches)` / 2)^2)) |>
  mutate(small_tree_density = n_trees / 0.06487603,
         small_tree_ba_ha = ba_tot * 0.00064516 / 0.06487603 / 0.404686) |> # Convert from sq in per plot to sq m per ha
  select(plot_id, small_tree_density, small_tree_ba_ha)

large_tree_density = trees_foc |>
  group_by(plot_id) |>
  filter(`DBH (inches)` >= 10) |>
  summarize(n_trees = n(),
            n_gt40in = sum(`DBH (inches)` > 40, na.rm = TRUE),
            ba_tot = sum(3.14 * (`DBH (inches)` / 2)^2),
            large_qmd = sqrt(mean(`DBH (inches)`^2)),
            max_diam = max(`DBH (inches)`)) |>
  mutate(large_tree_density = n_trees / 0.25950413,
         gt40in_density = n_gt40in / 0.25950413,
         large_tree_ba_ha = ba_tot * 0.00064516 / 0.25950413 / 0.404686,
         large_qmd) |> # Convert from sq in per plot to sq m per ha
  select(plot_id, large_tree_density, gt40in_density, large_tree_ba_ha, large_qmd, max_diam)

full_summary = full_join(small_tree_dens, large_tree_density, by = "plot_id") |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(tree_density = small_tree_density + large_tree_density,
         ba_ha = small_tree_ba_ha + large_tree_ba_ha)

full_summary


ggplot(full_summary, aes(x = tree_density, y = ba_ha)) +
  geom_point()
  
ggplot(full_summary, aes(x = tree_density, y = large_qmd)) +
  geom_point()

# Large tree plots
large_tree_plots = full_summary |>
  filter((large_qmd > 25 & tree_density > 175))
large_tree_plots
mean(tree_density$tree_density)

large_tree_plots = full_summary |>
  filter(gt40in_density > 15)
large_tree_plots


# Pull in the coords
sp = sf::st_read("/home/derek/Documents/repo-data-local/yuba-large-trees_data/selected-plots-for-veg-survey/small-plots_for-iri_v2.shp")
sp

large_sp = right_join(sp, large_tree_plots, by = join_by("Name" == "plot_id"))
sf::st_write(large_sp, "/home/derek/Documents/repo-data-local/ofo-r/field-reference-data/field-plot-summaries/gt40-smallplots.gpkg", delete_dsn = TRUE)



# Large plots
d = read_csv("/home/derek/Documents/repo-data-local/ofo-r/field-reference-data/field-plot-summaries/field-plot-summary.csv")

plots_foc = c(1:51, 88:111) |> str_pad(4, "left", "0")

d = d |>
  filter(plot_id %in% plots_foc)

names(d)

ggplot(d, aes(x = dbh_quad_mean, y = n_trees)) +
  geom_point()

large_tree_plots = d |>
  filter((dbh_quad_mean > 60 & ba_ha > 100))

large_tree_plots

write = large_tree_plots |> sf::st_as_sf(coords = c("plot_lon", "plot_lat"), crs = 4326)
sf::st_write(write, "/home/derek/Documents/repo-data-local/ofo-r/field-reference-data/field-plot-summaries/large-tree-plots.gpkg", delete_dsn = TRUE)


# PLots with highest density of trees over 40 inches

hist(d$n_gt40in)

gt40_plots = d |>
  filter(n_gt40in > 8)
gt40_plots
  
write = gt40_plots |> sf::st_as_sf(coords = c("plot_lon", "plot_lat"), crs = 4326)
sf::st_write(write, "/home/derek/Documents/repo-data-local/ofo-r/field-reference-data/field-plot-summaries/gt40-plots.gpkg", delete_dsn = TRUE)
