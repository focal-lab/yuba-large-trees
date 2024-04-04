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
  summarize(n_trees = n()) |>
  mutate(small_tree_density = n_trees / 0.06487603) |>
  select(plot_id, small_tree_density)

large_tree_density = trees_foc |>
  group_by(plot_id) |>
  filter(`DBH (inches)` >= 10) |>
  summarize(n_trees = n()) |>
  mutate(large_tree_density = n_trees / 0.25950413) |>
  select(plot_id, large_tree_density)

tree_density = full_join(small_tree_dens, large_tree_density, by = "plot_id") |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  mutate(tree_density = small_tree_density + large_tree_density)

mean(tree_density$tree_density)

mean_trees_per_plot = mean(tree_density$tree_density) * 0.15
mean_trees_per_plot
