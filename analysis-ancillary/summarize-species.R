# Author: Emily Marie Purvis
# Date: 10.17.2024
# Goal: Create some quick species summaries to aid in deciding which species to thin in our scenarios

#### Load libraries ####

library(tidyverse)

#### Import data ####

trees <- read.csv("C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\field_trees.csv")

plot_summaries <- read.csv("C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\plot_summaries_thinned_demos.csv")

# Add species alphabetical codes for clarity

trees <- trees %>%
  add_column(spp_alpha = "")

trees$spp_alpha[trees$Species == '0'] <- 'unknown' 

trees$spp_alpha[trees$Species == '15'] <- 'ABCO' 

trees$spp_alpha[trees$Species == '20'] <- 'ABMA' 

trees$spp_alpha[trees$Species == '64'] <- 'JUOC' 

trees$spp_alpha[trees$Species == '81'] <- 'CADE' 

trees$spp_alpha[trees$Species == '108'] <- 'PICO' 

trees$spp_alpha[trees$Species == '116'] <- 'PIJE' 

trees$spp_alpha[trees$Species == '117'] <- 'PILA' 

trees$spp_alpha[trees$Species == '119'] <- 'PIMO3' 

trees$spp_alpha[trees$Species == '122'] <- 'PIPO' 

trees$spp_alpha[trees$Species == '202'] <- 'PSME' 

trees$spp_alpha[trees$Species == '231'] <- 'TABR2' 

trees$spp_alpha[trees$Species == '312'] <- 'ACMA3' 

trees$spp_alpha[trees$Species == '361'] <- 'ARME' 

trees$spp_alpha[trees$Species == '631'] <- 'NODE3' 

trees$spp_alpha[trees$Species == '805'] <- 'QUCH2' 

trees$spp_alpha[trees$Species == '818'] <- 'QUKE' 

trees$spp_alpha[trees$Species == '839'] <- 'QUWI2' 

# calculate basal area for each tree

trees <- trees %>%
  add_column(ba_m2 = "")

trees$ba_m2 <- ((pi * ((trees$DBH..cm./2)^2))/1000)

# remove trees in western white pine and red fir plots

plot_summaries_mixedconifer <- subset (plot_summaries, mixed_conifer == TRUE)

mixedconiferplots <- plot_summaries_mixedconifer[,-c(2:3)]

mixedconiferplots <- mixedconiferplots[,-c(4:7)]

mixedconiferplots <- mixedconiferplots %>% rename (Plot. = 1)

trees_wplotinfo <- left_join(trees, mixedconiferplots)

trees_mixedconiferplots <- subset (trees_wplotinfo, mixed_conifer == TRUE)

#### Calculations ####

# Table of all the species

spp <- as.data.frame(unique(trees_mixedconiferplots$spp_alpha)) %>% rename (Species = 1)

# Number of total individuals by species

spp$total_individuals <- ""

for(i in 1:nrow(spp)) {
  
  spp_id_current = spp[i,]$Species
  
  spp$total_individuals[i] = nrow(subset (trees_mixedconiferplots, spp_alpha == spp_id_current))
    
}

# Basal area total by species

spp$total_ba <- ""

for(i in 1:nrow(spp)) {
  
  spp_id_current = spp[i,]$Species
  
  assign(paste0(spp_id_current), (subset(trees_mixedconiferplots, spp_alpha ==     spp_id_current)))
  
  spp$total_ba[i] = sum(get(paste0(spp_id_current))$ba_m2)
  
}

# Number of plots with at least one individual of that species

spp$total_plots_containing <- ""

for(i in 1:nrow(spp)) {
  
  spp_id_current = spp[i,]$Species
  
  spp$total_plots_containing[i] = nrow(as.data.frame(unique(get(paste0     (spp_id_current))$Plot.)))
  
}

#### Export ####

write.csv(spp, "C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\species_summaries_for_scenario_determination.csv")

species_summary <- read.csv("C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\species_summaries_for_scenario_determination.csv")
