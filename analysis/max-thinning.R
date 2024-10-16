# Author: Emily Marie Purvis
# Date: 10.14.2024
# Goal: create a template that can perform maximum thinning for any diameter limit scenario

#### Load libraries ####

library(tidyverse)
library(stringr)

#### Import data ####

trees <- read.csv("C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\field_trees.csv")

plots <- read.csv("C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\field_plots.csv")

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

#### Create data.frame with unthinned density and basal area for each plot ####

plot_summaries <- as.data.frame(unique(trees$Plot.))

plot_summaries <- plot_summaries %>% rename("plot_number" = "unique(trees$Plot.)")

plot_summaries <- plot_summaries %>% add_column(unthinned_ba_m2ha = "")

plot_summaries <- plot_summaries %>% add_column(unthinned_density_tph = "")

# each plot (both small + large radii) is 0.1050708628 hectare. unthinned basal area (m2/ha) is the sum of all the basal areas in the plot divided by the area in hectares. unthinned density (trees per hectare) is the number of trees in the plot divided by the area in hectares. automate these calculations.

for(i in 1:nrow(plot_summaries)) {
  
  plot_id_current = plot_summaries[i,]$plot_number
  
  assign(paste0(plot_id_current), (trees %>% filter (Plot. == paste0(plot_id_current))))
  
  plot_summaries$unthinned_density_tph[i] = (nrow(get(paste0(plot_id_current)))/0.1050708628) 
    
  plot_summaries$unthinned_ba_m2ha[i] = (sum(get(paste0(plot_id_current))$ba_m2)/0.1050708628)
  
}

# export

# write.csv(plot_summaries, "C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\plot_summaries_unthinned.csv")

# add forest type and mixed conifer T/F columns

plot_summaries <- plot_summaries %>% add_column(forest_type = "")

plot_summaries <- plot_summaries %>% add_column(mixed_conifer = "")

plot_number <- "([S][0-9]{3})"

plots$plot_id_main <- str_extract(plots$plot_id, plot_number)

for(i in 1:nrow(plot_summaries)) {
  
  plot_id_current = plot_summaries[i,]$plot_number
  
  plot_summaries$forest_type[i] = plots$forest_type[plots$plot_id_main == plot_id_current]
  
}

# Western white pine and red fir forests types don't count as mixed conifer. California mixed conifer, douglas fir, ponderosa pine, jeffery pine, and tanoak forest types DO count as mixed conifer. We can re-evaluate this later as needed-- in particular, we should remove NODE if we aren't thinning NODEs. 

for(i in 1:nrow(plot_summaries)) {
  
  plot_id_current = plot_summaries[i,]$plot_number
  
  if((plot_summaries[i,]$forest_type) == "371 – California mixed conifer") {
    
    plot_summaries$mixed_conifer[i] = "TRUE"
    
  }
  
  else if((plot_summaries[i,]$forest_type) == "261 – White fir") {
    
    plot_summaries$mixed_conifer[i] = "TRUE"
    
  }

  else if((plot_summaries[i,]$forest_type) == "201 – Douglas-fir") {
    
    plot_summaries$mixed_conifer[i] = "TRUE"
    
  }
  
  else if((plot_summaries[i,]$forest_type) == "221 – Ponderosa pine") {
    
    plot_summaries$mixed_conifer[i] = "TRUE"
    
  }
  
  else if((plot_summaries[i,]$forest_type) == "223 – Jeffrey pine / Coulter pine / bigcone Douglas-fir") {
    
    plot_summaries$mixed_conifer[i] = "TRUE"
    
  }
  
  else if((plot_summaries[i,]$forest_type) == "941 – Tanoak") {
    
    plot_summaries$mixed_conifer[i] = "TRUE"
    
  }
  
  else {
    
    plot_summaries$mixed_conifer[i] = "FALSE"
    
  }
}

#### Max thinning function ####

# Goal: for a given diameter scenario, remove all trees under the cap. Before running this, must define trees$diam_cutoff_in. 

for(i in 1:nrow(plot_summaries)) {
  
  plot_id_current = plot_summaries[i,]$plot_number
  
  assign(paste0(plot_id_current), (trees %>% filter (Plot. == paste0(plot_id_current))))
  
  above_cap <- filter(get(paste0(plot_id_current)), DBH..inches. >= diam_cutoff_in)
  
  plot_summaries$thinned_density_tph[i] = (nrow(above_cap)/0.1050708628) # consider changing "thinned_density_tph" to include a descriptor of the diameter cutting scenario, e.g. "thinned_density_tph_30in" for the blanket 30 in rule
  
  plot_summaries$thinned_ba_m2ha[i] = (sum(above_cap$ba_m2)/0.1050708628) # consider changing "thinned_ba_m2ha" to include a descriptor of the diameter cutting scenario, e.g. "thinned_ba_m2ha_30in" for the blanket 30 in rule
  
}

# blanket 30 inch cap (remove all trees under 30 inches DBH)

trees$diam_cutoff_in <- 30

plot_summaries <- plot_summaries %>% add_column(thinned_density_tph_30in = "")

plot_summaries <- plot_summaries %>% add_column(thinned_ba_m2ha_30in = "")

for(i in 1:nrow(plot_summaries)) {
  
  plot_id_current = plot_summaries[i,]$plot_number
  
  assign(paste0(plot_id_current), (trees %>% filter (Plot. == paste0(plot_id_current))))
  
  above_cap <- filter(get(paste0(plot_id_current)), DBH..inches. >= diam_cutoff_in)
  
  plot_summaries$thinned_density_tph_30in[i] = (nrow(above_cap)/0.1050708628) 
  
  plot_summaries$thinned_ba_m2ha_30in[i] = (sum(above_cap$ba_m2)/0.1050708628)
  
}

# blanket 40 inch cap (remove all trees under 40 inches DBH)

trees$diam_cutoff_in <- 40

plot_summaries <- plot_summaries %>% add_column(thinned_density_tph_40in = "")

plot_summaries <- plot_summaries %>% add_column(thinned_ba_m2ha_40in = "")

for(i in 1:nrow(plot_summaries)) {
  
  plot_id_current = plot_summaries[i,]$plot_number
  
  assign(paste0(plot_id_current), (trees %>% filter (Plot. == paste0(plot_id_current))))
  
  above_cap <- filter(get(paste0(plot_id_current)), DBH..inches. >= diam_cutoff_in)
  
  plot_summaries$thinned_density_tph_40in[i] = (nrow(above_cap)/0.1050708628) 
  
  plot_summaries$thinned_ba_m2ha_40in[i] = (sum(above_cap$ba_m2)/0.1050708628)
  
}

# template for diameter cut-offs that vary by species. for right now I included all species in all the plots; we can remove species later (like western white pines) once we decide which to include

trees$diam_cutoff_in <- 0

for(i in 1:nrow(trees)) {
  
  if((trees[i,]$spp_alpha) == "ABCO") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "ABMA") {
    
    trees$diam_cutoff_in[i] = ""
    
  }

  else if((trees[i,]$spp_alpha) == "ACMA3") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "ARME") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "CADE") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "JUOC") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "NODE3") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "PICO") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "PIJE") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "PILA") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "PIMO3") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "PIPO") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "PSME") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "QUCH2") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "QUKE") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "QUWI2") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "TABR2") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
  else if((trees[i,]$spp_alpha) == "unknown") {
    
    trees$diam_cutoff_in[i] = ""
    
  }
  
}

# export 30 and 40 inch blanket rules

write.csv(plot_summaries, "C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\plot_summaries_thinned_demos.csv")
