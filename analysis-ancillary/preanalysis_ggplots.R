# Author: Emily Marie Purvis
# Date: 10.14.2024
# Goal: create a template that can perform maximum thinning for any diameter limit scenario

#### Load libraries ####

library(tidyverse)
library(ggplot2)
library(stringr)

#### Import data ####

trees <- read.csv("C:\\Users\\empurvis\\Box\\TNC_Yuba\\data\\IRI data\\field_trees.csv")

plots <- read.csv("C:\\Users\\empurvis\\Box\\TNC_Yuba\\data\\IRI data\\field_plots.csv")

megaplots <- read.csv("C:\\Users\\empurvis\\Box\\TNC_Yuba\\data\\megaplots_116.csv")

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

trees$ba_m2 <- (pi * (((trees$DBH..cm./100)/2)^2))

#### Create data.frame with unthinned density and basal area for each plot ####

plot_summaries <- as.data.frame(unique(trees$Plot.))

plot_summaries <- plot_summaries %>% rename("plot_number" = "unique(trees$Plot.)")

plot_summaries <- plot_summaries %>% add_column(unthinned_ba_m2ha = "")

plot_summaries <- plot_summaries %>% add_column(unthinned_density_tph = "")

# large plots (all trees >10" DBH) are 0.1050708628 hectare. small plots (all trees <=10") are 0.0262677157 hectare. 

#unthinned basal area (m2/ha) is the sum of 1. the sum of the basal areas of trees >10" DBH divided by 0.1050708628 plus 2. the sum of the basal areas of trees <=10" DBH divided by 0.0262677157. unthinned density (trees per hectare) is the sum of 1. the number of trees >10" DBH divided by 0.1050708628 plus 2. the number of trees <=10" DBH divided by 0.0262677157. automate these calculations.

for(i in 1:nrow(plot_summaries)) {
  
  plot_id_current = plot_summaries[i,]$plot_number
  
  assign(paste0(plot_id_current), (trees %>% filter (Plot. == paste0(plot_id_current))))
  
  assign(paste0(plot_id_current, "_largetrees"), (get(paste0(plot_id_current)) %>% filter (DBH..inches. > 10)))
  
  assign(paste0(plot_id_current, "_smalltrees"), (get(paste0(plot_id_current)) %>% filter (DBH..inches. <= 10)))
  
  plot_summaries$unthinned_density_tph[i] = (sum(get(paste0(plot_id_current))$DBH..inches. > 10)/0.1050708628) + (sum(get(paste0(plot_id_current))$DBH..inches. <= 10)/0.0262677157)
  
  plot_summaries$unthinned_ba_m2ha[i] = (sum(get(paste0(plot_id_current, "_largetrees"))$ba_m2)/0.1050708628) + (sum(get(paste0(plot_id_current, "_smalltrees"))$ba_m2)/0.0262677157)
  
}

# export

# write.csv(plot_summaries, "C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\plot_summaries_unthinned_corrected.csv")

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
  
  above_cap_largetrees <- filter (above_cap, (DBH..inches. > 10))
  
  above_cap_smalltrees <- filter(above_cap, (DBH..inches. <= 10))
  
  plot_summaries$thinned_density_tph[i] = (nrow(above_cap_largetrees)/0.1050708628) + (nrow(above_cap_smalltrees)/0.0262677157) # consider changing "thinned_density_tph" to include a descriptor of the diameter cutting scenario, e.g. "thinned_density_tph_30in" for the blanket 30 in rule
  
  plot_summaries$thinned_ba_m2ha[i] = (sum(above_cap_largetrees$ba_m2)/0.1050708628) + (sum(above_cap_smalltrees$ba_m2)/0.0262677157) # consider changing "thinned_ba_m2ha" to include a descriptor of the diameter cutting scenario, e.g. "thinned_ba_m2ha_30in" for the blanket 30 in rule
  
}

# blanket 30 inch cap (remove all trees under 30 inches DBH)

trees$diam_cutoff_in <- 30

plot_summaries <- plot_summaries %>% add_column(thinned_density_tph_30in = "")

plot_summaries <- plot_summaries %>% add_column(thinned_ba_m2ha_30in = "")

for(i in 1:nrow(plot_summaries)) {
  
  plot_id_current = plot_summaries[i,]$plot_number
  
  assign(paste0(plot_id_current), (trees %>% filter (Plot. == paste0(plot_id_current))))
  
  above_cap <- filter(get(paste0(plot_id_current)), DBH..inches. >= diam_cutoff_in)
  
  above_cap_largetrees <- filter (above_cap, (DBH..inches. > 10))
  
  above_cap_smalltrees <- filter(above_cap, (DBH..inches. <= 10))
  
  plot_summaries$thinned_density_tph_30in[i] = (nrow(above_cap_largetrees)/0.1050708628) + (nrow(above_cap_smalltrees)/0.0262677157)
  
  plot_summaries$thinned_ba_m2ha_30in[i] = (sum(above_cap_largetrees$ba_m2)/0.1050708628) + (sum(above_cap_smalltrees$ba_m2)/0.0262677157) 
  
}

# blanket 40 inch cap (remove all trees under 40 inches DBH)

trees$diam_cutoff_in <- 40

plot_summaries <- plot_summaries %>% add_column(thinned_density_tph_40in = "")

plot_summaries <- plot_summaries %>% add_column(thinned_ba_m2ha_40in = "")

for(i in 1:nrow(plot_summaries)) {
  
  plot_id_current = plot_summaries[i,]$plot_number
  
  assign(paste0(plot_id_current), (trees %>% filter (Plot. == paste0(plot_id_current))))
  
  above_cap <- filter(get(paste0(plot_id_current)), DBH..inches. >= diam_cutoff_in)
  
  above_cap_largetrees <- filter (above_cap, (DBH..inches. > 10))
  
  above_cap_smalltrees <- filter(above_cap, (DBH..inches. <= 10))
  
  plot_summaries$thinned_density_tph_40in[i] = (nrow(above_cap_largetrees)/0.1050708628) + (nrow(above_cap_smalltrees)/0.0262677157)
  
  plot_summaries$thinned_ba_m2ha_40in[i] = (sum(above_cap_largetrees$ba_m2)/0.1050708628) + (sum(above_cap_smalltrees$ba_m2)/0.0262677157) 
  
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

# write.csv(plot_summaries, "C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\plot_summaries_thinned_demos_corrected.csv")

#### Graph results ####

plot_summaries$unthinned_ba_m2ha <- as.numeric(plot_summaries$unthinned_ba_m2ha)

plot_summaries$unthinned_density_tph <- as.numeric(plot_summaries$unthinned_density_tph)

plot_summaries$thinned_ba_m2ha_30in <- as.numeric(plot_summaries$thinned_ba_m2ha_30in)

plot_summaries$thinned_density_tph_30in <- as.numeric(plot_summaries$thinned_density_tph_30in)

plot_summaries$thinned_ba_m2ha_40in <- as.numeric(plot_summaries$thinned_ba_m2ha_40in)

plot_summaries$thinned_density_tph_40in <- as.numeric(plot_summaries$thinned_density_tph_40in)

# make a subset of plot_summaries that only has mixed conifer plots

# plot_summaries <- read.csv("C:\\Users\\emily\\Desktop\\TNC Yuba\\data\\IRI data\\plot_summaries_thinned_demos_corrected.csv")

plot_summaries_mixedconifer <- subset (plot_summaries, mixed_conifer == TRUE)

## make a data.frame of megaplot info equivalent to plot_summaries_mixedconifer. note that these mega-plots only have mixed conifer plots.

# make data.frame with megaplot_id and bpu number

megaplot_id <- c(1:500)

megaplot_summaries <- as.data.frame(megaplot_id)

megaplot_summaries <- megaplot_summaries %>% add_column(bpu=0)

megaplot_summaries[1:129,2] <- 1 # bpu 1 plots

megaplot_summaries[130:254,2] <- 2 # bpu 2 plots

megaplot_summaries[255:366,2] <- 3 # bpu 3 plots

megaplot_summaries[367:500,2] <- 4 # bpu 4 plots

# add unthinned density and ba using same code as above

megaplot_summaries <- megaplot_summaries %>% add_column(unthinned_ba_m2ha = "")

megaplot_summaries <- megaplot_summaries %>% add_column(unthinned_density_tph = "")

for(i in 1:nrow(megaplot_summaries)) {
  
  plot_id_current = megaplot_summaries[i,]$megaplot_id
  
  assign(paste0(plot_id_current), (megaplots %>% filter (megaplot_id == paste0(plot_id_current))))
  
  megaplot_summaries$unthinned_density_tph[i] = (nrow(get(paste0(plot_id_current)))/0.5253543)
  
  megaplot_summaries$unthinned_ba_m2ha[i] = (sum(get(paste0(plot_id_current))$ba_m2)/0.5253543)
  
}


#### Violin plots ####

# simple 1 variable density violin plots

violin_unthinned_density <- 
  ggplot(plot_summaries_mixedconifer, aes(x="", y=unthinned_density_tph)) +
  ylim(0,850) +
  labs(title="Unthinned plot density", x="", y="density_tph") +
  geom_violin() + 
  geom_boxplot(width=0.1) +
  theme_classic()

violin_unthinned_density

violin_thinned_density_30 <- 
  ggplot(plot_summaries_mixedconifer, aes(x="", y=thinned_density_tph_30in)) +
  ylim(0,130) +
  labs(title="Thinned plot density 30 inch rule", x="", y="density_tph") +
  geom_violin() + 
  geom_boxplot(width=0.1) +
  theme_classic()

violin_thinned_density_30

# make 2 column data frame to enable creation of side by side violin plots comparing thinning scenarios and unthinned plots

plot_densities <- plot_summaries_mixedconifer[,-(1:2)]
plot_densities <- plot_densities[,-(2:3)]
plot_densities <- plot_densities[,-(3)]
plot_densities <- plot_densities[,-(4)]

plot_density_unthinned <- as.data.frame(plot_densities[,-(2:3)])
plot_density_unthinned$thinning_scenario <- "unthinned"
plot_density_unthinned <- plot_density_unthinned %>% rename("density_tph" = 1)

plot_density_30 <- plot_densities[,-(1)]
plot_density_30$thinning_scenario <- "30 inch rule"
plot_density_30 <- plot_density_30[,-(2)]
plot_density_30 <- plot_density_30 %>% rename("density_tph" = 1)

plot_density_40 <- as.data.frame(plot_densities[,-(1:2)])
plot_density_40$thinning_scenario <- "40 inch rule"
plot_density_40 <- plot_density_40 %>% rename("density_tph" = 1)

plot_density_2column <- full_join(plot_density_30, plot_density_40)
plot_density_2column <- full_join(plot_density_2column, plot_density_unthinned)

# violin plot with all densities

violin_all_densities <- 
  ggplot(plot_density_2column, aes(x=thinning_scenario, y=density_tph, fill = thinning_scenario)) +
  ylim(0,700) +
  labs(title="Plot Densities", x="Thinning scenario", y="Density (trees per hectare)") +
  geom_violin() + 
  geom_boxplot(width=0.01) +
  theme_classic()

violin_all_densities

# violin plot with just 30 and 40 inch rule densities

plot_density_2column_3040 <- full_join(plot_density_30, plot_density_40)

violin_density_thinned <- 
  ggplot(plot_density_2column_3040, aes(x=thinning_scenario, y=density_tph, fill = thinning_scenario)) +
  ylim(0,130) +
  labs(title="Plot Densities", x="Thinning scenario", y="Density (trees per hectare)") +
  geom_violin() + 
  geom_boxplot(width=0.05) +
  theme_classic()

violin_density_thinned

# simple 1 variable ba violin plots

violin_unthinned_ba <- 
  ggplot(plot_summaries_mixedconifer, aes(x="", y=unthinned_ba_m2ha)) +
  # ylim(0,1600) +
  labs(title="Unthinned plot basal area", x="", y="ba_m2ha") +
  geom_violin() + 
  geom_boxplot(width=0.1) +
  theme_classic()

violin_unthinned_ba

# make 2 column BA data frame to enable creation of side by side violin plots comparing thinning scenarios and unthinned plots

plot_bas <- plot_summaries_mixedconifer[,-(1)]
plot_bas <- plot_bas[,-(2:5)]
plot_bas <- plot_bas[,-(3)]

plot_ba_unthinned <- as.data.frame(plot_bas[,-(2:3)])
plot_ba_unthinned$thinning_scenario <- "unthinned"
plot_ba_unthinned <- plot_ba_unthinned %>% rename("ba_m2ha" = 1)

plot_ba_30 <- plot_bas[,-(1)]
plot_ba_30$thinning_scenario <- "30 inch rule"
plot_ba_30 <- plot_ba_30[,-2]
plot_ba_30 <- plot_ba_30 %>% rename("ba_m2ha" = 1)

plot_ba_40 <- as.data.frame(plot_bas[,-(1:2)])
plot_ba_40$thinning_scenario <- "40 inch rule"
plot_ba_40 <- plot_ba_40 %>% rename("ba_m2ha" = 1)

plot_bas_2column <- full_join(plot_ba_30, plot_ba_40)
plot_bas_2column <- full_join(plot_bas_2column, plot_ba_unthinned)

# violin plot with all BAs

violin_all_bas <- 
  ggplot(plot_bas_2column, aes(x=thinning_scenario, y=ba_m2ha, fill = thinning_scenario)) +
  #  ylim(0,850) +
  labs(title="Plot basal areas", x="Thinning scenario", y="Basal area (m2 per ha)") +
  geom_violin() + 
  geom_boxplot(width=0.01) +
  theme_classic()

violin_all_bas

#### Histograms ####

# unthinned density

histo_unthinned_density <-
  ggplot(plot_summaries_mixedconifer, aes(x=unthinned_density_tph, y = after_stat(density))) + 
  geom_histogram(binwidth=25, , colour="black", fill="white") +
  labs(title="Unthinned plot densities", x="Unthinned density (tph)", y="Number of plots") +
  geom_density(alpha=.25) 

histo_unthinned_density

# the kernal density estimate looks dumb

histo_unthinned_density <-
  ggplot(plot_summaries_mixedconifer, aes(x=unthinned_density_tph)) + 
  geom_histogram(binwidth=25, , colour="black", fill="white") +
  labs(title="Unthinned plot densities", x="Unthinned density (tph)", y="Number of plots") +
  theme(plot.title = element_text(hjust = 0.5))

histo_unthinned_density

# histogram with the unthinned + thinned densities

histo_all_densities <-
  ggplot(plot_density_2column, aes(x=density_tph, color = thinning_scenario)) + 
  geom_histogram(binwidth=10, fill="white", alpha = 0.5, position = "dodge") +
  labs(title="Plot densities", x="Density (tph)", y="Number of plots") +
  guides(color = guide_legend(title = "Thinning scenario")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette="Set2")

histo_all_densities

# histogram with unthinned + thinned BAs

histo_all_bas <-
  ggplot(plot_bas_2column, aes(x=ba_m2ha, color = thinning_scenario)) + 
  geom_histogram(binwidth=25, fill="white", alpha = 0.5, position = "dodge") +
  labs(title="Plot basal areas", x="Basal area (m2/ha)", y="Number of plots") +
  guides(color = guide_legend(title = "Thinning scenario")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette="Set2")

histo_all_bas

## histogram with unthinned plot density vs unthinned megaplot density

# make a combined data.frame with all unthinned density values and corresponding dataset

plot_density <- as.data.frame(as.numeric(plot_summaries_mixedconifer$unthinned_density_tph)) %>% rename (unthinned_density_tph = 1)

megaplot_density <- as.data.frame(as.numeric(megaplot_summaries$unthinned_density_tph)) %>% rename (unthinned_density_tph = 1)

plot_density$dataset = "unthinned plots"

megaplot_density$dataset = "unthinned megaplots"

densityresults_forplotting <- full_join(plot_density, megaplot_density)

# cut data

density_breaks <- c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000)

densityresults_forplotting$density_cut =
  cut(densityresults_forplotting$unthinned_density_tph, breaks = density_breaks)

# sort data

densityresults_forplotting =
  densityresults_forplotting %>%
  group_by(dataset, density_cut) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# make labels

density_labels = c("0-200", "200-400", "400-600", "600-800", "800-1000","1000-1200", "1200-1400", "1400-1600", "1600-1800", "1800-2000")

# plot

ggplot(data=densityresults_forplotting,
       aes(x = density_cut, y=freq*100, fill=dataset)) +
  geom_bar(stat="identity",position="dodge") +
  scale_x_discrete(labels = density_labels) +
  ylab("Percentage of plots") +
  xlab("Density (tph)")

# histogram with unthinned plot BA vs unthinned megaplot BA

# make a combined data.frame with all unthinned BA values and corresponding dataset

plot_ba <- as.data.frame(as.numeric(plot_summaries_mixedconifer$unthinned_ba_m2ha)) %>% rename (unthinned_ba_m2ha = 1)

megaplot_ba <- as.data.frame(as.numeric(megaplot_summaries$unthinned_ba_m2ha)) %>% rename (unthinned_ba_m2ha = 1)

plot_ba$dataset = "unthinned plots"

megaplot_ba$dataset = "unthinned megaplots"

baresults_forplotting <- full_join(plot_ba, megaplot_ba)

# cut data

ba_breaks <- c(0, 20, 40, 60, 80, 100, 120, 140, 160)

baresults_forplotting$ba_cut =
  cut(baresults_forplotting$unthinned_ba_m2ha, breaks = ba_breaks)

# sort data

baresults_forplotting =
  baresults_forplotting %>%
  group_by(dataset, ba_cut) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# make labels

ba_labels = c("0-20", "20-40", "40-60", "60-80", "80-100","100-120", "120-140", "140-160", "160-180")

# plot

ggplot(data=baresults_forplotting,
       aes(x = ba_cut, y=freq*100, fill=dataset)) +
  geom_bar(stat="identity",position="dodge") +
  scale_x_discrete(labels = ba_labels) +
  ylab("Percentage of plots") +
  xlab("Basal area (m2/ha)")

## add in simulated tph data to the above two histograms (unthinned plots vs megaplots)

NRV_sim_tph <- read.csv("C:\\Users\\empurvis\\Box\\TNC_Yuba\\nrv_docs\\simulation_drafts\\simulated_tph_draft_20241024.csv")

NRV_sim_ba <- read.csv("C:\\Users\\empurvis\\Box\\TNC_Yuba\\nrv_docs\\simulation_drafts\\simulated_ba_draft_20241024.csv")

# prepare simulated tph data

simulated_tph <- NRV_sim_tph[,-1]

simulated_tph <- as.data.frame(simulated_tph[,-c(2:3)]) %>% rename (unthinned_density_tph = 1)

simulated_tph$dataset = "NRV simulation"

# prepare simulated ba data

simulated_ba <- NRV_sim_ba[,-1]

simulated_ba <- as.data.frame(simulated_ba[,-c(2:3)]) %>% rename (unthinned_ba_m2ha = 1)

simulated_ba$dataset = "NRV simulation"

# density histo

densityresults_forplotting <- full_join(plot_density, megaplot_density)

densityresults_forplotting <- full_join(densityresults_forplotting, simulated_tph)

density_breaks <- c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, 3000)

densityresults_forplotting$density_cut =
  cut(densityresults_forplotting$unthinned_density_tph, breaks = density_breaks)

densityresults_forplotting =
  densityresults_forplotting %>%
  group_by(dataset, density_cut) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

density_labels = c("0-200", "200-400", "400-600", "600-800", "800-1000","1000-1200", "1200-1400", "1400-1600", "1600-1800", "1800-2000", "2000-3000")

ggplot(data=densityresults_forplotting,
       aes(x = density_cut, y=freq*100, fill=dataset)) +
  geom_bar(stat="identity",position="dodge") +
  scale_x_discrete(labels = density_labels) +
  ylab("Percentage of plots") +
  xlab("Density (tph)")

# ba histo

baresults_forplotting <- full_join(plot_ba, megaplot_ba)

baresults_forplotting <- full_join(baresults_forplotting, simulated_ba)

ba_breaks <- c(0, 20, 40, 60, 80, 100, 120, 140, 160, 1400)

baresults_forplotting$ba_cut =
  cut(baresults_forplotting$unthinned_ba_m2ha, breaks = ba_breaks)

baresults_forplotting =
  baresults_forplotting %>%
  group_by(dataset, ba_cut) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ba_labels = c("0-20", "20-40", "40-60", "60-80", "80-100","100-120", "120-140", "140-160", "160-1400")

ggplot(data=baresults_forplotting,
       aes(x = ba_cut, y=freq*100, fill=dataset)) +
  geom_bar(stat="identity",position="dodge") +
  scale_x_discrete(labels = ba_labels) +
  ylab("Percentage of plots") +
  xlab("Basal area (m2/ha)")