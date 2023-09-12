# Compile batches 2 and 3 data for checking

library(here)
library(sf)
library(tidyverse)

# The root of the data directory
data_dir = readLines(here("data-dir.txt"), n=1)

b1 = st_read(file.path(data_dir, "checking-veg-survey/checking-round2/Stem_Batch_2_treecoordinates_updatedplotcenters.gpkg"))
b2 = st_read(file.path(data_dir, "checking-veg-survey/checking-round2/Stem_Batch_3_treecoordinates.gpkg"))

b = bind_rows(b1, b2)

b = b |>
  mutate(TreeNum = Tree., Dist_ft = HorizontalDistance, Az = AZM, DBH_cm = DBH * 2.54, Ht_ft = Actual.HT, Crown = X.Crown, Defects = Live.Tree.Defects, Decay = Snag.Decay.Class)

b$`Species`[b$`Species` == '122'] <- 'PIPO'
b$`Species`[b$`Species` == '15'] <- 'ABCO'
b$`Species`[b$`Species` == '20'] <- 'ABMA'
b$`Species`[b$`Species` == '117'] <- 'PILA'
b$`Species`[b$`Species` == '101'] <- 'PIAL'
b$`Species`[b$`Species` == '119'] <- 'PIMO3'
b$`Species`[b$`Species` == '108'] <- 'PICOL'
b$`Species`[b$`Species` == '81'] <- 'CADE27'
b$`Species`[b$`Species` == '202'] <- 'PSME'
b$`Species`[b$`Species` == '127'] <- 'PISA2'
b$`Species`[b$`Species` == '116'] <- 'PIJE'
b$`Species`[b$`Species` == '103'] <- 'PIAT'
b$`Species`[b$`Species` == '361'] <- 'ARME'
b$`Species`[b$`Species` == '631'] <- 'LIDE3'
b$`Species`[b$`Species` == '312'] <- 'ACMA3'
b$`Species`[b$`Species` == '981'] <- 'UMCA'
b$`Species`[b$`Species` == '333'] <- 'AECA'
b$`Species`[b$`Species` == '805'] <- 'QUCH2'
b$`Species`[b$`Species` == '807'] <- 'QUDO'
b$`Species`[b$`Species` == '818'] <- 'QUKE'
b$`Species`[b$`Species` == '839'] <- 'QUWI2'
b$`Species`[b$`Species` == '64'] <- 'JUOC'

b = b |>
  select(PlotID = Plot.,
         TreeID = tree_id,
         TreeNum = Tree.,
         Dist_ft,
         Az,
         DBH_cm,
         Ht_ft,
         Position,
         Crown,
         Defects,
         Decay,
         Notes) |>
  arrange(PlotID, TreeID) |>
  mutate(across(where(is.numeric), ~(as.character(round(.,1)))),
         across(where(is_character), ~ifelse(is.na(.), "", .))
         )

st_geometry(b) = NULL



write_csv(b, file.path(data_dir, "checking-veg-survey/checking-round2/batches-2-3-data-for-checking.csv"))


