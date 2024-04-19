# Author: Emily Marie Purvis
# Date: 4.18.2024
# Goal: combine all IRI batches

#### Load libraries ####
library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(pracma)

#### Combine coordinates into one KML ####

#### Batch 1 ####

# Set temporary working directory to save typing

setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\updated (9.11.23) data and output files\\Stem_Batch_1")

# Import plot coordinates

L506 <- read_sf('Post_Processed_GPS\\L-506_g\\L-506.shp')

L506 = L506 %>% mutate(`Plot#` = 'L506')

L506 <- L506 [-c(1:18, 21:22)]

# st_crs(L506)
# EPSG 4326

L511 <- read_sf('Post_Processed_GPS\\L-511_g_CORRECTED\\L511.shp')

L511 = L511 %>% mutate(`Plot#` = 'L511')

L511 <- L511 [-c(1:18, 21:22)]

# st_crs(L511)

L512 <- read_sf('Post_Processed_GPS\\L-512_g\\L-512.shp')

L512 = L512 %>% mutate(`Plot#` = 'L512')

L512 <- L512 [-c(1:18, 21:22)]

# st_crs(L512)

L515 <- read_sf('Post_Processed_GPS\\L-515_g\\L-515.shp')

L515 = L515 %>% mutate(`Plot#` = 'L515')

L515 <- L515 [-c(1:18, 21:22)]

# st_crs(L515)

L517 <- read_sf('Post_Processed_GPS\\L-517_g\\L-517.shp')

L517 = L517 %>% mutate(`Plot#` = 'L517')

L517 <- L517 [-c(1:19, 22:23)]

# st_crs(L517)

L525 <- read_sf('Post_Processed_GPS\\L-525_g\\L-525.shp')

L525 = L525 %>% mutate(`Plot#` = 'L525')

L525 <- L525 [-c(1:18, 21:22)]

# st_crs(L525)

S107 <- read_sf('Post_Processed_GPS\\S-107_g\\S-107.shp')

S107 = S107 %>% mutate(`Plot#` = 'S107')

S107 <- S107 [-c(1:19, 22:23)]

# st_crs(S107)

S109 <- read_sf('Post_Processed_GPS\\S-109_g\\S-109.shp')

S109 = S109 %>% mutate(`Plot#` = 'S109')

S109 <- S109 [-c(1:18, 21:22)]

# st_crs(S109)

S113 <- read_sf('Post_Processed_GPS\\S-113_g\\S-113.shp')

S113 = S113 %>% mutate(`Plot#` = 'S113')

S113 <- S113 [-c(1:18, 21:22)]

# st_crs(S113)

S118 <- read_sf('Post_Processed_GPS\\S-118_g\\S-118.shp')

S118 = S118 %>% mutate(`Plot#` = 'S118')

S118 <- S118 [-c(1:19, 22:23)]

# st_crs(S118)

Batch1plotsWGS84 <- rbind(L506, L511, L512, L515, L517, L525, S107, S109, S113, S118)

Batch1plotsWGS84 <- Batch1plotsWGS84 [-c(1:2)]

Batch1plotsUTM10N <- Batch1plotsWGS84 %>% st_transform(32610)

#### Batch 2 ####

# Set temporary working directory to save typing

setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\updated (9.11.23) data and output files\\Stem_Batch_2")

# Import plot coordinates

L502 <- read_sf('Post_Processed_GPS\\L-502_g\\L-502.shp')

L502 = L502 %>% mutate(`Plot#` = 'L502')

L502 <- L502 [-c(1:23)]

# st_crs(L502)
# EPSG 4326

L504 <- read_sf('Post_Processed_GPS\\L-504_g\\L-504.shp')

L504 = L504 %>% mutate(`Plot#` = 'L504')

L504 = L504 %>% st_set_crs(4326)

L504 <- L504 [-c(1:22)]

# not all shapefiles have a crs, so I'm checking manually and adding if necessary
# st_crs(L504)

L505 <- read_sf('Post_Processed_GPS\\L-505_g\\L-505.shp')

L505 = L505 %>% mutate(`Plot#` = 'L505')

L505 = L505 %>% st_set_crs(4326)

L505 <- L505 [-c(1:22)]

# st_crs(L505)

L507 <- read_sf('Post_Processed_GPS\\L-507_g\\L-507.shp')

L507 = L507 %>% mutate(`Plot#` = 'L507')

L507 <- L507 [-c(1:23)]

# st_crs(L507)

L508 <- read_sf('Post_Processed_GPS\\L-508_g\\L-508.shp')

L508 = L508 %>% mutate(`Plot#` = 'L508')

L508 <- L508 [-c(1:23)]

# st_crs(L508)

L521 <- read_sf('Post_Processed_GPS\\L-521_g\\L-521.shp')

L521 = L521 %>% mutate(`Plot#` = 'L521')

L521 = L521 %>% st_set_crs(4326)

L521 <- L521 [-c(1:22)]

# st_crs(L521)

L529 <- read_sf('Post_Processed_GPS\\L-529_g\\L-529.shp')

L529 = L529 %>% mutate(`Plot#` = 'L529')

L529 <- L529 [-c(1:22)]

# st_crs(L529)

L536 <- read_sf('Post_Processed_GPS\\L-536_g\\L-536.shp')

L536 = L536 %>% mutate(`Plot#` = 'L536')

L536 <- L536 [-c(1:23)]

# st_crs(L536)

S038 <- read_sf('Post_Processed_GPS\\S-038_g\\S-038.shp')

S038 = S038 %>% mutate(`Plot#` = 'S038')

S038 <- S038 [-c(1:23)]

# st_crs(S038)

S313 <- read_sf('Post_Processed_GPS\\S-313_g\\S-313.shp')

S313 = S313 %>% mutate(`Plot#` = 'S313')

S313 <- S313 [-c(1:22)]

# st_crs(S313)

Batch2plotsWGS84 <- rbind(L502, L504, L505, L507, L508, L521, L529, L536, S038, S313)

Batch2plotsUTM10N <- Batch2plotsWGS84 %>% st_transform(32610)

#### Batch 3 ####

setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\Post_Processed_GPS\\")

# Import plot coordinates

L519 <- read_sf('L-519_g\\L-519.shp')

L519 = L519 %>% mutate(`Plot#` = 'L519')

L519 <- L519 [-c(1:19, 22:23)]

# st_crs(L519)
# EPSG 4326

S015 <- read_sf('S-015_g\\S-015.shp')

S015 = S015 %>% mutate(`Plot#` = 'S015')

S015 <- S015 [-c(1:18, 21:22)]

# st_crs(S015)
# EPSG 4326

S074 <- read_sf('S-074_g\\S_074.shp')

S074 = S074 %>% mutate(`Plot#` = 'S074')

S074 <- S074 [-c(1:19, 22:23)]

# st_crs(S074)
# EPSG 4326

S075 <- read_sf('S-075_g\\S-075.shp')

S075 = S075 %>% mutate(`Plot#` = 'S075')

S075 <- S075 [-c(1:19, 22:23)]

# st_crs(S075)
# EPSG 4326

S080 <- read_sf('S-080_g\\S-080.shp')

S080 = S080 %>% mutate(`Plot#` = 'S080')

S080 <- S080 [-c(1:18, 21:22)]

# st_crs(S080)
S080 <- S080 %>% st_set_crs(4326)
# EPSG 4326

S083 <- read_sf('S-083_g\\S-083.shp')

S083 = S083 %>% mutate(`Plot#` = 'S083')

S083 <- S083 [-c(1:18, 21:22)]

# st_crs(S083)
S083 <- S083 %>% st_set_crs(4326)
# EPSG 4326

S084 <- read_sf('S-084_g\\S-084.shp')

S084 = S084 %>% mutate(`Plot#` = 'S084')

S084 <- S084 [-c(1:18, 21:22)]

# st_crs(S084)
S084 <- S084 %>% st_set_crs(4326)
# EPSG 4326

S087 <- read_sf('S-087_g\\S-087.shp')

S087 = S087 %>% mutate(`Plot#` = 'S087')

S087 <- S087 [-c(1:18, 21:22)]

# st_crs(S087)
S087 <- S087 %>% st_set_crs(4326)
# EPSG 4326

S097 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\rebatch3gpsfiles_s097\\S-097.shp')

S097 = S097 %>% mutate(`Plot#` = 'S097')

S097 <- S097 [-c(1:19, 22:23)]

# st_crs(S097)
# EPSG 4326

S973 <- read_sf('C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\rebatch3gpsfiles_s973\\S-973.shp')

S973 = S973 %>% mutate(`Plot#` = 'S973')

S973 <- S973 [-c(1:19, 22:23)]

# st_crs(S097)
# EPSG 4326

Batch3plotsWGS84 <- rbind(L519, S015, S074, S075, S080, S083, S084, S087, S097, S973)

Batch3plotsWGS84 <- Batch3plotsWGS84 [-c(1:2)]

Batch3plotsUTM10N <- Batch3plotsWGS84 %>% st_transform(32610)

Batch3plotsUTM10N <- Batch3plotsUTM10N [-c(1:2)]

#### Batch 4 ####

L522 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-522_g\\L-522.shp")

L522 <- L522 %>%
  add_column(`Plot#` = "L522")

L522 <- L522 [-c(1:18, 21:22)]

# check CRS
# st_crs(L522)
# EPSG 4326

L528 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-528_g\\L-528.shp")

L528 <- L528 %>%
  add_column(`Plot#` = "L528")

L528 <- L528 [-c(1:18, 21:22)]

# check CRS
# st_crs(L528)
# EPSG 4326

S091 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\S-091_g\\S-091.shp")

S091 <- S091 %>%
  add_column(`Plot#` = "S091")

S091 <- S091 [-c(1:18, 21:22)]

# check CRS
# st_crs(S091)
# EPSG 4326

S907 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\S-907_g\\S-907.shp")

S907 <- S907 %>%
  add_column(`Plot#` = "S907")

S907 <- S907 [-c(1:18, 21:22)]

# check CRS
# st_crs(S907)
# EPSG 4326

L524 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-524_g\\L-524.shp")

L524 <- L524 %>%
  add_column(`Plot#` = "L524")

L524 <- L524 [-c(1:18, 21:22)]

# check CRS
# st_crs(L524)
# there is no crs, manually checking geospatial data and adding the correct crs
L524 <- L524 %>% st_set_crs(4326)

L520 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-520_g\\L-520.shp")

L520 <- L520 %>%
  add_column(`Plot#` = "L520")

L520 <- L520 [-c(1:18, 21:22)]

# check CRS
# st_crs(L520)
# EPSG 4326

S305 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\S-305_g\\S-305.shp")

S305 <- S305 %>%
  add_column(`Plot#` = "S305")

S305 <- S305 [-c(1:18, 21:22)]

# check CRS
# st_crs(S305)
# EPSG 4326

L535 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L535_g\\L535.shp")

L535 <- L535 %>%
  add_column(`Plot#` = "L535")

L535 <- L535 [-c(1:18, 21:22)]

# check CRS
# st_crs(L535)
# EPSG 4326

L539 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L539_g\\L539.shp")

L539 <- L539 %>%
  add_column(`Plot#` = "L539")

L539 <- L539 [-c(1:18, 21:22)]

# check CRS
# st_crs(L539)
# EPSG 4326

L534 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Post_Processed_GPS\\L-534_g\\L-534.shp")

L534 <- L534 %>%
  add_column(`Plot#` = "L534")

L534 <- L534 [-c(1:18, 21:22)]

# check CRS
# st_crs(L534)
# EPSG 4326

# combine all the plot data into one spatial dataframe

Batch4plotsWGS84 <- rbind(L522, L528, S091, S907, L524, L520, S305, L535, L539, L534)

Batch4plotsWGS84 <- Batch4plotsWGS84 [-c(1:2)]

Batch4plotsUTM10N <- Batch4plotsWGS84 %>% st_transform(32610)

Batch4plotsUTM10N <- Batch4plotsUTM10N [-c(1:2)]

#### Batch 5 ####

L092 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\L-092\\S092.shp")

L092 <- L092 %>%
  add_column(`Plot#` = "L092")

L092 <- L092 [-c(1:18, 21:22)]

# check CRS
# st_crs(L092)
# EPSG 4326

S100 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S-100\\S-100.shp")

S100 <- S100 %>%
  add_column(`Plot#` = "S100")

S100 <- S100 [-c(1:19, 22:23)]

# check CRS
# st_crs(S100)
# EPSG 4326

S103 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S-103\\S-103.shp")

S103 <- S103 %>%
  add_column(`Plot#` = "S103")

S103 <- S103 [-c(1:19, 22:23)]

# check CRS
# st_crs(S103)
# EPSG 4326

S099 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S099\\S099.shp")

S099 <- S099 %>%
  add_column(`Plot#` = "S099")

S099 <- S099 [-c(1:18, 21:22)]

# check CRS
# st_crs(S099)
# EPSG 4326

S105 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S105\\S105.shp")

S105 <- S105 %>%
  add_column(`Plot#` = "S105")

S105 <- S105 [-c(1:18, 21:22)]

# check CRS
# st_crs(S105)
# EPSG 4326

S111 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S111\\S111.shp")

S111 <- S111 %>%
  add_column(`Plot#` = "S111")

S111 <- S111 [-c(1:18, 21:22)]

# check CRS
# st_crs(S111)
# EPSG 4326

S098 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S098\\S098.shp")

S098 <- S098 %>%
  add_column(`Plot#` = "S098")

S098 <- S098 [-c(1:18, 21:22)]

# check CRS
# st_crs(S098)
# EPSG 4326

S101 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S101\\S101.shp")

S101 <- S101 %>%
  add_column(`Plot#` = "S101")

S101 <- S101 [-c(1:18, 21:22)]

# check CRS
# st_crs(S101)
# EPSG 4326

S108 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S108\\S108.shp")

S108 <- S108 %>%
  add_column(`Plot#` = "S108")

S108 <- S108 [-c(1:18, 21:22)]

# check CRS
# st_crs(S108)
# EPSG 4326

S106 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Post_Processed_GPS\\S106\\S106.shp")

S106 <- S106 %>%
  add_column(`Plot#` = "S106")

S106 <- S106 [-c(1:18, 21:22)]

# check CRS
# st_crs(S106)
# EPSG 4326

# combine all the plot data into one spatial dataframe

Batch5plotsWGS84 <- rbind(L092, S100, S103, S099, S105, S111, S098, S101, S108, S106)

Batch5plotsWGS84 <- Batch5plotsWGS84 [-c(1:2)]

Batch5plotsUTM10N <- st_transform (Batch5plotsWGS84, 32610)

#### Batch 6 ####

S119 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S119\\S119.shp")

S119 <- S119 %>%
  add_column(`Plot#` = "S119")

S119 <- S119 [-c(1:18, 21:22)]

# check CRS
# st_crs(S119)
# EPSG 4326

S965 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S965\\S965.shp")

S965 <- S965 %>%
  add_column(`Plot#` = "S965")

S965 <- S965 [-c(1:19, 22:23)]

# check CRS
# st_crs(S965)
# EPSG 4326

S104 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S-104\\S-104.shp")

S104 <- S104 %>%
  add_column(`Plot#` = "S104")

S104 <- S104 [-c(1:19, 22:23)]

# check CRS
# st_crs(S104)
# EPSG 4326

S110 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S-110\\S-110.shp")

S110 <- S110 %>%
  add_column(`Plot#` = "S110")

S110 <- S110 [-c(1:19, 22:23)]

# check CRS
# st_crs(S110)
# EPSG 4326

S112 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S112\\S112.shp")

S112 <- S112 %>%
  add_column(`Plot#` = "S112")

S112 <- S112 [-c(1:18, 21:22)]

# check CRS
# st_crs(S112)
# EPSG 4326

S114 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S114\\S114.shp")

S114 <- S114 %>%
  add_column(`Plot#` = "S114")

S114 <- S114 [-c(1:18, 21:22)]

# check CRS
# st_crs(S114)
# EPSG 4326

S115 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S115\\S115.shp")

S115 <- S115 %>%
  add_column(`Plot#` = "S115")

S115 <- S115 [-c(1:18, 21:22)]

# check CRS
# st_crs(S115)
# EPSG 4326

S117 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S117\\S117.shp")

S117 <- S117 %>%
  add_column(`Plot#` = "S117")

S117 <- S117 [-c(1:18, 21:22)]

# check CRS
# st_crs(S117)
# EPSG 4326

S120 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S120\\S120.shp")

S120 <- S120 %>%
  add_column(`Plot#` = "S120")

S120 <- S120 [-c(1:18, 21:22)]

# check CRS
# st_crs(S120)
# EPSG 4326

S503 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Post_Processed_GPS\\S503\\S503.shp")

S503 <- S503 %>%
  add_column(`Plot#` = "S503")

S503 <- S503 [-c(1:18, 21:22)]

# check CRS
# st_crs(S503)
# EPSG 4326

# combine all the plot data into one spatial dataframe

Batch6plotsWGS84 <- rbind(S119, S965, S104, S110, S112, S114, S115, S117, S120, S503)

Batch6plotsWGS84 <- Batch6plotsWGS84 [-c(1:2)]

Batch6plotsUTM10N <- st_transform (Batch6plotsWGS84, 32610)

#### Batch 7 ####

S303 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S-303\\S-303.shp")

S303 <- S303 %>%
  add_column(`Plot#` = "S303")

S303 <- S303 [-c(1:19, 22:23)]

# check CRS
# st_crs(S303)
# EPSG 4326

S931 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S-931\\S-931.shp")

S931 <- S931 %>%
  add_column(`Plot#` = "S931")

S931 <- S931 [-c(1:19, 22:23)]

# check CRS
# st_crs(S931)
# EPSG 4326

S037 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S037\\S037.shp")

S037 <- S037 %>%
  add_column(`Plot#` = "S037")

S037 <- S037 [-c(1:19, 22:23)]

# check CRS
# st_crs(S037)
# EPSG 4326

S058 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S058\\S058.shp")

S058 <- S058 %>%
  add_column(`Plot#` = "S058")

S058 <- S058 [-c(1:19, 22:23)]

# check CRS
# st_crs(S058)
# EPSG 4326

S065 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S065\\S065.shp")

S065 <- S065 %>%
  add_column(`Plot#` = "S065")

S065 <- S065 [-c(1:19, 22:23)]

# check CRS
# st_crs(S065)
# EPSG 4326

S066 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S066\\S066.shp")

S066 <- S066 %>%
  add_column(`Plot#` = "S066")

S066 <- S066 [-c(1:19, 22:23)]

# check CRS
# st_crs(S066)
# EPSG 4326

S070 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S070\\S070.shp")

S070 <- S070 %>%
  add_column(`Plot#` = "S070")

S070 <- S070 [-c(1:19, 22:23)]

# check CRS
# st_crs(S070)
# EPSG 4326

S076 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S076\\S076.shp")

S076 <- S076 %>%
  add_column(`Plot#` = "S076")

S076 <- S076 [-c(1:19, 22:23)]

# check CRS
# st_crs(S076)
# EPSG 4326

S989 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S989\\S989.shp")

S989 <- S989 %>%
  add_column(`Plot#` = "S989")

S989 <- S989 [-c(1:19, 22:23)]

# check CRS
# st_crs(S989)
# EPSG 4326

S071 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Post_Processed_GPS\\S-071\\S-071.shp")

S071 <- S071 %>%
  add_column(`Plot#` = "S071")

S071 <- S071 [-c(1:19, 22:23)]

# check CRS
# st_crs(S071)
# EPSG 4326

# combine all the plot data into one spatial dataframe

Batch7plotsWGS84 <- rbind(S303, S931, S037, S058, S065, S066, S070, S076, S989, S071)

Batch7plotsWGS84 <- Batch7plotsWGS84 [-c(1:2)]

Batch7plotsUTM10N <- st_transform (Batch7plotsWGS84, 32610)

#### Batch 8 ####

setwd("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization")

S023 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S023\\S023.shp")

S023= S023 %>% mutate(Name = 'S023')

S023 <- S023 [-c(1:19, 22:23)]

# st_crs(S023)
# EPSG 4326

S028 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S028\\S028.shp")

S028= S028 %>% mutate(Name = 'S028')

S028 <- S028 [-c(1:19, 22:23)]

# st_crs(S028)
# EPSG 4326

S032 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S032\\S032.shp")

S032= S032 %>% mutate(Name = 'S032')

S032 <- S032 [-c(1:19, 22:23)]

# st_crs(S032)
# EPSG 4326

S079 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S079\\S079.shp")

S079= S079 %>% mutate(Name = 'S079')

S079 <- S079 [-c(1:19, 22:23)]

# st_crs(S079)
# EPSG 4326

S302 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S302\\S302.shp")

S302= S302 %>% mutate(Name = 'S302')

S302 <- S302 [-c(1:18, 21:22)]

# st_crs(S302)
# EPSG 4326

S308 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S308\\S308.shp")

S308= S308 %>% mutate(Name = 'S308')

S308 <- S308 [-c(1:18, 21:22)]

# st_crs(S308)
# EPSG 4326

S310 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S310\\S310.shp")

S310= S310 %>% mutate(Name = 'S310')

S310 <- S310 [-c(1:19, 22:23)]

# st_crs(S310)
# EPSG 4326

S903 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S-903\\S-903.shp")

S903= S903 %>% mutate(Name = 'S903')

S903 <- S903 [-c(1:19, 22:23)]

# st_crs(S903)
# EPSG 4326

S945 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S945\\S945.shp")

S945= S945 %>% mutate(Name = 'S945')

S945 <- S945 [-c(1:19, 22:23)]

# st_crs(S945)
# EPSG 4326

S960 <- read_sf("TNC.Stem_Batch_8\\Batch_8\\Post_Processed_GPS\\S-960\\S-960.shp")

S960= S960 %>% mutate(Name = 'S960')

S960 <- S960 [-c(1:19, 22:23)]

# st_crs(S960)
# EPSG 4326

# Combine all plots

Batch8plotsWGS84 <- rbind(S023, S028, S032, S079, S302, S308, S310, S903, S945, S960)

Batch8plotsWGS84 <- Batch8plotsWGS84 [-c(1:2)]

Batch8plotsUTM10N <- Batch8plotsWGS84 %>% st_transform(32610)

#### Batch 9 ####

S009 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S009\\S009.shp")

S009= S009 %>% mutate(Name = 'S009')

S009 <- S009 [-c(1:19, 22:23)]

# st_crs(S009)
# EPSG 4326

S030 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S030\\S030.shp")

S030= S030 %>% mutate(Name = 'S030')

S030 <- S030 [-c(1:19, 22:23)]

# st_crs(S030)
# EPSG 4326

S040 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S040\\S040.shp")

S040= S040 %>% mutate(Name = 'S040')

S040 <- S040 [-c(1:19, 22:23)]

# st_crs(S040)
# EPSG 4326

S048 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S048\\S048.shp")

S048= S048 %>% mutate(Name = 'S048')

S048 <- S048 [-c(1:19, 22:23)]

# st_crs(S048)
# EPSG 4326

S053 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S053\\S053.shp")

S053= S053 %>% mutate(Name = 'S053')

S053 <- S053 [-c(1:19, 22:23)]

# st_crs(S053)
# EPSG 4326

S062 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S062\\S062.shp")

S062= S062 %>% mutate(Name = 'S062')

S062 <- S062 [-c(1:19, 22:23)]

# st_crs(S062)
# EPSG 4326

S063 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S063\\S063.shp")

S063= S063 %>% mutate(Name = 'S063')

S063 <- S063 [-c(1:19, 22:23)]

# st_crs(S063)
# EPSG 4326

S073 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S073\\S073.shp")

S073= S073 %>% mutate(Name = 'S073')

S073 <- S073 [-c(1:19, 22:23)]

# st_crs(S073)
# EPSG 4326

S085 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S085\\S085.shp")

S085= S085 %>% mutate(Name = 'S085')

S085 <- S085 [-c(1:19, 22:23)]

# st_crs(S085)
# EPSG 4326

S092 <- read_sf("TNC.Stem_Batch_9\\Batch_9\\Post_Processed_GPS\\S092\\S092.shp")

S092= S092 %>% mutate(Name = 'S092')

S092 <- S092 [-c(1:19, 22:23)]

# st_crs(S092)
# EPSG 4326

# Combine all plots

Batch9plotsWGS84 <- rbind(S009, S030, S040, S048, S053, S062, S063, S073, S085, S092)

Batch9plotsWGS84 <- Batch9plotsWGS84 [-c(1:2)]

Batch9plotsUTM10N <- Batch9plotsWGS84 %>% st_transform(32610)

#### Batch 10 ####

S036 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S036\\S036.shp")

S036= S036 %>% mutate(Name = 'S036')

S036 <- S036 [-c(1:19, 22:23)]

# st_crs(S036)
# EPSG 4326

S042 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S042\\S042.shp")

S042= S042 %>% mutate(Name = 'S042')

S042 <- S042 [-c(1:19, 22:23)]

# st_crs(S042)
# EPSG 4326

S046 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S046\\S046.shp")

S046= S046 %>% mutate(Name = 'S046')

S046 <- S046 [-c(1:19, 22:23)]

# st_crs(S046)
# EPSG 4326

S051 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S051\\S051.shp")

S051= S051 %>% mutate(Name = 'S051')

S051 <- S051 [-c(1:19, 22:23)]

# st_crs(S051)
# EPSG 4326

S055 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S055\\S055.shp")

S055= S055 %>% mutate(Name = 'S055')

S055 <- S055 [-c(1:19, 22:23)]

# st_crs(S055)
# EPSG 4326

S089 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S089\\S089.shp")

S089= S089 %>% mutate(Name = 'S089')

S089 <- S089 [-c(1:19, 22:23)]

# st_crs(S089)
# EPSG 4326

S093 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S093\\S093.shp")

S093= S093 %>% mutate(Name = 'S093')

S093 <- S093 [-c(1:19, 22:23)]

# st_crs(S093)
# EPSG 4326

S314 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S314\\S314.shp")

S314= S314 %>% mutate(Name = 'S314')

S314 <- S314 [-c(1:19, 22:23)]

# st_crs(S314)
# EPSG 4326

S315 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S315\\S315.shp")

S315= S315 %>% mutate(Name = 'S315')

S315 <- S315 [-c(1:19, 22:23)]

# st_crs(S315)
# EPSG 4326

S982 <- read_sf("TNC.Stem_Batch_10\\Batch_10_final\\Post_Proccessed_GPS\\S982\\S982.shp")

S982= S982 %>% mutate(Name = 'S982')

S982 <- S982 [-c(1:19, 22:23)]

# st_crs(S982)
# EPSG 4326

# Combine all plots

Batch10plotsWGS84 <- rbind(S036, S042, S046, S051, S055, S089, S093, S314, S315, S982)

Batch10plotsWGS84 <- Batch10plotsWGS84 [-c(1:2)]

Batch10plotsUTM10N <- Batch10plotsWGS84 %>% st_transform(32610)

#### Batch 11 ####

# super important note: there are a handful of duplicated plots in this batch that also appeared in batch 7. S058, S065, S070, S076 were originally in batch 7 but were resent with batch 11. I'm not including the duplicate plots here!!

S001 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S001\\S001.shp")

S001= S001 %>% mutate(Name = 'S001')

S001 <- S001 [-c(1:19, 22:23)]

# st_crs(S001)
# EPSG 4326

S007 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S007\\S007.shp")

S007= S007 %>% mutate(Name = 'S007')

S007 <- S007 [-c(1:19, 22:23)]

# st_crs(S007)
# EPSG 4326

S008 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S008\\S008.shp")

S008= S008 %>% mutate(Name = 'S008')

S008 <- S008 [-c(1:19, 22:23)]

# st_crs(S008)
# EPSG 4326

S307 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S307\\S307.shp")

S307= S307 %>% mutate(Name = 'S307')

S307 <- S307 [-c(1:19, 22:23)]

# st_crs(S307)
# EPSG 4326

S505 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S505\\S505.shp")

S505= S505 %>% mutate(Name = 'S505')

S505 <- S505 [-c(1:19, 22:23)]

# st_crs(S505)
# EPSG 4326

S940 <- read_sf("TNC.Stem_Batch_11\\Batch_11_Final\\S0940\\S940.shp")

S940= S940 %>% mutate(Name = 'S940')

S940 <- S940 [-c(1:19, 22:23)]

# st_crs(S940)
# EPSG 4326

# Combine all plots

Batch11plotsWGS84 <- rbind(S001, S007, S008, S307, S505, S940)

Batch11plotsWGS84 <- Batch11plotsWGS84 [-c(1:2)]

Batch11plotsUTM10N <- Batch11plotsWGS84 %>% st_transform(32610)

#### Batch 12 ####

S021 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S021\\S021.shp")

S021= S021 %>% mutate(Name = 'S021')

S021 <- S021 [-c(1:19, 22:23)]

# st_crs(S021)
# EPSG 4326

S022  <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S022\\S022.shp")

S022= S022 %>% mutate(Name = 'S022')

S022 <- S022 [-c(1:19, 22:23)]

# st_crs(S022)
# EPSG 4326

S026 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S-026\\S-026.shp")

S026= S026 %>% mutate(Name = 'S026')

S026 <- S026 [-c(1:19, 22:23)]

# st_crs(S026)
# EPSG 4326

S034 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S-034\\S-034.shp")

S034= S034 %>% mutate(Name = 'S034')

S034 <- S034 [-c(1:19, 22:23)]

# st_crs(S034)
# EPSG 4326

S045 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S045\\S045.shp")

S045= S045 %>% mutate(Name = 'S045')

S045 <- S045 [-c(1:19, 22:23)]

# st_crs(S045)
# EPSG 4326

S061 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S061\\S061.shp")

S061= S061 %>% mutate(Name = 'S061')

S061 <- S061 [-c(1:19, 22:23)]

# st_crs(S061)
# EPSG 4326

S069 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S069\\S069.shp")

S069= S069 %>% mutate(Name = 'S069')

S069 <- S069 [-c(1:19, 22:23)]

# st_crs(S069)
# EPSG 4326

S096 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S096\\S-096.shp")

S096= S096 %>% mutate(Name = 'S096')

S096 <- S096 [-c(1:19, 22:23)]

# st_crs(S096)
# EPSG 4326

S507 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S507\\S507.shp")

S507= S507 %>% mutate(Name = 'S507')

S507 <- S507 [-c(1:19, 22:23)]

# st_crs(S507)
# EPSG 4326

S508 <- read_sf("TNC.Stem_Batch_12\\Batch_12_updated\\Post_Processed_GPS\\S508\\S-508.shp")

S508= S508 %>% mutate(Name = 'S508')

S508 <- S508 [-c(1:19, 22:23)]

# st_crs(S508)
# EPSG 4326

# Combine all plots

Batch12plotsWGS84 <- rbind(S021, S022, S026, S034, S045, S061, S069, S096, S507, S508)

Batch12plotsWGS84 <- Batch12plotsWGS84 [-c(1:2)]

Batch12plotsUTM10N <- Batch12plotsWGS84 %>% st_transform(32610)

#### Batch 13 ####

S003 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S003\\S003.shp")

S003= S003 %>% mutate(Name = 'S003')

S003 <- S003 [-c(1:19, 22:23)]

# st_crs(S003)
# EPSG 4326

S006 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S006\\S-006.shp")

S006= S006 %>% mutate(Name = 'S006')

S006 <- S006 [-c(1:19, 22:23)]

# st_crs(S006)
# EPSG 4326

S014 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S014\\S014.shp")

S014= S014 %>% mutate(Name = 'S014')

S014 <- S014 [-c(1:19, 22:23)]

# st_crs(S014)
# EPSG 4326

S044 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S044\\S044.shp")

S044= S044 %>% mutate(Name = 'S044')

S044 <- S044 [-c(1:19, 22:23)]

# st_crs(S044)
# EPSG 4326

S047 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S047\\S047.shp")

S047= S047 %>% mutate(Name = 'S047')

S047 <- S047 [-c(1:19, 22:23)]

# st_crs(S047)
# EPSG 4326

S054 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S054\\S054.shp")

S054= S054 %>% mutate(Name = 'S054')

S054 <- S054 [-c(1:19, 22:23)]

# st_crs(S054)
# EPSG 4326

S312 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S-312\\S-312.shp")

S312= S312 %>% mutate(Name = 'S312')

S312 <- S312 [-c(1:19, 22:23)]

# st_crs(S312)
# EPSG 4326

S316 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S316\\S316.shp")

S316= S316 %>% mutate(Name = 'S316')

S316 <- S316 [-c(1:19, 22:23)]

# st_crs(S316)
# EPSG 4326

S502 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S502\\S502.shp")

S502= S502 %>% mutate(Name = 'S502')

S502 <- S502 [-c(1:19, 22:23)]

# st_crs(S502)
# EPSG 4326

S935 <- read_sf("TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\GPS\\S935\\S935.shp")

S935= S935 %>% mutate(Name = 'S935')

S935 <- S935 [-c(1:19, 22:23)]

# st_crs(S935)
# EPSG 4326

# Combine all plots

Batch13plotsWGS84 <- rbind(S003, S006, S014, S044, S047, S054, S312, S316, S502, S935)

Batch13plotsWGS84 <- Batch13plotsWGS84 [-c(1:2)]

Batch13plotsUTM10N <- Batch13plotsWGS84 %>% st_transform(32610)

#### Batch 14 ####

S049 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\049\\Point_generic.shp")

S049= S049 %>% mutate(Name = 'S049')

S049 <- S049 [-c(1:19, 22:23)]

# st_crs(S049)
# EPSG 4326

S004 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S-004\\S-004.shp")

S004= S004 %>% mutate(Name = 'S004')

S004 <- S004 [-c(1:19, 22:23)]

# st_crs(S004)
# EPSG 4326

S010 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S-010\\S-010.shp")

S010= S010 %>% mutate(Name = 'S010')

S010 <- S010 [-c(1:19, 22:23)]

# st_crs(S010)
# EPSG 4326

S011 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S-011\\S-011.shp")

S011= S011 %>% mutate(Name = 'S011')

S011 <- S011 [-c(1:19, 22:23)]

# st_crs(S011)
# EPSG 4326

S013 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S013\\S013.shp")

S013= S013 %>% mutate(Name = 'S013')

S013 <- S013 [-c(1:19, 22:23)]

# st_crs(S013)
# EPSG 4326

S017 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S017\\S-017.shp")

S017= S017 %>% mutate(Name = 'S017')

S017 <- S017 [-c(1:19, 22:23)]

# st_crs(S017)
# EPSG 4326

S043 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S043\\S-043.shp")

S043 <- S043 [-1,]

S043= S043 %>% mutate(Name = 'S043')

S043 <- S043 [-c(1:19, 22:23)]

# st_crs(S043)
# EPSG 4326

S086 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S086\\S086_final.shp")

S086= S086 %>% mutate(Name = 'S086')

S086 <- S086 [-c(1:19, 22:23)]

# st_crs(S086)
# EPSG 4326

S311 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S311\\S-311.shp")

S311= S311 %>% mutate(Name = 'S311')

S311 <- S311 [-c(1:19, 22:23)]

# st_crs(S311)
# EPSG 4326

S506 <- read_sf("TNC.Stem_Batch_14\\Batch14\\Post_Processed_GPS\\S-506\\S506.shp")

S506= S506 %>% mutate(Name = 'S506')

S506 <- S506 [-c(1:18, 21:22)]

# st_crs(S506)
# EPSG 4326

# Combine all plots

Batch14plotsWGS84 <- rbind(S049, S004, S010, S011, S013, S017, S043, S086, S311, S506)

Batch14plotsWGS84 <- Batch14plotsWGS84 [-c(1:2)]

Batch14plotsUTM10N <- Batch14plotsWGS84 %>% st_transform(32610)

#### Batch 15 ####

S005 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-005\\S-005.shp")

S005= S005 %>% mutate(Name = 'S005')

S005 <- S005 [-c(1:19, 22:23)]

# st_crs(S005)
# EPSG 4326

S018 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-018_g\\S018.shp")

S018= S018 %>% mutate(Name = 'S018')

S018 <- S018 [-c(1:18, 21:22)]

# st_crs(S018)
# EPSG 4326

S020 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-020\\S-020.shp")

S020= S020 %>% mutate(Name = 'S020')

S020 <- S020 [-c(1:19, 22:23)]

# st_crs(S020)
# EPSG 4326

S024 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-024_g\\Point_generic.shp")

S024= S024 %>% mutate(Name = 'S024')

S024 <- S024 [-c(1:18, 21:22)]

# st_crs(S024)
# EPSG 4326

S041 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-041\\S-041.shp")

S041= S041 %>% mutate(Name = 'S041')

S041 <- S041 [-c(1:19, 22:23)]

# st_crs(S041)
# EPSG 4326

S309 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-309\\S-309.shp")

S309= S309 %>% mutate(Name = 'S309')

S309 <- S309 [-c(1:19, 22:23)]

# st_crs(S309)
# EPSG 4326

S501 <- read_sf("TNC.Stem_Batch_15\\Batch15\\Post_Processed_GPS\\S-501\\S-501.shp")

S501= S501 %>% mutate(Name = 'S501')

S501 <- S501 [-c(1:19, 22:23)]

# st_crs(S501)
# EPSG 4326

# Combine all plots

Batch15plotsWGS84 <- rbind(S005, S018, S020, S024, S041, S309, S501)

Batch15plotsWGS84 <- Batch15plotsWGS84 [-c(1:2)]

Batch15plotsUTM10N <- Batch15plotsWGS84 %>% st_transform(32610)

#### Batch L527 ####

L527 <- read_sf("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_L527\\L-527\\Post_Processed_GPS\\L527\\L527.shp")

L527= L527 %>% mutate(Name = 'L527')

L527 <- L527 [-c(1:19, 22:23)]

# st_crs(L527)
# EPSG 4326

BatchL527plotsWGS84 <- L527

BatchL527plotsWGS84 <- BatchL527plotsWGS84 [-c(1:2)]

BatchL527plotsUTM10N <- BatchL527plotsWGS84 %>% st_transform(32610)

#### Combining the geospatial files of all the plot centers ####

Batch1plotsWGS84 <- Batch1plotsWGS84 %>% rename (Name = 'Plot#')

Batch2plotsWGS84 <- Batch2plotsWGS84 %>% rename (Name = 'Plot#')

Batch3plotsWGS84 <- Batch3plotsWGS84 %>% rename (Name = 'Plot#')

Batch4plotsWGS84 <- Batch4plotsWGS84 %>% rename (Name = 'Plot#')

Batch5plotsWGS84 <- Batch5plotsWGS84 %>% rename (Name = 'Plot#')

Batch6plotsWGS84 <- Batch6plotsWGS84 %>% rename (Name = 'Plot#')

Batch7plotsWGS84 <- Batch7plotsWGS84 %>% rename (Name = 'Plot#')

combinedplotsWGS84 <- rbind(Batch1plotsWGS84, Batch2plotsWGS84, Batch3plotsWGS84, Batch4plotsWGS84, Batch5plotsWGS84, Batch6plotsWGS84, Batch7plotsWGS84, Batch8plotsWGS84, Batch9plotsWGS84, Batch10plotsWGS84, Batch11plotsWGS84, Batch12plotsWGS84, Batch13plotsWGS84, Batch14plotsWGS84, Batch15plotsWGS84, BatchL527plotsWGS84)

# st_write(combinedplotsWGS84, data("C:\\Users\\emily\\Desktop\\TNC Yuba\\IRI data\\allbatches_actualplotcenters_WGS84.kml"),delete_dsn=TRUE)

Batch1plotsUTM10N <- Batch1plotsUTM10N %>% rename (Name = 'Plot#')

Batch2plotsUTM10N <- Batch2plotsUTM10N %>% rename (Name = 'Plot#')

Batch3plotsUTM10N <- Batch3plotsUTM10N %>% rename (Name = 'Plot#')

Batch4plotsUTM10N <- Batch4plotsUTM10N %>% rename (Name = 'Plot#')

Batch5plotsUTM10N <- Batch5plotsUTM10N %>% rename (Name = 'Plot#')

Batch6plotsUTM10N <- Batch6plotsUTM10N %>% rename (Name = 'Plot#')

Batch7plotsUTM10N <- Batch7plotsUTM10N %>% rename (Name = 'Plot#')

combinedplotsUTM10N <- rbind(Batch1plotsUTM10N, Batch2plotsUTM10N, Batch3plotsUTM10N, Batch4plotsUTM10N, Batch5plotsUTM10N, Batch6plotsUTM10N, Batch7plotsUTM10N, Batch8plotsUTM10N, Batch9plotsUTM10N, Batch10plotsUTM10N, Batch11plotsUTM10N, Batch12plotsUTM10N, Batch13plotsUTM10N, Batch14plotsUTM10N, Batch15plotsUTM10N, BatchL527plotsUTM10N)

#### Now let's combine all the spreadsheets ####

Batch1trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_1\\updated (9.11.23) data and output files\\Stem_Batch_1\\Plot_Data\\StemData_Batch1_emp_edits.xlsx"),sheet=1,col_names = TRUE)

Batch2trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_2\\updated (9.11.23) data and output files\\Stem_Batch_2\\Plot_Data\\StemData_Batch2.xlsx"),sheet=1,col_names = TRUE)

Batch3trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_3\\StemData_Batch3.xlsx"),sheet=1,col_names = TRUE)

Batch4trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_4\\Stem_Batch_4\\Plot_Data\\StemData_Batch4.xlsx"),sheet=1,col_names = TRUE)

Batch5trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_5\\Batch_5\\Plot_Data\\Stem_Batch5.xlsx"),sheet=1,col_names = TRUE)

Batch6trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_6\\Batch_6\\Plot_Data\\StemData_Batch6.xlsx"),sheet=1,col_names = TRUE)

Batch7trees <- read_excel(data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_7\\Batch_7\\Plot_Data\\Stem_Batch7.xlsx"),sheet=1,col_names = TRUE)

Batch8trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_8\\Batch_8\\Plot_Data\\Stem_Batch_8.xlsx"),sheet=1,col_names = TRUE)

Batch9trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_9\\Batch_9\\Batch_9_Complete.xlsx"),sheet=1,col_names = TRUE)

Batch10trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_10\\Batch_10_final\\Batch_10_Final_Data.xlsx"),sheet=1,col_names = TRUE)

Batch11trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_11\\Batch_11_Final\\Batch_11.xlsx"),sheet=1,col_names = TRUE)

Batch12trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_12\\Batch_12_updated\\Plot_Data\\Stem_Batch12.xlsx"),sheet=1,col_names = TRUE)

Batch13trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_13\\Batch_13_Final_Geoff\\Batch13.xlsx"),sheet=1,col_names = TRUE)

Batch14trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_14\\Batch14\\Plot_Data\\Stem_Batch14_emp_edits.xlsx"),sheet=1,col_names = TRUE)

Batch15trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_15\\Batch15\\Plot_Data\\Stem_Batch15.xlsx"),sheet=1,col_names = TRUE)

BatchL527trees <- read_excel (data("C:\\Users\\emily\\Box\\FOCAL\\field-data-standardization\\TNC.Stem_Batch_L527\\L-527\\Data\\L-527.xlsx"),sheet=1,col_names = TRUE)

# add batch number to each spreadsheet just in case this info is helpful later

Batch1trees$Batch <- 1
Batch2trees$Batch <- 2
Batch3trees$Batch <- 3
Batch4trees$Batch <- 4
Batch5trees$Batch <- 5
Batch6trees$Batch <- 6
Batch7trees$Batch <- 7
Batch8trees$Batch <- 8
Batch9trees$Batch <- 9
Batch10trees$Batch <- 10
Batch11trees$Batch <- 11
Batch12trees$Batch <- 12
Batch13trees$Batch <- 13
Batch14trees$Batch <- 14
Batch15trees$Batch <- 15
BatchL527trees$Batch <- L527

# Rename columns so they all match

Batch4trees <- Batch4trees %>% rename(`Slope distance`=`Slope Dist`, `H Distance`= `H Dist`)

Batch3trees <- Batch3trees %>% rename(`Tree#`=`Tree #`)

Batch1trees <- Batch1trees %>% rename(`Plot #`=`Plot#`)

Batch2trees <- Batch2trees %>% rename(`Plot #`=`Plot#`)

Batch3trees <- Batch3trees %>% rename(`Plot #`=`Plot#`)

Batch4trees <- Batch4trees %>% rename(`Plot #`=`Plot#`)

Batch11trees <- Batch11trees %>% rename(`Slope distance`=`Slope Dist`, `H Distance`= `H Dist`)

Batch13trees <- Batch13trees %>% rename(`Slope distance`=`Slope Dist`, `H Distance`= `H Dist`)

Batch11trees <- Batch11trees %>% rename(`Plot #`=`Plot#`)

Batch13trees <- Batch13trees %>% rename(`Plot #`=`Plot#`)

# remove duplicated plots from batch 11 (S058, S065, S070, S076 )

Batch11trees <- Batch11trees[-(which(Batch11trees$`Plot #` %in% "S058")),]

Batch11trees <- Batch11trees[-(which(Batch11trees$`Plot #` %in% "S065")),]

Batch11trees <- Batch11trees[-(which(Batch11trees$`Plot #` %in% "S070")),]

Batch11trees <- Batch11trees[-(which(Batch11trees$`Plot #` %in% "S076")),]

# combine all tree datasets

combinedtrees <- rbind(Batch1trees, Batch2trees, Batch3trees, Batch4trees, Batch5trees, Batch6trees, Batch7trees, Batch8trees, Batch9trees, Batch10trees, Batch11trees, Batch12trees, Batch13trees, Batch14trees, Batch15trees, BatchL527trees)

# Rename columns to be more descriptive

combinedtrees <- combinedtrees %>% rename("PercentSlope" = `% slope`)

combinedtrees <- combinedtrees %>% rename(`Horizontal distance` = `H Distance`)

combinedtrees <- combinedtrees %>% rename(`Horizontal distance (feet)` = `Horizontal distance`)

combinedtrees <- combinedtrees %>% rename(`Slope distance (feet)` = `Slope distance`)

combinedtrees <- combinedtrees %>% rename(`Azimuth` = `AZM`)

combinedtrees <- combinedtrees %>% rename(`DBH (inches)` = `DBH`)

combinedtrees <- combinedtrees %>% rename(`Height (feet)` = `Actual HT`)

combinedtrees <- combinedtrees %>% rename(`CanopyPosition` = `Position`)

# need to delete random rows of all NAs in treedata

combinedtrees <- combinedtrees %>% drop_na(`Plot #`)

combinedtrees <- combinedtrees %>% drop_na(`Tree#`)

#### Add a complete horizontal distance column ####

# some of the trees were measured with horizontal distance, some with slope + slope distance, some with both. we want all trees to have horizontal distance.

# create a blank column for horizontal distance

combinedtrees$`All Horizontal Distances (feet)` <- 0

# if horizontal distance is NA, then calculate it from percent slope and slope distance, then put that value in the horizontal distance column. if horizontal distance is not NA, use the value the crew measured

combinedtrees$`Slope distance (feet)` <- as.numeric(combinedtrees$`Slope distance (feet)`)

combinedtrees$`PercentSlope` <- as.numeric(combinedtrees$`PercentSlope`)

for(i in 1:nrow(combinedtrees)) {
  if(is.na(combinedtrees$`Horizontal distance (feet)`[i])) {
    combinedtrees[i,]$`All Horizontal Distances (feet)` = (combinedtrees[i,]$`Slope distance (feet)`)*cos(atan(combinedtrees[i,]$`PercentSlope`/100))
  }
  else {
    combinedtrees[i,]$`All Horizontal Distances (feet)` = combinedtrees[i,]$`Horizontal distance (feet)`
  }
}

#### Add plot coordinates to tree dataframe ####

combinedplotsWGS84 <- combinedplotsWGS84 %>% rename (`Plot #`=Name)

# extract the geometry features into columns of lat and long

# extract coordinates

combinedplotsWGS84 <- st_zm(combinedplotsWGS84)

combinedplotsWGS84_coordinates <- data.frame(combinedplotsWGS84$`Plot #`, st_coordinates(combinedplotsWGS84[,1], st_coordinates(combinedplotsWGS84[,2])))

combinedplotsUTM10N <- st_zm(combinedplotsUTM10N)

combinedplotsUTM10N_coordinates <- data.frame(combinedplotsUTM10N$Name, st_coordinates(combinedplotsUTM10N[,1], st_coordinates(combinedplotsUTM10N[,2])))

# rename new columns

combinedplotsWGS84_coordinates <- combinedplotsWGS84_coordinates %>% rename (`Plot #`=combinedplotsWGS84..Plot..., PlotLongitudeWGS84=X, PlotLatitudeWGS84=Y)

combinedplotsUTM10N_coordinates <- combinedplotsUTM10N_coordinates %>% rename (`Plot #`= combinedplotsUTM10N.Name, PlotLongitudeUTM10N=X, PlotLatitudeUTM10N=Y)

# make sure all plot names are in the same format

# Batch 1

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-506'] <- 'L506'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-511'] <- 'L511'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-512'] <- 'L512'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-515'] <- 'L515'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-517'] <- 'L517'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-525'] <- 'L525'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-107'] <- 'S107'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-109'] <- 'S109'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-113'] <- 'S113'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-118'] <- 'S118'

# Batch 2

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-521'] <- 'L521'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-505'] <- 'L505'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-504'] <- 'L504'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-536'] <- 'L536'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-507'] <- 'L507'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-502'] <- 'L502'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-038'] <- 'S038'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-529'] <- 'L529'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-508'] <- 'L508'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-313'] <- 'S313'

# Batch 3

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-519'] <- 'L519'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-087'] <- 'S087'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-084'] <- 'S084'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-080'] <- 'S080'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-083'] <- 'S083'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-097'] <- 'S097'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-038'] <- 'S038'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-075'] <- 'S075'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-973'] <- 'S973'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-015'] <- 'S015'

# Batch 4

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-522'] <- 'L522'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-528'] <- 'L528'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-091'] <- 'S091'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-907'] <- 'S907'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-524'] <- 'L524'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-520'] <- 'L520'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-305'] <- 'S305'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-535'] <- 'L535'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-539'] <- 'L539'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-534'] <- 'L534'

# Batch 5

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-092'] <- 'L092'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-100'] <- 'S100'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-103'] <- 'S103'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-099'] <- 'S099'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-105'] <- 'S105'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-111'] <- 'S111'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-098'] <- 'S098'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-101'] <- 'S101'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-108'] <- 'S108'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-106'] <- 'S106'

# Batch 6

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-119'] <- 'S119'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-965'] <- 'S965'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-104'] <- 'S104'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-110'] <- 'S110'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-112'] <- 'S112'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-114'] <- 'S114'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-115'] <- 'S115'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-117'] <- 'S117'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-120'] <- 'S120'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-503'] <- 'S503'

# Batch 7

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-303'] <- 'S303'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-931'] <- 'S931'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-037'] <- 'S037'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-058'] <- 'S058'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-065'] <- 'S065'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-066'] <- 'S066'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-070'] <- 'S070'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-076'] <- 'S076'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-989'] <- 'S989'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-071'] <- 'S071'

# Batch 8

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-023'] <- 'S023'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-028'] <- 'S028'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-032'] <- 'S032'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-079'] <- 'S079'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-302'] <- 'S302'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-308'] <- 'S308'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-310'] <- 'S310'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-903'] <- 'S903'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-945'] <- 'S945'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-960'] <- 'S960'

# Batch 9

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-009'] <- 'S009'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-030'] <- 'S030'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-040'] <- 'S040'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-048'] <- 'S048'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-053'] <- 'S053'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-062'] <- 'S062'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-063'] <- 'S063'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-073'] <- 'S073'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-085'] <- 'S085'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-092'] <- 'S092'

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 's053'] <- 'S053'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 's063'] <- 'S063'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 's048'] <- 'S048'

# Batch 10

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-036'] <- 'S036'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-042'] <- 'S042'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-046'] <- 'S046'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-051'] <- 'S051'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-055'] <- 'S055'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-089'] <- 'S089'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-093'] <- 'S093'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-314'] <- 'S314'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-315'] <- 'S315'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-982'] <- 'S982'

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 's093'] <- 'S093'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 's982'] <- 'S982'

# Batch 11

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-001'] <- 'S001'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-007'] <- 'S007'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-008'] <- 'S008'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-307'] <- 'S307'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-505'] <- 'S505'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-940'] <- 'S940'

# Batch 12

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-021'] <- 'S021'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-022'] <- 'S022'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-026'] <- 'S026'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-034'] <- 'S034'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-045'] <- 'S045'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-061'] <- 'S061'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-069'] <- 'S069'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-096'] <- 'S096'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-507'] <- 'S507'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-508'] <- 'S508'

# Batch 13

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-003'] <- 'S003'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-006'] <- 'S006'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-014'] <- 'S014'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-044'] <- 'S044'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-047'] <- 'S047'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-054'] <- 'S054'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-312'] <- 'S312'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-316'] <- 'S316'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-502'] <- 'S502'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-935'] <- 'S935'

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 's054'] <- 'S054'

# Batch 14

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-049'] <- 'S049'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-004'] <- 'S004'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-010'] <- 'S010'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-011'] <- 'S011'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-013'] <- 'S013'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-017'] <- 'S017'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-043'] <- 'S043'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-086'] <- 'S086'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-311'] <- 'S311'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-506'] <- 'S506'

# Batch 15

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-005'] <- 'S005'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-018'] <- 'S018'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-020'] <- 'S020'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-024'] <- 'S024'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-041'] <- 'S041'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-309'] <- 'S309'
combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'S-501'] <- 'S501'

# Batch L527

combinedtrees$`Plot #`[combinedtrees$`Plot #` == 'L-527'] <- 'L527'

# merge plot coordinates into tree data frame

combinedtrees <- full_join(combinedtrees, combinedplotsWGS84_coordinates, by="Plot #")

combinedtrees <- full_join(combinedtrees, combinedplotsUTM10N_coordinates, by="Plot #")

#### Tree coordinates ####

# Make new columns for tree coordinates

combinedtrees$`TreeLongitudeUTM10N` <- 0

combinedtrees$`TreeLatitudeUTM10N` <- 0

## Calculate tree coordinates in UTMs

# Longitude UTM10N

for(i in 1:nrow(combinedtrees)) {
  combinedtrees[i,]$`TreeLongitudeUTM10N` = (combinedtrees[i,]$PlotLongitudeUTM10N) + ((sin(deg2rad(combinedtrees[i,]$Azimuth))) * ((combinedtrees[i,]$`All Horizontal Distances (feet)`) * 0.3048))
}

# Latitude UTM10N

for(i in 1:nrow(combinedtrees)) {
  combinedtrees[i,]$`TreeLatitudeUTM10N` = (combinedtrees[i,]$PlotLatitudeUTM10N) + ((cos(deg2rad(combinedtrees[i,]$Azimuth))) * ((combinedtrees[i,]$`All Horizontal Distances (feet)`) * 0.3048))
}

#### Convert to WGS84 Coordinates ####

# give trees ID numbers

combinedtrees = combinedtrees %>% mutate(ofo_tree_id = 1:nrow(combinedtrees))

# remove trees without lat/lon --> this happens when there is no horizontal distance AND either/or no percent slope/slope distance, so a horizontal distance cannot be calculated

# fortunately there are only six trees in this whole dataset of 4969 that don't have a horizontal distance, five in batch 2 (an ABCO in plot L536, an ABCO in plot L502, two LIDE3 and one ACMA3 in L529) and one in batch 4 (a PILA in plot L535)

combinedtrees <- combinedtrees %>% drop_na(`TreeLongitudeUTM10N`)

# make a spatial data frame

treecoordinatesconversion <- data.frame(combinedtrees$ofo_tree_id, combinedtrees$TreeLongitudeUTM10N, combinedtrees$TreeLatitudeUTM10N)

treecoordinatesconversion <- treecoordinatesconversion %>% rename (ofo_tree_id=combinedtrees.ofo_tree_id, TreeLongitudeUTM10N=combinedtrees.TreeLongitudeUTM10N, TreeLatitudeUTM10N=combinedtrees.TreeLatitudeUTM10N)

treecoordinatesconversion <- st_as_sf(treecoordinatesconversion, coords = c("TreeLongitudeUTM10N", "TreeLatitudeUTM10N"), crs = 32610, remove=F)

# change the CRS

treecoordinatesconversion <- treecoordinatesconversion %>% st_transform(4326)

# extract the geometry features into columns of lat and long WGS84, then merge back into the tree data frame

treecoordinatesconversionWGS84 <- data.frame(treecoordinatesconversion$ofo_tree_id, st_coordinates(treecoordinatesconversion[,1], st_coordinates(treecoordinatesconversion[,2])))

treecoordinatesconversionWGS84 <- treecoordinatesconversionWGS84 %>% rename (ofo_tree_id=treecoordinatesconversion.ofo_tree_id, TreeLongitudeWGS84=X, TreeLatitudeWGS84=Y)

combinedtrees <- full_join (combinedtrees, treecoordinatesconversionWGS84, by="ofo_tree_id")

# save tree coordinates for later

treecoordinatesconversion <- treecoordinatesconversion[-c(2:3)]

treeinfoforkml <- select(combinedtrees, c('ofo_tree_id','Plot #','Tree#')) %>% rename ('IRI_tree_id'= 'Tree#')

treecoordinates <- full_join (treecoordinatesconversion, treeinfoforkml, by= 'ofo_tree_id')

st_write(treecoordinates, data("C:\\Users\\emily\\Desktop\\TNC Yuba\\IRI data\\allbatches_treecoordinates_WGS84.kml"),delete_dsn=TRUE)

#### Convert DBH from inches to cm in a new column ####

# add new column

combinedtrees$`DBH (cm)` <- 0

# calculate

combinedtrees$`DBH (cm)` = combinedtrees$`DBH (inches)` * 2.54

#### Add new column of species names (in addition to codes) ####

combinedtrees$`Species_name` <- 0

combinedtrees$`Species_name`[combinedtrees$`Species` == '122'] <- 'PIPO'
combinedtrees$`Species_name`[combinedtrees$`Species` == '112'] <- 'PIPO' # 112 is pinus engelmannii, almost certainly a typo
combinedtrees$`Species_name`[combinedtrees$`Species` == '212'] <- 'PIPO' # 212 is giant sequoia, almost certainly a typo
combinedtrees$`Species_name`[combinedtrees$`Species` == '15'] <- 'ABCO'
combinedtrees$`Species_name`[combinedtrees$`Species` == '20'] <- 'ABMA'
combinedtrees$`Species_name`[combinedtrees$`Species` == '117'] <- 'PILA'
combinedtrees$`Species_name`[combinedtrees$`Species` == '101'] <- 'PIAL'
combinedtrees$`Species_name`[combinedtrees$`Species` == '119'] <- 'PIMO3'
combinedtrees$`Species_name`[combinedtrees$`Species` == '108'] <- 'PICOL'
combinedtrees$`Species_name`[combinedtrees$`Species` == '81'] <- 'CADE27'
combinedtrees$`Species_name`[combinedtrees$`Species` == '202'] <- 'PSME'
combinedtrees$`Species_name`[combinedtrees$`Species` == '127'] <- 'PISA2'
combinedtrees$`Species_name`[combinedtrees$`Species` == '116'] <- 'PIJE'
combinedtrees$`Species_name`[combinedtrees$`Species` == '103'] <- 'PIAT'
combinedtrees$`Species_name`[combinedtrees$`Species` == '361'] <- 'ARME'
combinedtrees$`Species_name`[combinedtrees$`Species` == '631'] <- 'LIDE3'
combinedtrees$`Species_name`[combinedtrees$`Species` == '312'] <- 'ACMA3' # Bigleaf Maple
combinedtrees$`Species_name`[combinedtrees$`Species` == '981'] <- 'UMCA'
combinedtrees$`Species_name`[combinedtrees$`Species` == '333'] <- 'AECA'
combinedtrees$`Species_name`[combinedtrees$`Species` == '805'] <- 'QUCH2'
combinedtrees$`Species_name`[combinedtrees$`Species` == '807'] <- 'QUDO'
combinedtrees$`Species_name`[combinedtrees$`Species` == '818'] <- 'QUKE'
combinedtrees$`Species_name`[combinedtrees$`Species` == '188'] <- 'QUKE'# another typo I'm guessing at the right answer for-- 188 isn't a code for anything
combinedtrees$`Species_name`[combinedtrees$`Species` == '816'] <- 'QUKE' # 816 is quercus ilicifolia which only grows in the eastern US so almost certainly a typo. this is a guess.
combinedtrees$`Species_name`[combinedtrees$`Species` == '839'] <- 'QUWI2'
combinedtrees$`Species_name`[combinedtrees$`Species` == '64'] <- 'JUOC'
combinedtrees$`Species_name`[combinedtrees$`Species` == '768'] <- 'PREM'
combinedtrees$`Species_name`[combinedtrees$`Species` == '21'] <- 'ABMAS' # Shasta Red Fir
combinedtrees$`Species_name`[combinedtrees$`Species` == '313'] <- 'ACNE2' # Box Elder
combinedtrees$`Species_name`[combinedtrees$`Species` == '492'] <- 'CONU4'
combinedtrees$`Species_name`[combinedtrees$`Species` == '999'] <- 'Unknown'
combinedtrees$`Species_name`[combinedtrees$`Species` == '9'] <- 'Unknown'
combinedtrees$`Species_name`[is.na(combinedtrees$`Species`)] <- "Unknown"
combinedtrees$`Species_name`[combinedtrees$`Species` == '231'] <- 'TABR2' #Pacific yew

#### Convert damage codes into words####

combinedtrees$`Live Tree Defects`[combinedtrees$`Live Tree Defects` == '0'] <- 'no damage'

combinedtrees$`Live Tree Defects`[is.na(combinedtrees$`Live Tree Defects`)] <- ""

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`,  "90001", "broken top")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90002", "dead top")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90004", "forked top")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90005", "forked below merch top")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90006", "crook or sweep")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "9006", "crook or sweep")

combinedtrees$`Live Tree Defects` <- str_replace(combinedtrees$`Live Tree Defects`, "90011", "open wound")

#### Convert status into live/dead ####

combinedtrees$`Live/Dead`[combinedtrees$`Live/Dead` == '1'] <- 'Live'

combinedtrees$`Live/Dead`[combinedtrees$`Live/Dead` == '11'] <- 'Live' # an obvious typo

combinedtrees$`Live/Dead`[combinedtrees$`Live/Dead` == '2'] <- 'Dead'

#### Correcting typos in the damage codes ####

# Weird but helpful for me: all the trees with a Live Tree Defect code of 3 (which isn't a real code) already have that exact code in the adjacent canopy position column. These trees are all alive. I'm just deleting the code from the Live Tree Defect column.

combinedtrees$`Live Tree Defects`[combinedtrees$`Live Tree Defects` == '3'] <- ''

combinedtrees$`Live Tree Defects`[combinedtrees$`Live Tree Defects` == ']'] <- ''

#### Add horizontal distance in meters ####

combinedtrees$`All Horizontal Distances (meters)` <- combinedtrees$`All Horizontal Distances (feet)` * 0.3048

#### Save tree datasheet ####

combinedtrees_allincludinglargeplots <- apply(combinedtrees, 2, as.character)

write.csv(combinedtrees_allincludinglargeplots, "C:\\Users\\emily\\Desktop\\TNC Yuba\\IRI data\\alltrees_includinglargeplots.csv")

combinedtrees_smallplotsonly <- combinedtrees[-which(combinedtrees$'Plot #' %in% "L092"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L502"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L504"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L505"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L506"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L507"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L508"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L511"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L512"),]

# combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L515"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L517"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L519"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L520"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L521"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L522"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L524"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L525"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L527"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L528"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L529"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L534"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L535"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L536"),]

combinedtrees_smallplotsonly <- combinedtrees_smallplotsonly[-which(combinedtrees_smallplotsonly$'Plot #' %in% "L539"),]

combinedtrees_smallplotsonly <- apply(combinedtrees_smallplotsonly, 2, as.character)

write.csv(combinedtrees_smallplotsonly, "C:\\Users\\emily\\Desktop\\TNC Yuba\\IRI data\\alltrees_smallplotsonly.csv")
