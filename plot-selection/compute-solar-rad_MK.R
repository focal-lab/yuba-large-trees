# Purpose: compute solar radiation following Pierce et al 2005 (code adapted from Kevin McGarigal)

library(sf)
library(terra)
library(here)
library(suncalc)

# get the root of the local data directory
datadir = readLines("data-dir.txt")

## set up the paths to the files that we will use
dem_path = file.path(datadir, "spatial/intermediate/dem.tif")
focal_area_path = file.path(datadir, "/spatial/raw/north_yuba_area.kml") # I couldn't get the layer embedded in the GPKG to open properly in R, so I had to use QGIS to export to KML so we can open it in R


# Load DEM
dem = rast(dem_path)

# Load focal area
focal_area = st_read(focal_area_path)


##### Begin code to compute solar rad

# Compute slope and aspect in radians
slope <- terrain(dem,v="slope",unit="radians")
aspect <- terrain(dem,v="aspect",unit="radians")

# Get average latitude and longitude for the reference region
#     Note, must reproject to latlong to get latitude and longitude
geo.prj="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
reference_aoi <- st_transform(focal_area,4326)
cent <- st_centroid(reference_aoi)
lat <- as.vector(ext(cent))[3] #latitude
lon <- as.vector(ext(cent))[1] #longitude

# Create vector of dates for growing season
grow_dates<-c('04/15/2021','05/15/2021','06/15/2021','07/15/2021','08/15/2021','09/15/2021')
grow_dates<-as.Date(grow_dates,format='%m/%d/%y')
grow_dates<-julian(grow_dates)
grow_dates<-as.Date(grow_dates,origin=as.Date("1970-01-01"))

# Calculate sunrise/sunset for growing season days
solar_period<-getSunlightTimes(date=grow_dates,lat=lat,lon=lon,
                               keep=c('sunrise','sunset'),tz='US/Pacific')

# Compute solar period for each day
solar_period$solar_period<-as.numeric(solar_period$sunset-solar_period$sunrise)

# # Create empty raster
# sri <- dem
# sri <- app(sri, fun=function(x) { ifelse(x>=0, 0, NA) })

### Loop thru each growing season day (midpoint of the growing-season months) and compute SRI
for(i in 1:nrow(solar_period)){
  
  #select day
  day <- solar_period[i,]
  
  #find 10 equal spaced times between sunrise and sunset
  z <- day$solar_period/11
  
  #loop thru times of day  
  for(j in 1:10){
    
    print(paste0('processing day ',i,' of ',nrow(solar_period),' time ',j,' of 10'))
    
    #compute day-time
    day_time <- day$sunrise+(z*j*60*60)
    
    #getSunlightPosition
    sunlight_position <- getSunlightPosition(date=day_time, lat=lat, lon=lon)
    
    #convert altitude and azimuth to degrees
    sunlight_position$angle <- sunlight_position$altitude*(180/pi)
    sunlight_position$direction <- sunlight_position$azimuth*(180/pi)
    
    #rotate direction by 180 degrees so that 0 is north
    sunlight_position$direction <- sunlight_position$direction+180
    
    #compute shade raster
    shade <- shade(slope=slope, aspect=aspect, angle=sunlight_position$angle, direction=sunlight_position$direction,
                   normalize=TRUE)
    
    if(j == 1) { 
      shade_stack = shade
    } else {
      shade_stack <- c(shade_stack, shade)
    }
  }
  
  # Sum across the 10 timepoints for the day to get daily rad
  day_sum <- sum(shade_stack, na.rm=TRUE)
  
  # Multiply by day solar_period
  day_sri <- day_sum * day$solar_period
  
  # Stack across days
  if(i == 1) {
    sri_stack = day_sri
  } else {
    sri_stack = c(sri_stack, day_sri)
  }
  
  
}

# Sum across days to get growing-season SRI sum
sri_tot = sum(sri_stack, na.rm=TRUE)

# Write
writeRaster(sri_tot, file.path(datadir, "spatial/intermediate/sri.tif"), overwrite=TRUE)
