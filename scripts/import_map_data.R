rm(list = ls()) #clear environment

# install packages --------------------------------------------------------

library(tidyverse) #for everything
library(marmap) #for noaa bathy
library(mapdata) #for basemaps
library(sf) #for all mapping stuff
library(tigris) #for census data

options(tigris_use_cache = TRUE)

sf_use_s2(FALSE)

# load census data --------------------------------------------------------

RI.sf <- states() %>%
  filter(STUSPS %in% c('RI', 'MA', "CT")) %>%
  erase_water(area_threshold = 0.9)

save(RI.sf, file="Rdata/GIS/census_RI.Rdata")

RI_roads.sf <- primary_secondary_roads(state=c("RI"))
MA_roads.sf <- primary_secondary_roads(state=c("MA"))
CT_roads.sf <- primary_secondary_roads(state=c("CT"))

RI_roads.sf <- rbind_tigris(RI_roads.sf, MA_roads.sf, CT_roads.sf)

save(RI_roads.sf, file="Rdata/GIS/RI_roads.Rdata")

USA.sf <- states()

save(USA.sf, file="Rdata/GIS/census_USA.Rdata")

major_roads.sf <- primary_roads()

save(major_roads.sf, file="Rdata/GIS/major_roads.Rdata")

# load topo data ----------------------------------------------------------

#set coordinates
xmin=-72
xmax=-70
ymin=41
ymax=42.5

bathy <- getNOAA.bathy(lon1 = xmin, lon2 = xmax, lat1 = ymin, lat2 = ymax, resolution = 0.01)

# convert bathymetry to data frame
topobathy.df = fortify.bathy(bathy)

save(topobathy.df,file="Rdata/GIS/narragansett_topobathy.Rdata")

#set coordinates
xmin=-76
xmax=-67
ymin=39
ymax=45

NE_topo <- getNOAA.bathy(lon1 = xmin, lon2 = xmax, lat1 = ymin, lat2 = ymax, resolution = 1)

# convert bathymetry to data frame
NE_topo.df = fortify.bathy(NE_topo)

save(NE_topo.df,file="Rdata/GIS/newengland_topobathy.Rdata")

# load shapefile boundaries -----------------------------------------------

<<<<<<< HEAD
watershed <- read_sf("Rdata/GIS/nb_watershed/")
=======
watershed.sf <- read_sf("raw/nb_watershed/")

save(watershed.sf,file="Rdata/GIS/watershed.Rdata")
>>>>>>> 91a0e3269bb358e840af6d13767437173cb274d0

