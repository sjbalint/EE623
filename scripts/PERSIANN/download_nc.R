rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(ncdf4)
library(ncdump)
library(gganimate) #for animated figures
library(gifski) #for GIF
library(magick) #also for GIF
library(data.table)

# configuration stuff -----------------------------------------------------

#coordinate for main map
xmin=-72
xmax=-70.5
ymin=41.2
ymax=42.5

sf_use_s2(FALSE)

# load data ---------------------------------------------------------------

#load("Rdata/GIS/census_RI.Rdata")
#load("Rdata/GIS/watershed.Rdata")

# load netcdf -------------------------------------------------------------

#first file
#each file is one month
netcdf_file <- "raw/netCDF/PDIR_basins_l47050039170_2024-03-15072255am_201807.nc"

#get metadata
NetCDF(netcdf_file)

read_netCDF <- function(netcdf_file){
  
  #extract the data we want
  nc <- nc_open(netcdf_file)
  
  # spatial components
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  #temporal component
  hours <- ncvar_get(nc, "datetime")
  #precipitation data
  prcp <- ncvar_get(nc, "precip")
  
  #convert datetime to an actual time
  datetime <- as.POSIXct("2000-03-01 00:00:00", tz = "UTC", 
                         origin = "2000-03-01 00:00:00") + hours * 3600
  
  result.list <- list()
  
  for (i in 1:length(datetime)) {
    
    temp.df = data.frame(lon,prcp[,,i] %>% as.data.frame()) %>% 
      as.tibble()
    
    each <- length(lon)
    
    temp.df <- temp.df %>%
      gather(key = "key", value = "prcp", 2:ncol(temp.df)) %>%
      mutate(datetime = datetime[i], lat = rep(lat, each = each)) %>% 
      select(datetime, lon, lat, prcp)
    
    result.list <- append(result.list, list(temp.df))
  }
  
  prcp.df <- bind_rows(result.list)%>%
    as.data.table()
  
  return(prcp.df)
}

nc.dt <- #new dataframe of our raw data
  list.files(path = "raw/netCDF", pattern = "*.nc", recursive=TRUE, full.names=TRUE) %>% #list the filenames of every excel document oin the folder
  map_df(~read_netCDF(.)) %>%
  as.data.table()

saveRDS(nc.dt, file="Rdata/netCDF.rds")


# plot a subset of the data -----------------------------------------------

plot.df <- prcp.df %>%
  filter(datetime==as.POSIXct("2000-03-01 12:00")) %>%
  filter(prcp!=-99)

ggplot()+
  theme_bw()+
  geom_tile(data = plot.df,
              aes(x = lon, y = lat, fill = prcp))+
  geom_sf(data=RI.sf, fill=NA)+
  scale_fill_distiller(palette="RdYlBu")+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)+
  labs(x=NULL, y=NULL, fill="Precip.\n(mm/hr)")

