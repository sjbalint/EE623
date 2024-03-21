rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(ncdump)
library(data.table)
library(progress) #to keep us sane

# make a function to import netCDF ----------------------------------------

#DEBUGGING each file is one month
#netcdf_file <- "raw/netCDF/PDIR_basins_l47050039170_2024-03-15072255am_201807.nc"

#DEBUGGING get metadata
#NetCDF(netcdf_file)

read_netCDF <- function(netcdf_file){

  start_time <- substring(NetCDF(netcdf_file)$unlimdims$units, 13)
  
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
  datetime <- as.POSIXct(paste0(start_time, ":00:00"), tz = "UTC", 
                         origin = paste0(start_time, ":00:00")) + hours * 3600
  
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


# import all the netCDFs --------------------------------------------------

file.list <- list.files(path = "raw/netCDF", pattern = "*.nc", recursive=TRUE, full.names=TRUE)

n_iter <- length(file.list) #figure out how long it will run

pb <- progress_bar$new(format = "(:spin) [:bar] :percent [:elapsed || :eta]",
                       total = n_iter,complete = "=",incomplete = "-",current = ">",
                       clear = FALSE, width = 100, show_after=0)
pb$tick(0)

for (file in file.list){
  
  start_time <- substring(NetCDF(file)$unlimdims$units, 13, 19)
  
  nc.dt <- read_netCDF(file) %>%
    as.data.table
  
  saveRDS(nc.dt, file=paste0("Rdata/netCDF_output/",start_time,".rds"))
  
  pb$tick() 
}

pb$terminate()