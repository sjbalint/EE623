rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(progress)
library(sf)
library(data.table) #for large datasets
library(tidyverse)

sf_use_s2(FALSE)

# import all the netCDFs --------------------------------------------------

file.list <- list.files(path = "Rdata/netCDF_output/", pattern = "*.rds", recursive=TRUE, full.names=TRUE)

n_iter <- length(file.list) #figure out how long it will run

pb <- progress_bar$new(format = "(:spin) [:bar] :percent [:elapsed || :eta]",
                       total = n_iter,complete = "=",incomplete = "-",current = ">",
                       clear = FALSE, width = 100, show_after=0)
pb$tick(0)

result.list <- list()

for (file in file.list){
  
  df <- readRDS(file)
  
  df <- df %>%
    filter(prcp>=0) %>%
    mutate(date=as.Date(datetime)) %>%
    group_by(date, lat, lon) %>%
    summarize(prcp.mm=sum(prcp, na.rm=TRUE), .groups="keep") %>%
    ungroup()
  
  result.list <- append(result.list, list(df))
  
  pb$tick() 
}

pb$terminate()

daily.df <- bind_rows(result.list)

saveRDS(daily.df, file="Rdata/daily_nc.rds")


# summarize by basin ------------------------------------------------------

daily.df <- readRDS("Rdata/daily_nc.rds")

load("Rdata/GIS/watershed.Rdata")

nc.sf <- st_as_sf(daily.df, coords = c("lon", "lat"), crs = 4326)

watershed.sf <- st_transform(watershed.sf, crs = 4326)

watershed.list <- watershed.sf %>%
  pull(Basins) %>%
  unique()

result.list <- list()

for (watershed in watershed.list){
  print(watershed)
  
  basin.sf <- watershed.sf %>%
    filter(Basins==watershed)
  
  nc_crop.sf <- st_intersection(nc.sf, basin.sf) %>%
    mutate(Basins=watershed)
  
  nc.df <- st_drop_geometry(nc_crop.sf)
  
  saveRDS(nc.df,file=paste0("Rdata/cropped_netCDF",watershed,".rds"))
  
  result.list <- append(result.list, list(nc.df))
  
}

basins.df <- bind_rows(result.list)

watershed.df <- st_drop_geometry(watershed.sf) %>%
  dplyr::select(Basins, Sq_Km)

basins.df <- left_join(basins.df, watershed.df)

saveRDS(basins.df, file="Rdata/basin_nc.rds")

# summarize by basin ------------------------------------------------------

basins.df <- basins.df %>%
  group_by(date, Basins, Sq_Km) %>%
  summarize(prcp.mm=mean(prcp.mm, na.rm=TRUE)) %>%
  ungroup()

saveRDS(basins.df, file="Rdata/basin.rds")

# summarize by entire area ------------------------------------------------

daily.df <- daily.df %>%
  group_by(date) %>%
  summarize(prcp.mm=mean(prcp.mm, na.rm=TRUE)) %>%
  ungroup()

saveRDS(daily.df, file="Rdata/daily.rds")


# summarize by year -------------------------------------------------------

yearly.df <- daily.df %>%
  mutate(year=format(date, "%Y")) %>%
  group_by(year) %>%
  summarize(prcp.mm=sum(prcp.mm, na.rm=TRUE)) %>%
  ungroup()

saveRDS(yearly.df, file="Rdata/yearly.rds")
