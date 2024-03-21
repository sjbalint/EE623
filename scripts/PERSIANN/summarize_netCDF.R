rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(data.table) #for large datasets
library(progress)

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

  
  