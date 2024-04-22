rm(list = ls()) #clear environment

# import libraries --------------------------------------------------------

library(tidyverse)


# import data -------------------------------------------------------------

df <- read.csv("raw/NOAA_prcp.csv")

colnames(df) <- c("station","name","latitude.degrees","longitude.degrees",
                  "elevation.m","date","prcp.mm","snow.mm","temperature.c")

df <- df %>%
  select(latitude.degrees,longitude.degrees,date,prcp.mm,snow.mm,temperature.c) %>%
  mutate(date = as.Date(date))

saveRDS(df,file="Rdata/NOAA_prcp.rds") #export the data
