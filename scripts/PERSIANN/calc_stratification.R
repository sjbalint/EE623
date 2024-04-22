rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse) #for data manipulation
library(marelac) #for density calculation

# load data ---------------------------------------------------------------

df <- readRDS(file="Rdata/highres_buoy.rds")

wide.df <- df %>%
  mutate(year=format(datetime.est, "%Y"),
         density.kgm3 = sw_dens(S=salinity.ppt, t=temp.c, method="UNESCO")) %>%
  select(datetime.est, station, depth, density.kgm3) %>%
  group_by(datetime.est, station, depth) %>%
  summarize_all(mean, na.rm=TRUE) %>%
  pivot_wider(names_from="depth", values_from="density.kgm3") %>%
  ungroup()

strat.df <- wide.df %>%
  mutate(strat.kgm3=bottom-surface) %>%
  select(datetime.est, station, strat.kgm3) %>%
  drop_na()

strat.df <- strat.df %>%
  mutate(date=as.Date(datetime.est)) %>%
  group_by(station, date) %>%
  summarize(strat.kgm3=mean(strat.kgm3, na.rm=TRUE)) %>%
  ungroup()

saveRDS(strat.df, file="Rdata/stratification.rds")
