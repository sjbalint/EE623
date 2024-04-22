rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(zoo) #for rollapply

source("functions/stats_functions.R")

# load data ---------------------------------------------------------------

persiann.df <- readRDS("Rdata/daily.rds") %>%
  mutate(year=format(date, "%Y"),
         prcp.mm=perform_IQR(prcp.mm),
         PERSIANN=prcp.mm/10)%>%
  select(date, year, PERSIANN)

noaa.df <- readRDS("Rdata/NOAA_prcp.rds") %>%
  mutate(year=format(date, "%Y"),
         NOAA=prcp.mm/10) %>%
  select(date, year, NOAA)


# assess correlation ------------------------------------------------------

rolling.list <- (1:365)

result.list <- list()

for (days in rolling.list){
  
  df <- left_join(persiann.df,noaa.df, by = join_by(date, year)) %>%
    filter(year>2018) %>%
    mutate(PERSIANN=rollsum(PERSIANN, k=days, na.pad = TRUE, align="center", na.rm=TRUE),
           NOAA=rollsum(NOAA, k=days, na.pad = TRUE, align="center", na.rm=TRUE)
           #season=get_season(date)
           )
  
  #ggplot(df, aes(PERSIANN, NOAA, color=season))+geom_point()
    
    rsq <- summary(lm(PERSIANN~NOAA, data=df))$r.squared
    
    temp.df <- data.frame("days"=days, "rsq"=rsq)
    
    result.list <- append(result.list, list(temp.df))
  
}

result.df <- bind_rows(result.list)

# graph the results -------------------------------------------------------

ggplot(result.df, aes(days, rsq))+
  geom_point()
