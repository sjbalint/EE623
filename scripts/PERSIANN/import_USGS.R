rm(list = ls()) #clear environment

# import libraries --------------------------------------------------------

library(tidyverse)
library(dataRetrieval)
library(rsoi)

date.min <- as.Date("2000-01-01") #our beginning date
date.max <- Sys.Date() #our ending date, which is the current date

# import USGS data --------------------------------------------------------

#find sites
USGS.sites.df <- data.frame(
  site.id=c("01114000","01113895","01109403","01116500","01117000","01109060","01108410"),
  site.name=c("Providence River","Blackstone River","Ten Mile River",
             "Pawtuxet River","Hunt River", "Three Mile River", "Mill River"),
  site.region=c("Providence River","Providence River","Providence River","Providence River",
               "Mid Bay","Taunton Rover","Taunton River")
)

sitenumbers <- USGS.sites.df %>%
  pull(site.id) %>%
  unique()

siteNo <- "01114000" #providence river
pCode <- "00060" #discharge
start.date <- as.character(date.min)
end.date <- as.character(date.max)

USGS.df <- readNWISuv(siteNumbers = sitenumbers, parameterCd = pCode,
                   startDate = start.date, endDate = end.date)  %>%
  rename("discharge.ft3.sec"="X_00060_00000",
         "site.id"="site_no")

USGS.df$date <- as.Date(USGS.df$dateTime)

USGS.df <- USGS.df %>%
  group_by(date,site.id) %>%
  summarize(discharge.ft3.sec.day=sum(discharge.ft3.sec),
            count.15m=n()) %>%
  ungroup()

USGS.df$discharge.ft3.day <- USGS.df$discharge.ft3.sec.day*USGS.df$count.15m*900
USGS.df$discharge.m3.day <- USGS.df$discharge.ft3.day/35.3147
USGS.df$date <- as.Date(USGS.df$date)

USGS.df <- USGS.df %>%
  select(date,site.id,discharge.m3.day)

USGS.df <- left_join(USGS.df,USGS.sites.df)

saveRDS(USGS.df,file="Rdata/USGS.rds") #export the data


