
library(tidyverse)
library(sf)
library(gganimate) #for animated figures
library(gifski) #for GIF
library(magick) #also for GIF
library(data.table)


# load data ---------------------------------------------------------------

prcp.df <- readRDS("Rdata/netCDF_output/2018-07.rds")

load("Rdata/GIS/census_RI.Rdata")
load("Rdata/GIS/watershed.Rdata")

# configuration stuff -----------------------------------------------------

sf_use_s2(FALSE)

#coordinate for main map
xmin=-72
xmax=-70.5
ymin=41.2
ymax=42.5

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

