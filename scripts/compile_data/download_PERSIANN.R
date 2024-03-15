rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(R.utils)
library(rasterVis)
library(grid)
library(scales)


# configuration stuff -----------------------------------------------------

#coordinate for main map
xmin=-72
xmax=-70
ymin=41
ymax=42.5

sf_use_s2(FALSE)

# load data ---------------------------------------------------------------

load("Rdata/GIS/census_RI.Rdata")

r <- raster(xmn=0, xmx=360, ymn=-60, ymx=60, nrow=3000, ncol=9000, crs="+proj=longlat +datum=WGS84")
values(r) <- 1
r <- writeRaster(r, "raw/raster/test.grd", datatype="FLT4S", overwrite=TRUE)
x <- readLines("raw/raster/test.grd")
x[grep("byteorder", x)] <- "byteorder=big"
x[grep("nodatavalue", x)] <- "nodatavalue=-9999"
writeLines(x, "raw/raster/test.grd")

#load the data
url <- "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN-CCS/daily/rgccs1d03001.bin.gz"
gzf <- paste0("raw/raster/",basename(url))
download.file(url, gzf)
gunzip(gzf, overwrite=TRUE)

f <- gsub("\\.gz$", "", gzf)
file.rename(f, extension(f, "gri"))
fg <- extension(f, "grd")
file.copy("raw/raster/test.grd", fg, overwrite=TRUE)

r <- raster(fg) * 1
r <- rotate(r, filename=paste0(fg), overwrite=TRUE)

e <- extent(xmin, xmax, ymin, ymax)
rc <- crop(r, e)

rc.spdf <- as(rc, "SpatialPixelsDataFrame")
rc.df <- as.data.frame(rc.spdf)
colnames(rc.df) <- c("rainfall_mm", "x", "y")

rc.df <- rc.df %>%
  filter(rainfall_mm>0)

# looks ok
ggplot()+
  theme_classic()+
  geom_tile(data=rc.df, aes(x=x, y=y, fill=rainfall_mm))+
  geom_sf(data=RI.sf, fill=NA)+
  scale_fill_distiller(palette="RdYlBu")+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)

