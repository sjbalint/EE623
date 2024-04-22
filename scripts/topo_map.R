rm(list = ls()) #clear environment

# install packages --------------------------------------------------------

devtools::install_github("eliocamp/ggnewscale") #development version

library(tidyverse) #for everything
library(sf) #for all mapping stuff
library(metR) #for contour functions. better than ggplot
library(ggspatial) #for north arrow, distance scale, etc.
library(ggnewscale) #for separate fill scales by layer
library(ggsci) #for color pallets
library(cowplot) #for ggdraw (inset)

packageVersion("ggnewscale")

# some global stuff -------------------------------------------------------

#coordinate for main map
xmin=-72
xmax=-70.3
ymin=41.2
ymax=42.4

sf_use_s2(FALSE)

# import topography data --------------------------------------------------

#topobathy of narragansett bay region
load("Rdata/GIS/narragansett_topobathy.Rdata")
load("Rdata/GIS/newengland_topobathy.Rdata")

topobathy.df <- topobathy.df %>%
  filter(between(x, xmin-0.1, xmax+0.1),
         between(y, ymin-0.1, ymax+0.1))

bathy.df <- topobathy.df %>%
  filter(z<20)

topo.df <- topobathy.df %>%
  filter(z>0)

# import other stuff ------------------------------------------------------

#import census data
load("Rdata/GIS/census_RI.Rdata")
load("Rdata/GIS/census_USA.Rdata")
load("Rdata/GIS/RI_roads.Rdata")
load("Rdata/GIS/major_roads.Rdata")

load("Rdata/GIS/watershed.Rdata")

#import buoy stations
#locations.df <- readRDS("Rdata/location_info.rds") %>%
 # filter(station!="PB")

#convert to sf object
#stations.sf <- st_as_sf(locations.df,coords=c("longitude.degrees","latitude.degrees"),crs="+proj=longlat +datum=WGS84")

#remove NOAA
#stations.sf <- stations.sf %>%
 # filter(type=="Water Quality") %>%
  #mutate(region_3=factor(region_3, levels=c("EXCLUDED", "Upper Bay", "Mid Bay", "Lower Bay")))

# configure_graphing ------------------------------------------------------

#colors we want
land_color="antiquewhite"

mytheme <- list(
  theme_bw(),
  scale_shape_manual(values=c(21:26)),
  labs(x=NULL, y=NULL),
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(fill="white",colour = "black"),
        panel.grid=element_blank()
        #panel.background = element_rect(fill = water_color)
        )
)

#theme for the map inset. this just removes the axis ticks
insettheme <- list(
  theme(plot.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
)


# make a map of narragansett bay ------------------------------------------

p1a <- ggplot() +
  mytheme+
  #new_scale_fill()+
  #geom_contour_fill(data = bathy.df, aes(x=x, y=y, z=z),
                    #binwidth=5)+
  #scale_fill_viridis_c(option="mako")+
  
  new_scale_fill()+
  geom_contour_fill(data = topo.df, aes(x=x, y=y, z=z),
                    binwidth=20, show.legend=FALSE, alpha=0.5)+
  scale_fill_viridis_c(option="rocket", direction = -1)+
  geom_sf(data=RI.sf, color="black", fill=NA)+
  
  new_scale_fill()+
  geom_sf(data=watershed.sf, aes(fill=Basins), alpha=0.8, show.legend=FALSE)+
  #geom_sf(data=stations.sf,aes(shape=region_3,fill=region_3),size=4, alpha=0.9)+
  #geom_sf_text(data=stations.sf,aes(label=station),size=5,nudge_x = 0.001,nudge_y = 0.001)+
  scale_fill_nejm()+
  
  annotation_scale(location = "br", width_hint = 0.5, text_col="black") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position=c(0.85,0.85))+
  #labs(fill="Region",shape="Region")+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)

# make a map of new england -----------------------------------------------

p2 <- ggplot() +
  mytheme+
  geom_sf(data=USA.sf, fill="grey")+
  geom_rect(aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill = NA, color = "black",linewidth = 0.5)+
  coord_sf(xlim = c(-74, -69.5), ylim = c(44, 40.5), expand = FALSE)

# combine plots -----------------------------------------------------------

final <- ggdraw() +
  draw_plot(p1a) +
  draw_plot(p2+insettheme,
            x=.95,
            y=.95,
            scale=0.25,
            hjust = 1,
            vjust = 1,
            halign = 1,
            valign = 1)

#to save time, the plot does not render by default.
#final

ggsave(final, file="figures/bathy_map.png")


# bigger map --------------------------------------------------------------

ggplot() +
  mytheme+
  geom_contour_fill(data = NE_topo.df, aes(x=x, y=y, z=z),
                    binwidth=50, show.legend = FALSE, color="black", linewidth=0.1)+
  scale_fill_divergent(low=NA, high="black")+
  geom_sf(data=USA.sf, fill="black", alpha=0.2)+
  geom_sf(data=USA.sf, fill=NA, color="black")+
  geom_rect(aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill = NA, color = "red",linewidth = 0.5)+
  coord_sf(xlim = c(-75, -68), ylim = c(44, 40.5), expand = FALSE)

ggsave(file="figures/new_england_map_2.png")
