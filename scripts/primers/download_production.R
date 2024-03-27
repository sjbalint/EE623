rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(scales) #for log scale
library(ggsci) #for colors

sf_use_s2(FALSE)

# import data -------------------------------------------------------------

df <- read.table("raw/primers/marine_production.txt", sep = "\t", header=TRUE, fill=TRUE) %>%
  drop_na(c("Longitude","Latitude")) %>%
  rename("Prod.mgCm2d" = "Depth.integrated.primary.production..mg.C.m..2.day..1.",
         "distance.km"="Distance.from.coastline..Km.",
         "season"="Northern.hemisphere.season") %>%
  mutate(hemisphere=factor(hemisphere, levels=c("northern","equator","southern"),
                           labels=c("Northern","Equator","Southern")),
         season=factor(season, levels=c("winter","spring","summer","fall"),
                       labels=c("Winter","Spring","Summer","Fall")))

sf <- df %>%
  st_as_sf(coords=c("Longitude","Latitude"),crs="+proj=longlat +datum=WGS84")


wmap <- read_sf(dsn="Rdata/GIS/naturalearth/ne_110m_admin_0_countries", layer="ne_110m_admin_0_countries") %>%
  st_transform(crs="+proj=robin")

grat <- read_sf("Rdata/GIS/naturalearth/ne_110m_graticules_all", layer="ne_110m_graticules_15") %>%
  st_transform(crs="+proj=robin")

bbox <- read_sf("Rdata/GIS/naturalearth/ne_110m_graticules_all", layer="ne_110m_wgs84_bounding_box")%>%
  st_transform(crs="+proj=robin")




# configure graphing ------------------------------------------------------

mywidth <- 6
myheight <- 4

mytheme <- list(
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        #plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=22),
        legend.position="top",
        legend.title.position = "top")
  )

# make map ----------------------------------------------------------------

ggplot() + 
  mytheme+
  geom_sf(data=bbox, linetype="dashed", color="grey50")+
  geom_sf(data=grat, linetype="dashed", color="grey50") +
  geom_sf(data=wmap, fill="black", color="white")+
  geom_sf(data=sf, shape=21, aes(fill=Prod.mgCm2d))+
  geom_sf(data=sf, shape=21, aes(fill=Prod.mgCm2d, size=Prod.mgCm2d), show.legend=FALSE)+
  scale_fill_distiller(palette="RdYlBu", trans = "log10",
                       breaks=trans_breaks('log10', function(x) 10^x),
                       labels=trans_format('log10', math_format(10^.x)),
                       guide=guide_colorbar(barwidth=20))+
  labs(fill=bquote("Production"~"(mgC/"*m^2*"/d)"))

ggsave("figures/primers/map1.png", width=mywidth, height=myheight)

plot.df <- df %>%
  arrange(hemisphere)

ggplot(plot.df, aes(distance.km, Prod.mgCm2d))+
  theme_bw()+
  geom_point(aes(fill=hemisphere, shape=hemisphere), alpha=0.7)+
  geom_smooth(method="lm", se=FALSE, color="black", linewidth=2)+
  scale_y_log10(labels=trans_format('log10', math_format(10^.x)))+
  theme(legend.position="top",
        legend.title=element_blank())+
  scale_fill_viridis_d(option="cividis", begin=0.3)+
  scale_shape_manual(values=c(21:25))+
  labs(y=bquote("Production"~"(mgC/"*m^2*"/d)"),
       x="Distance from coast (km)")

ggsave("figures/primers/distancefromcoast.png", width=mywidth, height=myheight)


plot.df <- df %>%
  filter(hemisphere=="Northern")

ggplot(plot.df, aes(season, Prod.mgCm2d))+
  theme_bw()+
  geom_violin(aes(fill=season), alpha=0.5, show.legend=FALSE)+
  geom_boxplot(width=0.1, outlier.shape=NA)+
  scale_y_log10(labels=trans_format('log10', math_format(10^.x)))+
  theme(legend.position="top",
        legend.title=element_blank())+
  scale_fill_viridis_d(option="cividis",)+
  scale_shape_manual(values=c(21:25))+
  labs(y=bquote("Production"~"(mgC/"*m^2*"/d)"),
       x="Season")

ggsave("figures/primers/season.png", width=mywidth, height=myheight)




        