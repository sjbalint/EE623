rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(zoo) #for rollapply
library(ggpmisc) #for stats

source("functions/stats_functions.R")

days_roll <- 365

# load data ---------------------------------------------------------------

persiann.df <- readRDS("Rdata/basin.rds") %>%
  group_by(Basins) %>%
  mutate(prcp.mm=perform_IQR(prcp.mm),
         PERSIANN=prcp.mm/10)%>%
  select(date, Basins, PERSIANN)

noaa.df <- readRDS("Rdata/NOAA_prcp.rds") %>%
  mutate(NOAA=prcp.mm/10) %>%
  select(date, NOAA)

df <- left_join(persiann.df,noaa.df) %>%
  pivot_longer(c("PERSIANN", "NOAA"), names_to="data", values_to="prcp.cm") %>%
  group_by(data, Basins) %>%
  mutate(prcp.cm=rollsum(prcp.cm, k=days_roll, na.pad = TRUE, align="right", na.rm=TRUE),
         season=get_season(date)) %>%
  ungroup()


# graphing theme ----------------------------------------------------------

myheight <- 5
mywidth <- 5

mytheme <- list(
  theme_classic(),
  labs(subtitle=paste0(days_roll,"-day rolling average")),
  scale_color_viridis_d(option="mako", end=0.7),
  theme(legend.position="top",
        legend.title = element_blank())
)

# make a graph ------------------------------------------------------------

plot.df <- df %>%
  pivot_wider(names_from="data", values_from="prcp.cm")

ggplot(plot.df, aes(NOAA, PERSIANN, color=Basins))+
  mytheme+
  geom_point(shape=21)+
  geom_smooth(method="lm", color="red")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE, vstep=0, color="black", label.y = "top", na.rm=TRUE)+
  labs(x="NOAA precipitation (cm)",
       y="PERSIANN precipitation (cm)")

ggsave("figures/method_regression.png", width=mywidth, height=myheight)

plot.df <- df 

ggplot(plot.df, aes(date, prcp.cm, color=Basins, linetype=data))+
  mytheme+
  geom_line(alpha=0.7)+
  #facet_wrap(.~season)+
  labs(x=NULL, color=NULL, y="1-year rolling average precipitation (cm/yr)")

ggsave("figures/rolling_annual.png", width=mywidth, height=myheight)

plot.df <- noaa.df %>%
  mutate(prcp.cm=rollsum(NOAA, k=days_roll, na.pad = TRUE, align="right", na.rm=TRUE),
         season=get_season(date),
         year=format(date, "%Y"),
         data="NOAA")

ggplot(plot.df, aes(date, prcp.cm))+
  mytheme+
  geom_line(alpha=0.7)+
  geom_smooth(data=subset(plot.df, year>1950), method="lm", color="red")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE, vstep=0, color="black", label.y = "top", na.rm=TRUE)+
  #facet_wrap(.~season)+
  labs(x=NULL, color=NULL, y="1-year rolling average precipitation (cm/yr)")

ggsave("figures/NOAA_annual.png", width=mywidth, height=myheight)
