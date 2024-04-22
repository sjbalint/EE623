rm(list = ls()) #clear environment

# import libraries --------------------------------------------------------

library(tidyverse)
library(ggpointdensity) #to color the points

# import data -------------------------------------------------------------



source("functions/stats_functions.R")

strat.df <- readRDS("Rdata/stratification.rds") %>%
  filter(station=="PD") %>%
  filter(chl.ugl.max>0) %>%
  mutate(d7.chl=rollmean(chl.ugl.max, k=7, fill=NA, align="right", na.rm=TRUE),
         d30.chl=rollmean(chl.ugl.max, k=30, fill=NA, align="right", na.rm=TRUE),
         d90.chl=rollmean(chl.ugl.max, k=90, fill=NA, align="right", na.rm=TRUE),
         
         d7.do=rollmean(do.mgl.min, k=7, fill=NA, align="right", na.rm=TRUE),
         d30.do=rollmean(do.mgl.min, k=30, fill=NA, align="right", na.rm=TRUE),
         d90.do=rollmean(do.mgl.min, k=90, fill=NA, align="right", na.rm=TRUE),
         
         d7.strat=rollmean(strat.kgm3, k=7, fill=NA, align="right", na.rm=TRUE),
         d30.strat=rollmean(strat.kgm3, k=30, fill=NA, align="right", na.rm=TRUE),
         d90.strat=rollmean(strat.kgm3, k=90, fill=NA, align="right", na.rm=TRUE))%>%
  mutate(season=get_season(date))%>%
  filter(season=="Summer")

usgs.df <- readRDS("Rdata/USGS.rds") %>%
  ungroup() %>%
  filter(site.name=="Blackstone River") %>%
  select(date, discharge.m3.day) %>%
  mutate(d7.discharge=rollsum(discharge.m3.day, k=7, fill=NA, align="right", na.rm=TRUE),
         d30.discharge=rollsum(discharge.m3.day, k=30, fill=NA, align="right", na.rm=TRUE),
         d90.discharge=rollsum(discharge.m3.day, k=90, fill=NA, align="right", na.rm=TRUE))

persiann.df <- readRDS("Rdata/basin.rds") %>%
  filter(Basins=="Blackstone River") %>%
  mutate(prcp.mm=perform_IQR(prcp.mm),
         prcp.cm=prcp.mm/10,
         d7.prcp=rollsum(prcp.cm, k=7, fill=NA, align="right", na.rm=TRUE),
         d30.prcp=rollsum(prcp.cm, k=60, fill=NA, align="right", na.rm=TRUE),
         d90.prcp=rollsum(prcp.cm, k=90, fill=NA, align="right", na.rm=TRUE))

df <- left_join(persiann.df, strat.df)

df <- left_join(df, usgs.df)

# graphing theme ----------------------------------------------------------

myheight <- 5
mywidth <- 5

mytheme <- list(
  theme_classic(),
  scale_color_viridis_c(option="mako", end=0.7),
  theme(legend.position="top",
        legend.title = element_blank())
)


# simple regression -------------------------------------------------------

plot.df <- df %>%
  filter(season=="Summer")

ggplot(plot.df, aes(strat.kgm3, do.mgl.min))+
  mytheme+
  geom_pointdensity(shape=21, show.legend=FALSE)+
  geom_smooth(method="lm", color="red")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE, vstep=0, color="black", label.y = "top", label.x="right", na.rm=TRUE)+
  labs(x=bquote("Water column stratification (kg/"*m^3*")"),
       y="Minimum bottom DO (mg/L)")

ggsave("figures/do_strat_regression.png", width=mywidth, height=myheight)

hist(df$strat.kgm3)

plot.df <- df %>%
  mutate(chl.ugl.max=perform_IQR(chl.ugl.max),
         discharge.m3.day=perform_IQR(discharge.m3.day))

ggplot(plot.df, aes(chl.ugl.max, do.mgl.min))+
  mytheme+
  geom_pointdensity(shape=21, show.legend=FALSE)+
  geom_smooth(method="lm", color="red")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE, vstep=0, color="black", label.y = "top", label.x="right", na.rm=TRUE)+
  labs(x=bquote("Maximum Surface Chlorophyll ("*mu*"g/L)"),
       y="Minimum bottom DO (mg/L)")

ggsave("figures/do_chl_regression.png", width=mywidth, height=myheight)

ggplot(plot.df, aes(discharge.m3.day, strat.kgm3))+
  mytheme+
  geom_pointdensity(shape=21, show.legend=FALSE)+
  geom_smooth(method="lm", color="red")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE, vstep=0, color="black", label.y = "top", label.x="right", na.rm=TRUE)+
  labs(x=bquote("River Discharge ("*m^3*"/day)"),
       y=bquote("Stratification (kg/"*m^3*")"))

ggsave("figures/discharge_strat_regression.png", width=mywidth, height=myheight)


# usgs and precip ---------------------------------------------------------

annual.df <- df %>%
  mutate(year=as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarize(prcp.cm=sum(prcp.cm, na.rm=TRUE),
            discharge.m3.day=sum(discharge.m3.day, na.rm=TRUE),
            chl.ugl.max=mean(chl.ugl.max, na.rm=TRUE),
            strat.kgm3=mean(strat.kgm3, na.rm=TRUE),
            year=mean(date, na.rm=TRUE)) %>%
  pivot_longer(c("prcp.cm", "discharge.m3.day", "chl.ugl.max"))

ggplot(annual.df, aes(year, value, group=name))+
  geom_line()+
  facet_wrap(.~name, ncol=1, scales="free_y")
  
