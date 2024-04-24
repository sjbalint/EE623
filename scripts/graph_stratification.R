rm(list = ls()) #clear environment

# import libraries --------------------------------------------------------

library(tidyverse)
library(ggpointdensity) #to color the points
library(cowplot)
library(ggpmisc) #for stats

# import data -------------------------------------------------------------



source("functions/stats_functions.R")

strat.df <- readRDS("Rdata/stratification.rds") %>%
  filter(station=="CP") %>%
  filter(chl.ugl.max>0) %>%
  mutate(month=as.numeric(format(date, "%m")),
         season=get_season(date))%>%
  filter(season=="Summer")

usgs.df <- readRDS("Rdata/USGS.rds") %>%
  ungroup() %>%
  filter(site.name=="Blackstone River") %>%
  select(date, discharge.m3.day)

persiann.df <- readRDS("Rdata/basin.rds") %>%
  filter(Basins=="Blackstone River") %>%
  mutate(prcp.mm=perform_IQR(prcp.mm),
         prcp.cm=prcp.mm/10)

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


# cross correlation -------------------------------------------------------

xcorr <- function(df, x, y, lag.max=30){
  
  temp.df <- df %>%
    select(all_of(c(x, y))) %>%
    drop_na()
  
  data <- ccf(temp.df[,x], temp.df[,y], lag.max=lag.max, plot=FALSE)
  
  plot.df <- data.frame("acf"=data$acf,
                        "lag"=data$lag)
  
  max <- plot.df %>%
    arrange(desc(abs(acf))) %>%
    pull(lag)
  
  max <- max[1]
  
  p1 <- ggplot(plot.df, aes(lag, acf))+
    mytheme+
    geom_hline(yintercept=0)+
    geom_area(color="black", fill="orange", alpha=0.5)+
    geom_point(shape=21)+
    geom_vline(xintercept=max, linetype="dashed")+
    labs(x="Lag (days)", y="ACF")+
    theme(panel.grid.major.y = element_line())+
    scale_x_continuous(limits=c(-(lag.max/2),lag.max))
  
  return(p1)
}

xcorr(df, x="do.mgl.min",y="chl.ugl.max", lag.max=20)+
  labs(title="Lag between Chlorophyll and DO")

ggsave("figures/lags/chl_do.png",width=mywidth, height=myheight)

xcorr(df, x="do.mgl.min",y="prcp.cm")+
  labs(title="Lag between Precipitation and DO")

ggsave("figures/lags/prcp_do.png",width=mywidth, height=myheight)

xcorr(df, x="discharge.m3.day",y="prcp.cm", lag.max=20)+
  labs(title="Lag between Precipitation and River Discharge")

ggsave("figures/lags/prcp_disch.png",width=mywidth, height=myheight)

xcorr(df, x="strat.kgm3",y="prcp.cm", lag.max=10)+
  labs(title="Lag between Precipitation and Stratification")

ggsave("figures/lags/prct_strat.png",width=mywidth, height=myheight)

xcorr(df, x="do.mgl.min",y="discharge.m3.day", lag.max=45)+
  labs(title="Lag between River Discharge and DO")

ggsave("figures/lags/disch_do.png",width=mywidth, height=myheight)

xcorr(df, x="chl.ugl.max",y="prcp.mm", lag.max=15)+
  labs(title="Lag between Precipitation and Chlorophyll")

ggsave("figures/lags/prcp_chl.png",width=mywidth, height=myheight)

p1 <- xcorr(df, x="chl.ugl.max",y="discharge.m3.day", lag.max=15)+
  labs(subtitle="River Discharge and Chlorophyll", x=NULL)

p2 <- xcorr(df, x="strat.kgm3",y="discharge.m3.day", lag.max=15)+
  labs(subtitle="River Discharge and Stratification", x=NULL, y=NULL)

p3 <- xcorr(df, x="do.mgl.min",y="chl.ugl.max", lag.max=20)+
  labs(subtitle="Chlorophyll and DO")

p4 <- xcorr(df, x="do.mgl.min",y="strat.kgm3")+
  labs(subtitle="Stratification and DO", y=NULL)

plot_grid(p1,p2,p3,p4, align="hv", labels="AUTO")

ggsave("figures/lags/final.png",width=10, height=10)

# simple regression -------------------------------------------------------

plot.df <- df %>%
  filter(season=="Summer")

p1 <- ggplot(plot.df, aes(strat.kgm3, do.mgl.min))+
  mytheme+
  geom_pointdensity(shape=21, show.legend=FALSE)+
  geom_smooth(method="lm", color="red")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE, vstep=0, color="black", label.y = "top", label.x="right", na.rm=TRUE)+
  labs(x=bquote("Water column stratification (kg/"*m^3*")"),
       y="Minimum bottom DO (mg/L)")

ggsave("figures/do_strat_regression.png", width=mywidth, height=myheight)

plot.df <- df %>%
  mutate(chl.ugl.max=perform_IQR(chl.ugl.max),
         chl.ugl.max=lag(chl.ugl.max, n=6))

p2 <- ggplot(plot.df, aes(chl.ugl.max, do.mgl.min))+
  mytheme+
  geom_pointdensity(shape=21, show.legend=FALSE)+
  geom_smooth(method="lm", color="red")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE, vstep=0, color="black", label.y = "top", label.x="right", na.rm=TRUE)+
  labs(x=bquote("Maximum Surface Chlorophyll ("*mu*"g/L)"),
       y="Lagged Minimum bottom DO (mg/L)")

ggsave("figures/do_chl_regression.png", width=mywidth, height=myheight)

plot.df <- df %>%
  mutate(discharge.m3.day=perform_IQR(discharge.m3.day),
         discharge.m3.day=lag(discharge.m3.day,0))

p3 <- ggplot(plot.df, aes(discharge.m3.day, chl.ugl.max))+
  mytheme+
  geom_pointdensity(shape=21, show.legend=FALSE)+
  geom_smooth(method="lm", color="red")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE, vstep=0, color="black", label.y = "top", label.x="right", na.rm=TRUE)+
  labs(x=bquote("River Discharge ("*m^3*"/day)"),
       y=bquote("Surface Chlorophyll ("*mu*"g/L)"))

ggsave("figures/discharge_chl_regression.png", width=mywidth, height=myheight)

p4 <- ggplot(plot.df, aes(discharge.m3.day, strat.kgm3))+
  mytheme+
  geom_pointdensity(shape=21, show.legend=FALSE)+
  geom_smooth(method="lm", color="red")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE, vstep=0, color="black", label.y = "top", label.x="right", na.rm=TRUE)+
  labs(x=bquote("River Discharge ("*m^3*"/day)"),
       y=bquote("Water column stratification (kg/"*m^3*")"))

ggsave("figures/discharge_strat_regression.png", width=mywidth, height=myheight)

plot_grid(p1,p2,p3,p4, align="hv", labels="AUTO")

ggsave("figures/regression_final.png",width=10, height=10)


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


