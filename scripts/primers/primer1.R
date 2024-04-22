rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(marelac)


# calculate o2 sat --------------------------------------------------------

temp <- data.frame("temp.c"=seq(0,30,0.1))
sal <- data.frame("sal.ppt"=seq(0,40,0.1))

do.df <- cross_join(temp, sal) %>%
  mutate(do.mgl = gas_O2sat(S = sal.ppt, t = temp.c))

contour.df <- do.df %>%
  filter(sal.ppt==10 | sal.ppt==20)

ggplot(do.df, aes(y=sal.ppt, x=temp.c, z=do.mgl))+
  theme_bw()+
  geom_raster(aes(fill=do.mgl))+
  geom_contour(color="white")+
  scale_fill_viridis_c(option="rocket", direction = -1)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="Salinity (ppt)", x="Temperature (C)", fill="DO (mg/L)")
