rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(zoo) #for rollapply

source("functions/stats_functions.R")

# load data ---------------------------------------------------------------

df <- readRDS("Rdata/daily.rds") %>%
  mutate(prcp.mm=perform_IQR(prcp.mm),
         prcp.mm=rollsum(prcp.mm, k=365, na.pad = TRUE, align="center", na.rm=TRUE),
         prcp.cm=prcp.mm/10
         )

# make a graph ------------------------------------------------------------

ggplot(df, aes(date, prcp.cm))+
  theme_bw()+
  geom_line()+
  labs(x=NULL, y="1-year rolling average precipitation (cm)")

ggsave("figures/rolling_annual.png")

