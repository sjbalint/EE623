#Sawyer Balint
#Feb 1 2024

rm(list = ls()) #clear environment

# import packages and data ------------------------------------------------

library(tidyverse)
library(ggbreak)

df <- read.csv("raw/seawater_comp.csv") %>%
  arrange(desc(mmol.kg))

levels <- df %>%
  pull(species) %>%
  unique()

df <- df %>%
  mutate(species=factor(species, levels=levels))

# make a plot -------------------------------------------------------------

ggplot(df,aes(species, mmol.kg))+
  theme_classic()+
  geom_col(color="black",fill="grey")+
  scale_y_break(breaks=c(0.3,0.5,2,3,75,400), scales="free", expand=FALSE)+
  labs(y="mmol/kg",x="Species")+
  theme(axis.text.x = element_text(angle=90,hjust=1, vjust=0.5))

ggsave("figures/seawater_chemical_comp.png", width=6, height=6)
