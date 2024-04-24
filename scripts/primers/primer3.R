rm(list = ls()) #clear environment

# load packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(scales) #for psuedolog
library(ggsci)

# import data -------------------------------------------------------------

filepath <- "raw/primers/benthic fluxes/"

lis.df <- read.csv(paste0(filepath,"LIS_BenthicFlux_Data.csv"))

wb_normoxic.df <- read.csv(paste0(filepath,"NormoxicFlux.csv")) %>%
  mutate(treatment="Normoxic")

wb_hypoxic.df <- read.csv(paste0(filepath,"HypoxicFlux.csv"))%>%
  mutate(treatment="Hypoxic")

wb.df <- bind_rows(wb_normoxic.df,wb_hypoxic.df)

nb.df <- read_excel(paste0(filepath,"Nbay_2021.xlsx"))


# clean up the data -------------------------------------------------------

flux.colnames <- c("o2.umolm2h","n2.umolm2hr","n2o.umolm2hr",
                   "ch4.umolm2hr","nh4.umolm2hr", "nox.umolm2hr","po4.umolm2hr")

nb.df <- nb.df %>%
  select(all_of(c("Site", "Coring Date", "Core ID", "O2Flux, umol m-2 h-1",
                  "N2Flux, umol m-2 h-1", "N2O flux, umol m-2 h-1",
                  "CH4 flux, umol m-2 h-1", "NH4+ Flux (umol m-2 h-1)",
                  "NO2- + NO3- Flux (umol m-2 h-1)", "PO43- Flux (umol m-2 h-1)")))

colnames(nb.df) <- c("station","date","ID",flux.colnames)

nb.df <- nb.df %>%
  mutate(across(all_of(c("station","ID")),as.factor)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(study="Narragansett Bay")

lis.df <- lis.df %>%
  select(all_of(c("Collection.Date","Station","Distance","SOD", "N2N", "N2O",
                  "CH4", "NH4","NOx","DIP")))

colnames(lis.df) <- c("date","station","distance.km",flux.colnames)

lis.df <- lis.df %>%
  mutate(date=as.Date(date, format="%d-%b-%y"),
         station=factor(station),
         study="Long Island Sound")

wb.df <- wb.df %>%
  mutate(nox=as.numeric(NA)) %>%
  select(all_of(c("Date","Station","treatment","O2.uptake","N2.N.Flux",
                  "N2O.Flux","CH4.Flux","NH4..Flux","nox","PO43..Flux")))

colnames(wb.df) <- c("date","station","treatment",flux.colnames)

wb.df <- wb.df %>%
  mutate(date=as.Date(date, format="%d-%b-%y"),
         across(all_of(c("station", "treatment")),as.factor)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(n2o.umolm2hr=n2o.umolm2hr/1000, #fix units
         ch4.umolm2hr=ch4.umolm2hr/1000) %>%
  mutate(study="Waquit Bay")

df <- bind_rows(nb.df, lis.df, wb.df) %>%
  mutate(o2.umolm2h=-o2.umolm2h)


# graphing theme ----------------------------------------------------------

mywidth=6
myheight=4

mytheme <- list(
  theme_classic(),
  geom_hline(yintercept=0, linetype="dashed"),
  scale_y_continuous(trans=pseudo_log_trans(base = 10), breaks=c(-10^seq(1,6,1), 0, 10^seq(1,7,1))),
  scale_x_discrete(labels=label_parse()),
  annotation_logticks(sides="l"),
  theme(legend.position="top"),
  labs(fill=NULL, x=NULL, y=bquote("Flux ("*mu*"mol"~m^-2~h^-1*")")),
  scale_fill_jco()
)


# make a graph ------------------------------------------------------------

long.df <- df %>%
  pivot_longer(all_of(flux.colnames)) %>%
  mutate(name=factor(name, levels=flux.colnames,
                     labels=c(bquote(O[2]),
                              bquote(N[2]),
                              bquote(N[2]*O),
                              bquote(CH[4]),
                              bquote(NH[4]),
                              bquote(NO[x]),
                              bquote(PO[4])
                              )))

plot.df <- long.df %>%
  filter(is.na(treatment) | treatment=="Normoxic")

ggplot(plot.df, aes(name, value, fill=study))+
  mytheme+
  geom_boxplot(outlier.shape=21, alpha=0.7)

ggsave("figures/primers/all_studies.png", width=mywidth, height=myheight)

plot.df <- long.df %>%
  filter(study=="Waquit Bay") %>%
  drop_na(value)

ggplot(plot.df, aes(name, value, fill=treatment))+
  mytheme+
  geom_boxplot(outlier.shape=21, alpha=0.7)+
  scale_fill_jama()

ggsave("figures/primers/waquit.png", width=mywidth, height=myheight)

plot.df <- long.df %>%
  filter(study=="Long Island Sound") %>%
  drop_na(value)

ggplot(plot.df, aes(name, value, fill=as.factor(distance.km)))+
  mytheme+
  geom_boxplot(outlier.shape=21, alpha=0.7)+
  scale_fill_brewer(palette="Blues", direction = -1)+
  labs(fill="Distance to NYC")

ggsave("figures/primers/LIS.png", width=mywidth, height=myheight)


# summarize these things --------------------------------------------------

df %>%
  group_by(study, treatment) %>%
  summarize(across(flux.colnames, mean, na.rm=TRUE))

