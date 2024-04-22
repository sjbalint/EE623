basetheme <- list(
  theme_classic(),
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    legend.title=element_blank(),
    panel.grid.major.x = element_line(linewidth=.1, color="gray"), 
    panel.grid.major.y = element_line(linewidth=.1, color="gray"),
    #axis.title.y = element_text(angle = 0,vjust = 0.5),
    #strip.text.y.left = element_text(angle = 0),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position="top",
    strip.placement = "outside"),
  scale_shape_manual(values=c(21:25,15:20)),
  scale_fill_nejm(),
  scale_color_nejm(),
  labs(x=NULL,y=NULL)
)

save(basetheme,file="Rdata/graphing/graphing_theme.Rdata")

labs.df <- data.frame(name=c("d15n.permil","ph","ph.mv","salinity.ppt","din.uml",
                             "prcp.mm.day","discharge.m3.day","secchi.depth.meters",
                             "chl.ugl","region","station","season",
                             "temp.c","do.mgl","latitude.degrees", "NAO"),
                      ylabs=as.character(c(
                        bquote(atop(delta^15*N,'(‰)')),
                        bquote(atop("pH","(nbs)")),
                        bquote(atop("pH","(mV)")),
                        bquote(atop("Salinity","(ppt)")),
                        bquote(atop("DIN","("*mu*"m/L)")),
                        bquote(atop("Precip","(mm/month)")),
                        bquote(atop("Discharge","("*m^3*"/month)")),
                        bquote(atop("Secchi","Depth"~"(m)")),
                        bquote(atop("Chlorophyll","("*mu*"g/L)")),
                        bquote("Region"),
                        bquote("Station"),
                        bquote("Season"),
                        bquote(atop("Temp","(C)")),
                        bquote(atop("DO","(mg/L)")),
                        bquote(atop("Latitude","(deg)")),
                        bquote(atop("N. Atlantic","Oscillation"))
                      )
                      ),
                      xlabs=as.character(c(
                        bquote(atop(delta^15*N~'(‰)')),
                        bquote(atop("pH"~"(nbs)")),
                        bquote(atop("pH"~"(mV)")),
                        bquote(atop("Salinity"~"(ppt)")),
                        bquote(atop("DIN"~"("*mu*"m/L)")),
                        bquote(atop("Precipitation"~"(mm/wk)")),
                        bquote(atop("Discharge"~"("*m^3*"/month)")),
                        bquote(atop("Secchi"~"Depth"~"(m)")),
                        bquote(atop("Chlorophyll"~"("*mu*"g/L)")),
                        bquote("Region"),
                        bquote("Station"),
                        bquote("Season"),
                        bquote("Temperature"~"Celcius"),
                        bquote("DO"~"(mg/L)"),
                        bquote("Latitude"~"(deg)"),
                        bquote(atop("North"~"Atlantic"~"Oscillation"))
                      )
                      )
)

save(labs.df,file="Rdata/graphing_labs.Rdata")
