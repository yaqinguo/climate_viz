library(tidyverse)

read_csv("data/GLB.Ts+dSST.csv",skip = 1,na="***") %>%
  select(year=Year,t_diff='J-D')%>%
  ggplot(aes(x=year,y=t_diff))+
  geom_point(shape=21,fill="white",aes(color="1"),show.legend = T)+
  geom_line(aes(color="1"),size=0.5,show.legend = F)+
  geom_smooth(se=FALSE,size=0.5,span=0.15,aes(color="2"),show.legend = F)+
  theme_light()+
  labs(x="YEAR",
       y="Temperature Anomaly (ºC)",
       title = "GLOBAL LAND-OCEAN\nTEMPERATURE INDEX",
       subtitle = "Data source: NASA's Goddard Insitute for Space\nStudies (GISS). Credit:NASA/GISS")+
  scale_x_continuous(breaks=seq(1880,2023,20),expand = c(0,0))+
  scale_y_continuous(limits=c(-0.5,1.5),expand = c(0,0))+
  scale_color_manual(name=NULL,
                     breaks = c(1,2),
                     values = c("gray","black"),
                     labels=c("Annual mean","Lowess smoothing"),
                     guide=guide_legend(override.aes = list(shape=15,size=5)))+
  theme(
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(b=10),color = "red"),
        plot.subtitle = element_text(margin = margin(b=10)),
    legend.position = c(0.2,0.9),
        legend.title = element_text(size=0),
        #legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),
        legend.margin = margin(0,0,0,0),
        legend.key = element_rect(color = "gray",size=0.5,margin(0,0,0,0)),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank()
        )
ggsave("figures/temeprature_difference.png",height = 4,width = 6)
  
