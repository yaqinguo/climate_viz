library(tidyverse)
library(scales)
library(glue)
t_data<-read_csv("data/GLB.Ts+dSST.csv",skip = 1,na="***")%>%
  select(year=Year,t_diff='J-D')%>%
  drop_na()

t_data%>%
  ggplot(aes(x=year,y=1,fill=t_diff))+
  geom_tile(show.legend = F)+
  scale_fill_stepsn(colors=c(low="#08306b","white","#67000D"),
                    values = rescale(c(min(t_data$t_diff),0,max(t_data$t_diff))),
                    n.breaks=12)+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = seq(1890,2020,30))+
  labs(title = glue("Global temeprature change ({min(t_data$year)}-{max(t_data$year)})"))+
  theme_void()+
  theme(
    axis.text.x = element_text(color = "white",margin = margin(t=5,b=10,unit = "pt")),
    plot.background = element_rect(fill="black"),
    plot.title = element_text(color = "white",margin = margin(t=10,b=5,r=5,unit="pt"),
                              hjust = 0.05)
  )
ggsave("figures/warming_stripes.png",width = 8,height = 4.5)
