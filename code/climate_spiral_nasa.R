library(tidyverse)
library(glue)
library(gganimate)
library(av)
t_diff<-read_csv("data/GLB.Ts+dSST.csv",skip = 1,na="***")%>%
  select(year=Year,month.abb)%>%
  pivot_longer(-year,names_to="month",values_to="t_diff")%>%
  drop_na()

# t_Dec<-t_diff%>%
#   filter(month=="Dec")%>%
#   mutate(year=year+1,
#          month="last_Dec")
  
t_Jan<-t_diff%>%
  filter(month=="Jan")%>%
  mutate(year=year-1,
         month="next_Jan")

t_data <- bind_rows(t_diff,t_Jan)%>%
  mutate(month=factor(month,levels = c(month.abb,"next_Jan")),
         month_number=as.numeric(month))%>%
  arrange(year,month)%>%
  filter(year !=1879)%>%
  mutate(step_number=1:nrow(.))

annotation <- t_data%>%
  slice_max(year)%>%
  slice_max(month_number)
temp_lines<-tibble(
  x=1,
  y=c(-1,0,1),
  labels=c("-1\u00B0 C","0\u00B0 C","1\u00B0 C")
)
month_labels<-tibble(
  x=1:12,
  y=1.5,
  labels=toupper(month.abb)
)
grindlines<-tibble(
  x=c(1.6,1.3,1.2),
  xend=c(12.4,12.7,12.8),
  y=c(-1,0,1),
  yend=y
)
a<-t_data %>% 
  ggplot(aes(x=month_number,y=t_diff,group=year,color=t_diff))+
  #geom_point(x=1,y=-1.5,color="black",size=120,inherit.aes = F)+
  #geom_point(data = annotation,aes(x=month_number,y=t_diff,color=year),inherit.aes = F,size=5)+
  geom_label(aes(x=1,y=-1.7,label=year),size=6,
             fill="black",show.legend = F,
             label.size = 0)+
  geom_line()+
  #geom_hline(yintercept = c(-1,0,1),color="red")+
  geom_segment(data = grindlines,aes(x=x,y=y,xend=xend,yend=yend),
               color=c("yellow","green","yellow"),
               inherit.aes = F,
               size=2)+
  geom_text(data=temp_lines,aes(x=x,y=y,label=labels),size=2,fontface="bold",
            color=c("yellow","green","yellow"),
            inherit.aes = F,show.legend = F,
             )+
  geom_text(data=month_labels,aes(x=x,y=y,label=labels),color="yellow",inherit.aes = F,
            #angle=seq(360-360/12,0,length.out = 12)
            )+
  scale_y_continuous(limits = c(-2.0,1.5),expand = c(0,-0.3))+
  scale_color_gradient2(low="blue",high = "red",mid = "white",midpoint = 0
                        )+
  coord_polar(start = 0)+
  labs(x=NULL, y=NULL,
       #title = glue("Global temperature change ({min(t_diff$year)}-{max(t_diff$year)})")
  )+
  theme(
      plot.background = element_rect(fill="black",color ="black" ),
      panel.background = element_rect(fill="black"),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )+
  transition_manual(frames = year,cumulative = T)
#ggsave("figures/climate_spiral_nasa.png",width=4.155,,height = 4.5, units = "in",dpi=300)
animate(a,width=6,height=6,unit="in",res=300,
        renderer = av_renderer("figures/climate_spiral_nasa.mp4"))

anim_save("figures/temperature_spiral.gif")
