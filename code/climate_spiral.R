library(tidyverse)
library(glue)
library(lubridate)
t_diff<-read_csv("data/GLB.Ts+dSST.csv",skip = 1,na="***")%>%
  select(year=Year,month.abb)%>%
  pivot_longer(-year,names_to="month",values_to="t_diff")%>%
  drop_na()%>%
  mutate(month=factor(month,levels = month.abb))

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
         month_number=as.numeric(month)-1)

annotation <- t_data%>%
  slice_max(year)%>%
  slice_max(month)
temp_lines<-tibble(
  x=12,
  y=c(1.5,2),
  labels=c("1.5\u00B0C","2.0\u00B0C")
)
month_labels<-tibble(
  x=1:12,
  y=2.5,
  labels=month.abb
)
t_data %>% 
  ggplot(aes(x=month_number,y=t_diff,group=year,color=year))+
  geom_point(x=12,y=-1.5,color="black",size=140)+
  geom_point(data = annotation,aes(x=month_number,y=t_diff,color=year),inherit.aes = F,size=5)+
  geom_text(x=12,y=-1.5,label="2022",size=5)+
  geom_hline(yintercept = c(1.5,2),color="red")+
  geom_line()+
  geom_label(data=temp_lines,aes(x=x,y=y,label=labels),color="red",inherit.aes = F,
             fill="black",label.size = 0)+
  geom_text(data=month_labels,aes(x=x,y=y,label=labels),color="white",inherit.aes = F,
            angle=seq(360-360/12,0,length.out = 12))+
  labs(x=NULL, y=NULL,
       title = glue("Global temperature change ({min(t_diff$year)}-{max(t_diff$year)})"))+
  scale_x_continuous(breaks = 1:12,
                      labels = month.abb)+
  scale_y_continuous(breaks = seq(-2,2,0.2),
                     limits = c(-1.5,2.5))+
  scale_color_viridis_c(breaks=seq(1880,2020,20),
                        guide = "none"
                        )+
  coord_polar()+
  theme(
      plot.background = element_rect(fill="#444444",color ="#444444" ),
      panel.background = element_rect(fill="#444444",size=1),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(colour = "white",size = 13),
      plot.title = element_text(colour = "white",hjust = 0.5,size = 13),
      
    )
ggsave("figures/temperature_spiral.png",width = 6,height = 6)
