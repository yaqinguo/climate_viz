library(tidyverse)
library(glue)
library(lubridate)
t_diff<-read_csv("data/GLB.Ts+dSST.csv",skip = 1,na="***")%>%
  select(year=Year,month.abb)%>%
  pivot_longer(-year,names_to="month",values_to="t_diff")%>%
  drop_na()%>%
  mutate(month=factor(month,levels = month.abb))

t_Dec<-t_diff%>%
  filter(month=="Dec")%>%
  mutate(year=year+1,
         month="last_Dec")
  
t_Jan<-t_diff%>%
  filter(month=="Jan")%>%
  mutate(year=year-1,
         month="next_Jan")

t_data <- bind_rows(t_Dec,t_diff,t_Jan)%>%
  mutate(month=factor(month,levels = c("last_Dec",month.abb,"next_Jan")),
         month_number=as.numeric(month)-1,
         this_year=year==year(today()))

annotation <- t_data%>%
  slice_max(year)%>%
  slice_max(month)

t_data %>% 
  ggplot(aes(x=month_number,y=t_diff,group=year,color=year,
             size=this_year))+
  geom_hline(yintercept = 0,color="white")+
  geom_line()+
  geom_text(data=annotation,aes(x=month_number,y=t_diff,label=year,color=year),
            inherit.aes = F,size=5,hjust=0,nudge_x = 0.15,fontface="bold")+
  labs(x=NULL, y="Temperature changes since pre-industrial times [\u00b0C]",
       title = glue("Global temperature change since {min(t_diff$year)} by month"))+
  scale_x_continuous(breaks = 1:12,
                      labels = month.abb,
                     sec.axis = dup_axis(name = NULL,labels = NULL))+
  scale_y_continuous(breaks = seq(-2,2,0.2),
                     sec.axis = dup_axis(name = NULL,labels = NULL))+
  scale_color_viridis_c(breaks=seq(1880,2020,20),
                        guide = guide_colorbar(frame.colour = "white",
                                               frame.linewidth = 1.2),
                        )+
  scale_size_manual(breaks = c(FALSE,TRUE),
                    values = c(0.25,1),guide="none")+
  coord_cartesian(xlim = c(1,12))+
  theme(
      plot.background = element_rect(fill="#444444"),
      panel.background = element_rect(fill="black",color = "white",size=1),
      panel.grid = element_blank(),
      axis.text = element_text(color = "white",size = 13),
      axis.ticks = element_line(color = "white"),
      axis.ticks.length = unit(-5,"pt"),
      axis.title = element_text(colour = "white",size = 13),
      plot.title = element_text(colour = "white",hjust = 0.5,size = 13),
      legend.background = element_rect(fill = NA),
      legend.title = element_blank(),
      legend.text = element_text(colour = "white"),
      legend.key.height = unit(55,"pt")
    )
ggsave("figures/temperature_lines.png",width = 8,height = 4.5)
