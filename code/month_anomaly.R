library(tidyverse)
library(glue)
library(gganimate)

month_anom<-read.table("data/merra2_seas_anom.txt",skip=3,header = T)%>%
  select(month=Month,seas_anom)%>%
  mutate(month=month.abb[month])

t_data<-read_csv("data/GLB.Ts+dSST.csv",skip = 1,na="***")%>%
  select(year=Year,all_of(month.abb))%>%
  pivot_longer(-year,names_to="month",values_to="t_diff")%>%
  drop_na()%>%
  inner_join(.,month_anom,by="month")%>%
  mutate(month=factor(month,levels = month.abb),
         month_anom=t_diff+seas_anom-0.7)%>%
  group_by(year)%>%
  mutate(ave=mean(month_anom))%>%
  ungroup()%>%
  mutate(ave=if_else(year==2022,max(abs(ave)),ave))
annotation<-t_data%>%
  slice_tail(n=1)
p<-t_data%>%
  ggplot(aes(x=month,y=month_anom,group=year,color=ave))+
  geom_line()+
  labs(x=NULL,y=NULL,title = "Temperature Anomaly (\u00B0 C)",
       subtitle ="Difference from 1980-2-15 annual mean" )+
  scale_color_gradient2(high = "darkred",mid="white",low="darkblue",midpoint = 0,
                        guide = "none")+
  scale_y_continuous(breaks = seq(-3,2,1))+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted",color = "gray",size=1),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(color="gray",size=10),
        plot.title.position = "plot",
        plot.margin = margin(t=10,r=15,b=10,l=10))

p+
  geom_point(data=annotation,aes(x=month,y=month_anom),size=5)+
  geom_text(data = annotation,aes(x=month,y=month_anom,label=glue("{month} {year}")),
            hjust=-0.2)
ggsave("figures/monthly_anomaly.png",width = 6,height = 4,units = "in") 


a<-p+
  geom_label(aes(x=7,y=0,label=year),fontface="bold",label.size = 0)+
  transition_manual(frames = year,cumulative = T)

animate(a,width = 6,height = 4,units = "in",res=300,
        renderer = av_renderer("figures/monthly_anomaly.mp4"))




