library(tidyverse)
library(glue)
library(plotly)
library(htmlwidgets)
t_data<- read_csv("data/GLB.Ts+dSST.csv",skip = 1,na="***")%>%
  select(year=Year,all_of(month.abb))%>%
  pivot_longer(-year,names_to="month",values_to="t_diff")%>%
  drop_na()%>%
  mutate(month=factor(month,levels = month.abb))%>%
  arrange(year,month)%>%
  mutate(month_number=as.numeric(month),
         theta=2*pi*(month_number-1)/12,
         radius=t_diff+1.5,
         x=radius*sin(theta),
         y=radius*cos(theta),
         z=year,
         label=glue("{month} {year}\n{t_diff}\u00B0C"))
axx <- list(
  title = "",
  showgrid=F,
  zeroline=F,
  showticklabels=F
)

axy <- list(
  title = "",
  showgrid=F,
  zeroline=F,
  showticklabels=F
)

axz <- list(
  title = ""
)
p<-plot_ly(t_data, 
        x = ~x, y = ~y, z = ~z, 
        text=~label,
        hoverinfo="text",
        type = 'scatter3d', 
        mode = 'lines',
        line = list(width = 10, color = ~t_diff, 
                    cmid=0,
                    colorscale = list(c(0,'blue'),
                                      c(0.5,'white'),
                                      c(1,'red'))))%>%
  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
              
results_dir = paste0(getwd(),"/figures") # get directory
saveWidget(p,
           file.path(results_dir,"climate_spiral_plotly.html"))
                  



















