list.of.packages <- c("sp","rgdal","leaflet","data.table","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/itep_2018_uganda_map")

ug = readOGR("shp/uganda.shp")
dist.dat = read.csv("data/districts.csv")
dist.dat$percent = NULL

ug.f = fortify(ug,region="name")
setnames(ug.f,"id","name")
ug.f = merge(ug.f,dist.dat,by="name",all.x=T)
ug.f = ug.f[order(ug.f$order),]

fdi_map = ggplot(ug.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=fdi,color="grey",size=0.02))+
  coord_fixed(1) +
  scale_fill_gradient(
    na.value = "#d0cccf",
    low="yellow",
    high="red",
    guide="legend"
  ) + 
  scale_color_identity()+
  scale_size_identity()+
  expand_limits(x=ug.f$long,y=ug.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  labs(x="",y="")
ggsave(paste0("eps/fdi.png"),fdi_map,device="png",width=10,height=6)
ggsave(paste0("eps/fdi.eps"),fdi_map,device="eps",width=10,height=6)