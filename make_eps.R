list.of.packages <- c("sp","rgdal","leaflet","data.table","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
source("https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/ddw/connect.R")

setwd("~/git/itep_2018_uganda_map")

ug = readOGR("shp/uganda.shp")
dist.dat = read.csv("data/districts.csv")
dist.dat$percent = NULL

districts = ddw("spotlight_on_uganda.ref_uganda_district")
setnames(districts,"id","district_id")
keep = c("name","value")
pov = ddw("spotlight_on_uganda.uganda_poverty_headcount")
pov = merge(pov,districts,by="district_id",all.x=T)
pov = pov[keep]
setnames(pov,"value","pov")

local = ddw("spotlight_on_uganda.uganda_local_percent")
local = merge(local,districts,by="district_id",all.x=T)
local = subset(local,year==2016)
local = local[keep]
setnames(local,"value","local")

donor = ddw("spotlight_on_uganda.uganda_donor_percent")
donor = merge(donor,districts,by="district_id",all.x=T)
donor = subset(donor,year==2016)
donor = donor[keep]
setnames(donor,"value","donor")

ug.f = fortify(ug,region="name")
setnames(ug.f,"id","name")
ug.f = merge(ug.f,dist.dat,by="name",all.x=T)
ug.f = merge(ug.f,pov,by="name",all.x=T)
ug.f = merge(ug.f,local,by="name",all.x=T)
ug.f = merge(ug.f,donor,by="name",all.x=T)

ug.f = ug.f[order(ug.f$order),]

pov_map = ggplot(ug.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=pov,color="grey",size=0.02))+
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
ggsave(paste0("eps/pov.png"),pov_map,device="png",width=10,height=6)
ggsave(paste0("eps/pov.eps"),pov_map,device="eps",width=10,height=6)

local_map = ggplot(ug.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=local,color="grey",size=0.02))+
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
ggsave(paste0("eps/local.png"),local_map,device="png",width=10,height=6)
ggsave(paste0("eps/local.eps"),local_map,device="eps",width=10,height=6)

donor_map = ggplot(ug.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=donor,color="grey",size=0.02))+
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
ggsave(paste0("eps/donor.png"),donor_map,device="png",width=10,height=6)
ggsave(paste0("eps/donor.eps"),donor_map,device="eps",width=10,height=6)

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