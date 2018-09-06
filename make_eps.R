list.of.packages <- c("sp","rgdal","leaflet","data.table","ggplot2","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
source("https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/ddw/connect.R")

setwd("~/git/itep_2018_uganda_map")

reds = c("#FBD7CB","#F6B2A7","#F28E83","#ED695E","#E8443A")
purples = c("#DEB5D6","#CD9DC8","#BC86BA","#AB6EAC","#9A579E","#893F90")

ug = readOGR("shp/uganda.shp")
dist.dat = read.csv("data/districts.csv")
dist.dat$fdi = NULL
setnames(dist.dat,"percent","fdi")
dist.dat$fdi = dist.dat$fdi*100

districts = ddw("spotlight_on_uganda.ref_uganda_district")
districts$name[which(districts$name=="Sembabule")] = "Ssembabule"
setnames(districts,"id","district_id")
keep = c("name","value")
pov = ddw("spotlight_on_uganda.uganda_poverty_headcount")
pov = merge(pov,districts,by="district_id",all.x=T)
pov = pov[keep]
setnames(pov,"value","pov")

local = ddw("spotlight_on_uganda.uganda_local_percent")
local = merge(local,districts,by="district_id",all.x=T)
local = subset(local,year==2016 & budget_type=="actual")
local = local[keep]
setnames(local,"value","local")

donor = ddw("spotlight_on_uganda.uganda_donor_percent")
donor = merge(donor,districts,by="district_id",all.x=T)
donor = subset(donor,year==2016 & budget_type=="actual")
donor = donor[keep]
setnames(donor,"value","donor")

ug.f = fortify(ug,region="name")
setnames(ug.f,"id","name")
ug.f = merge(ug.f,dist.dat,by="name",all.x=T)
ug.f = merge(ug.f,pov,by="name",all.x=T)
ug.f = merge(ug.f,local,by="name",all.x=T)
ug.f = merge(ug.f,donor,by="name",all.x=T)

ug.f = ug.f[order(ug.f$order),]

palbins = c(30,50,70,90,97)
names(palbins)=c("<30 %","30-50","50-70","70-90",">90 %")
pal <- colorBin(
  palette = reds,
  domain = pov$pov,
  na.color="#d0cccf",
  bins = palbins
)

pov_map = 
  ggplot(ug.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=pov,color="#eeeeee",size=0.21))+
  coord_fixed(1) +
  scale_fill_gradientn(
    na.value="#d0cccf",
    guide="legend",
    breaks=palbins,
    colors=pal(palbins),
    values=rescale(palbins)
  ) +
  scale_color_identity()+
  scale_size_identity()+
  expand_limits(x=ug.f$long,y=ug.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  guides(fill=guide_legend(title=""))+
  labs(x="",y="")
ggsave(paste0("eps/pov.png"),pov_map,device="png",width=10,height=6)
ggsave(paste0("eps/pov.eps"),pov_map,device="eps",width=10,height=6)

palbins = c(1,2,3,4,10,17)
names(palbins) = c("<1 %","1-2","2-3","3-4","4-10",">10 %")
pal <- colorBin(
  palette = purples,
  domain = local$local,
  na.color="#d0cccf",
  bins = palbins
)


local_map = ggplot(ug.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=local,color="#eeeeee",size=0.21))+
  coord_fixed(1) +
  scale_fill_gradientn(
    na.value="#d0cccf",
    guide="legend",
    breaks=palbins,
    colors=pal(palbins),
    values=rescale(palbins)
  ) +
  scale_color_identity()+
  scale_size_identity()+
  expand_limits(x=ug.f$long,y=ug.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  guides(fill=guide_legend(title=""))+
  labs(x="",y="")
ggsave(paste0("eps/local.png"),local_map,device="png",width=10,height=6)
ggsave(paste0("eps/local.eps"),local_map,device="eps",width=10,height=6)

palbins = c(1,2,3,4,10,30)
names(palbins) = c("<1 %","1-2","2-3","3-4","4-10",">10 %")
pal <- colorBin(
  palette = purples,
  domain = donor$donor,
  na.color="#d0cccf",
  bins = palbins
)


donor_map = ggplot(ug.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=donor,color="#eeeeee",size=0.21))+
  coord_fixed(1) +
  scale_fill_gradientn(
    na.value="#d0cccf",
    guide="legend",
    breaks=palbins,
    colors=pal(palbins),
    values=rescale(palbins)
  ) +
  scale_color_identity()+
  scale_size_identity()+
  expand_limits(x=ug.f$long,y=ug.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  guides(fill=guide_legend(title=""))+
  labs(x="",y="")
ggsave(paste0("eps/donor.png"),donor_map,device="png",width=10,height=6)
ggsave(paste0("eps/donor.eps"),donor_map,device="eps",width=10,height=6)

palbins = c(5,20,36)
names(palbins) = c("<5 %","5-20",">20 %")
pal <- colorBin(
  palette = "YlOrRd",
  domain = dist.dat$fdi,
  na.color="#d0cccf",
  bins = palbins
)


fdi_map = ggplot(ug.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=fdi,color="#eeeeee",size=0.21))+
  coord_fixed(1) +
  scale_fill_gradientn(
    na.value="#d0cccf",
    guide="legend",
    breaks=palbins,
    colors=pal(palbins),
    values=rescale(palbins)
  ) +
  scale_color_identity()+
  scale_size_identity()+
  expand_limits(x=ug.f$long,y=ug.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  guides(fill=guide_legend(title=""))+
  labs(x="",y="")
ggsave(paste0("eps/fdi.png"),fdi_map,device="png",width=10,height=6)
ggsave(paste0("eps/fdi.eps"),fdi_map,device="eps",width=10,height=6)