list.of.packages <- c("sp","rgdal","leaflet","data.table","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/itep_2018_uganda_map")

ug = readOGR("shp/uganda.shp")
dist.dat = read.csv("data/districts.csv")
dist.dat$percent = dist.dat$percent*100
ug = merge(ug,dist.dat,by="name")

cities = read.csv("data/cities.csv")
coordinates(cities)=~long+lat

pal <- colorBin(
  palette = "YlOrRd",
  domain = ug@data[,"percent"]
)

leaflet(data=ug) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(33, 1, zoom=6) %>% 
  addPolygons(color = pal(ug@data[,"percent"])
                      ,fillOpacity = 1
                      ,stroke=F
                      ,smoothFactor=0.2
                      ,popup=paste0(
                        "<b>",ug@data$name,"</b> <br/>",
                        "<b>Total FDI, 2012-2016 (US$ millions): </b>$",round(ug@data$fdi),"<br/>",
                        "<b>Percent: </b>",round(ug@data[,"percent"],2)
                      )) %>%
  addMarkers(
    data=cities
    ,popup=paste0(
      "<b>",cities@data$city,"</b> <br/>",
      "<b>District: </b>",cities@data$district,"<br/>",
      "<b>Total FDI, 2012-2016 (US$ millions): </b>$",round(cities@data$fdi),"<br/>"
    )
    ) %>%
addLegend("bottomright", pal=pal, values = ug@data[,"percent"], opacity = 1, title="Total FDI, 2012-2016",labFormat = labelFormat(suffix="%"))
