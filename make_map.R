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
  domain = ug@data[,"percent"],
  na.color="#d0cccf",
  bins = c(0,5,20,40)
)

centroids = getSpPPolygonsLabptSlots(ug)
label.df = data.frame(long=centroids[,1],lat=centroids[,2],label=ug@data$name,percent=ug@data$percent)
label.df = subset(label.df,percent>0)
coordinates(label.df)=~long+lat

leaflet(data=ug) %>%
  addTiles("http://178.79.185.236:8080/styles/ugandageojson/rendered/{z}/{x}/{y}.png") %>%
  setView(33, 1, zoom=6) %>% 
  addPolygons(color = pal(ug@data[,"percent"])
                      ,fillOpacity = 1
                      ,stroke=F
                      ,smoothFactor=0.2
                      ,popup=paste0(
                        "<b>District name: ",ug@data$name,"<br/>",
                        "<b>Total FDI, 2012-2016 (US$ millions): </b>$",round(ug@data$fdi),"<br/>",
                        "<b>Percent: </b>",round(ug@data[,"percent"],2)
                      )) %>%
  addPolylines(
    color="#eeeeee",
    weight=0.5,
    opacity=1,
    smoothFactor=0.2
  ) %>%
  addLabelOnlyMarkers(
    data=label.df,
    label=~label,
    labelOptions = labelOptions(
      noHide=T,
      textOnly=T,
      direction="right",
      style= list(
        "font-family"="serif"
        ,"text-shadow"="-1px 0 white, 0 1px white, 1px 0 white, 0 -1px white"
      )
    )
  ) %>%
  # addCircleMarkers(
  #   data=cities
  #   ,stroke=F
  #   ,radius=2.5
  #   ,fillOpacity=.8
  #   ,popup=paste0(
  #     "<b>City name: </b>",cities@data$city,"<br/>",
  #     "<b>District: </b>",cities@data$district,"<br/>",
  #     "<b>Total FDI, 2012-2016 (US$ millions): </b>$",round(cities@data$fdi),"<br/>"
  #   )
  #   ) %>%
addLegend(
  "bottomright"
  , pal=pal
  , values = ug@data[,"percent"]
  , opacity = 1
  , title="Percent of total FDI capital  <br> investments: 2012-2016"
  ,labFormat = labelFormat(suffix="%")
  ,na.label = "0/no data"
  )
