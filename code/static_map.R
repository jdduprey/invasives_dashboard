
library(tidyverse)
library(spData)
library(sf)
library(raster)
library(maps) 
library(mapdata)
library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(ggsn)
library(tidyverse)
library(here)
library(oce)
library(ocedata)
library(dplyr)

data("coastlineWorldFine")

NW_coast = read_sf("./data/ne_10m_coastline/ne_10m_coastline.shp")

site_meta <- read.csv("./data/site_meta.csv")

p <- ggplot() + 
  geom_sf(data=NW_coast, col = 1, fill = "ivory") +
  coord_sf(xlim = -c(125, 122), ylim = c(47,49)) +
  geom_point(data = site_meta, aes(x = long, y = lat), size = 8, col="blue") +
  ggrepel::geom_text_repel(data = site_meta, aes(x = long, y = lat, label = site_name))+
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 8, colour = 1, face = "bold"),
        panel.grid = element_line(colour = NA)) 

p 

p + geom_point(data = site_meta, aes(x = long, y = lat), size = 5, col="red")

p
  #scale_x_continuous(breaks = c(35,55))+
  #scale_y_continuous(breaks = c(-25, 2)) +
  # ggsn::scalebar(location = "bottomleft", x.min = 35, x.max = 60,
  #                y.min = -30, y.max = 5, dist = 600, dd2km = TRUE, 
  #                model = "WGS84", st.dist = 0.02, st.size = 4)+
  # labs(x = NULL, y = NULL)

#TODO link this with invasion data
#TODO clip shapefile so it loads reasonably 
#TODO lower opacity of circles so coast is visible

