
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
data("coastlineWorldFine")

NW_coast = read_sf("./data/ne_10m_coastline/ne_10m_coastline.shp")


long <-c(-122.973434)
lat <-c(47.378616)
site_name <- c("Twanoh")

site_coords <- cbind(long=data.frame(long), lat=data.frame(late), name=data.frame(site_name))

ggplot() + 
  geom_sf(data=NW_coast, col = 1, fill = "ivory") +
  coord_sf(xlim = -c(125, 122), ylim = c(47,49)) +
  geom_point(data = site_coords, aes(x = long, y = lat), size = 5) +
  ggrepel::geom_text_repel(data = site_coords, aes(x = long, y = lat, label = site_name))+
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 8, colour = 1),
        panel.grid = element_line(colour = NA)) 
  #scale_x_continuous(breaks = c(35,55))+
  #scale_y_continuous(breaks = c(-25, 2)) +
  # ggsn::scalebar(location = "bottomleft", x.min = 35, x.max = 60,
  #                y.min = -30, y.max = 5, dist = 600, dd2km = TRUE, 
  #                model = "WGS84", st.dist = 0.02, st.size = 4)+
  # labs(x = NULL, y = NULL)

