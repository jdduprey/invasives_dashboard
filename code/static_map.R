
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
library(dplyr)

# load shapefiles, metadata
NW_coast <- read_sf("./data/ne_10m_coastline/ne_10m_coastline.shp")
site_meta <- read.csv("./data/site_meta.csv")
invasion_data <- read.csv("./data/monthly_invasion_data.csv")

# link site data with invasives data
full_map_df <- left_join(invasion_data, site_meta)

# this function will operate with the reactive slider
time_filter <- function(df, selected_month, selected_year){
  out_df <- df %>%
    filter(month %in% c(selected_month)) %>%
    filter(year %in% c(selected_year))
}

# and get a single month and year
mar_2018_df <- time_filter(full_map_df, 03, 2018)


# store map with plot to variable
p <- ggplot() + 
  geom_sf(data=NW_coast, col = 1, fill = "ivory") +
  coord_sf(xlim = -c(125, 122), ylim = c(47,49)) +
  geom_point(data = mar_2018_df, aes(x = long, y = lat, color=prop_nn), size = 8, alpha = 0.8) +
  scale_color_distiller(palette = "RdYlBu") +
  geom_text(data= mar_2018_df, aes(x = long, y = lat, label=round(prop_nn,2)), size = 2) +
  ggrepel::geom_text_repel(data = mar_2018_df, aes(x = long, y = lat, label = site)) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 8, colour = 1, face = "bold"),
        panel.grid = element_line(colour = NA)) 

# save plot
ggsave(filename="./figures/site_map.png")

# display plot
p 


  #scale_x_continuous(breaks = c(35,55))+
  #scale_y_continuous(breaks = c(-25, 2)) +
  # ggsn::scalebar(location = "bottomleft", x.min = 35, x.max = 60,
  #                y.min = -30, y.max = 5, dist = 600, dd2km = TRUE, 
  #                model = "WGS84", st.dist = 0.02, st.size = 4)+
  # labs(x = NULL, y = NULL)

#TODO clip shapefile so it loads reasonably 
#TODO lower opacity of circles so coast is visible

