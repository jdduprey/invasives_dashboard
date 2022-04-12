#===================================================
# Joe Duprey
# Invasive's dashboard
# Takes monthly dataframe of invasive richness as input and plots invasion rate 
# over time across Puget Sound
#===================================================
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(sf)

# load shapefiles, metadata
NW_coast <- read_sf("./data/ne_10m_coastline/ne_10m_coastline.shp")
site_meta <- read.csv("./data/site_meta.csv")
invasion_data <- read.csv("./data/monthly_invasion_data.csv")

# link site data with invasives data
full_map_df <- left_join(invasion_data, site_meta)

# slider converter (could go in different doc)
slider_tics <- c("March 2017", "May 2017", "June 2017",
                "July 2017", "August 2017", "September 2017",
                "October 2017", "November 2017", "January 2018",
                "March 2018", "May 2018", "July 2018")
int_mons <- c(3, 5, 6, 7, 8, 9, 10, 11, 1, 3, 5, 7)
int_yrs <- c(2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2018, 2018, 2018, 2018)

slider_converter <- data.frame(slider_tics, int_mons, int_yrs)

one_month_df <- full_map_df

# UI
#===================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tabsetPanel(
    # puget sound map tab
    tabPanel("Invasives Map",
             sliderTextInput(inputId = "time_step", 
                         label = "Select month and year.", 
                         choices = c("March 2017", "May 2017", "June 2017",
                                     "July 2017", "August 2017", "September 2017",
                                     "October 2017", "November 2017", "January 2018",
                                     "March 2018", "May 2018", "July 2018"),
                         selected = c("March 2017")),
             
            
             plotOutput("map")), 
             ))



# SERVER
#===================================================
server <- function(input, output) {
  
  output$map <- renderPlot({
    
    selected_month <- slider_converter$int_mons[which(slider_converter$slider_tics==input$time_step)]
    selected_year <- slider_converter$int_yrs[which(slider_converter$slider_tics==input$time_step)]
    
    # store map with plot to variable
    p <- ggplot() + 
      geom_sf(data=NW_coast, col = 1, fill = "ivory") +
      coord_sf(xlim = -c(125, 122), ylim = c(47,49)) +
      geom_point(data = subset(one_month_df, year == selected_year & month == selected_month), 
                 aes(x = long, y = lat, color=prop_nn), size = 8, alpha = 0.8) +
      scale_color_distiller(palette = "RdYlBu", limits = c(0, 0.18)) +
      geom_text(data = subset(one_month_df, year == selected_year & month == selected_month), 
                aes(x = long, y = lat, label=round(prop_nn,2)), size = 3) +
      #ggrepel::geom_text_repel(data = one_month_df, aes(x = long, y = lat, label = site)) +
      theme_bw() + 
      theme(panel.background = element_rect(fill = "white"),
            axis.text = element_text(size = 8, colour = 1, face = "bold"),
            panel.grid = element_line(colour = NA)) 
    
    p
    
  })
  
  
}

shinyApp(ui = ui, server = server)
