# -------------------------
# Start of Shiny App `Ruben`
# -------------------------
# This is the user-interface definition of a Shiny web application.

# Attach Packages 
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(USAboundariesData)
library(USAboundaries)
library(devtools)
library(leaflet)
library(shinythemes)
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
#library(visNetwork)
library(rintrojs)
library(gt)

# ---------------------------------------
# Support functions:
# --------------------------------------

# Read in data and create `sf` file:
fire_mt <- read_csv(here::here("data", "montana_fire_data.csv")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)%>% 
  mutate(fire_year = as.numeric(fire_year))

# Dateframe with the top 1k fires by size:
top_1k_fire_size <- top_n(fire_mt, 1000, fire_mt$fire_size)

# Change `NA` values of names with `Unknown`: 
top_1k_fire_size$fire_name[is.na(top_1k_fire_size$fire_name)] <- "Unknown"

# Dateframe with `fire_name` in alphabeticla order from top 1k for searching:
fire_name_df <- top_1k_fire_size[order(top_1k_fire_size$fire_name),]

# Dataframe with `source_reporting_unit_name` in alphabetical order for searching:
fire_unit_df <- fire_mt[order(fire_mt$source_reporting_unit_name),]

# Color palette for cause map:

pal <- colorFactor(
  palette = "YlOrRd",
  domain = fire_mt$fire_size)

# Make an icon for our map:
icons <- awesomeIcons(
  icon = 'fire',
  iconColor = '#FFFFFF',
  library = 'fa',
  markerColor = pal)

# ------------------------------------
# ui.R
# -----------------------------------

ui <- navbarPage(title = img(src="Bren_logo.png", height = "34px"),
                 id = "navBar",
                 collapsible = TRUE,
                 inverse = FALSE,
                 windowTitle = "Montana Fire Data ShinyApp",
                 position = "fixed-top",
                 header = tags$style(
                   ".navbar-right {
                       float: right !important;
                       }",
                   "body {padding-top: 75px;}"),
                 
# ----------------------------------------------------------
# About Panel:
# -----------------------------------------------------
                 
                 tabPanel("About", icon = icon("bars"),
                          includeHTML("./www/test.html")),
                 
# ---------------------------------------------
# Fire Cause Panel:
# -----------------------------------------

                 tabPanel("Fires Causes", value = "cause", icon = icon("fire"),
                            sidebarLayout(
                            sidebarPanel(width = 3, align = 'left',
                                         checkboxGroupInput(inputId = "mt_cause",
                                                               label = "Choose a fire cause",
                                                               choices = c(unique(fire_mt$stat_cause_descr)),
                                                               selected = "Campfire",
                                                               inline = FALSE),
                                         sliderInput(inputId = "fire_year",
                                                     label = "Year",
                                                     min(fire_mt$fire_year),
                                                     max(fire_mt$fire_year),
                                                     sep = "",
                                                     value = range(fire_mt$fire_year),
                                                     step = 1)
                                         ),
                            
                            mainPanel(width = 8,
                                      fluidRow(
                                      leafletOutput("cause_map", height = 680))
                                      )
                            )
                          ),
                 
# ---------------------------------------
# Search by Fire Panel:
# ---------------------------------------

                 tabPanel("Search by Fire", value = "search", icon = icon("search"),
                          sidebarLayout(
                            sidebarPanel(width = 3, align = 'left',
                                         selectInput(inputId = "mt_fire_name", label = "Choose a Fire", 
                                                     choices = (fire_name_df$fire_name), 
                                                     selected = "Treasure County Complex")),
                            mainPanel(width = 8,
                                      fluidRow(leafletOutput("fire_map", height = 450)
                                        ),
                                      fluidRow(gt_output("fire_summary")
                                        )
                                      )
                            )
                          ),
                 
# -------------------------------------
# Containment Time Panel:
# ---------------------------------------

                 tabPanel("Containment Time", value = "time", icon = icon("clock"),
                          sidebarLayout(
                            sidebarPanel(width = 3, align = 'left',
                                         selectInput("reporting_unit", 
                                                     "Choose a reporting source:",
                                                     choices = c(
                                                       unique(fire_unit_df$source_reporting_unit_name)))
                                         ),
                            mainPanel(width = 8,
                                      fluidRow(
                                        plotOutput(outputId = "burn_time_plot", height = 680)
                                        )
                                      )
                            )
                          )
)

# ---------------------------------------------------------------
# -------------------------------------------------------------
# server.R
# ---------------------------------------------------------------
# --------------------------------------------------------------

server <- function(input, output){
 
# -------------------------------------
# Cause Map Output:
# ------------------------------------
  initial_zoom = 4
  
  filtered_mt_cause <- reactive({
    fire_mt %>%
      filter(stat_cause_descr == input$mt_cause) %>% 
      filter(fire_year >= input$fire_year[1] & fire_year <= input$fire_year[2])
    })
  
  output$cause_map <- renderLeaflet({
    leaflet(data = filtered_mt_cause()) %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Stamen Terrain", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Carto DB Dark Matter", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri NatGeo World Map ", options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(icon = icons,
                 label = ~fire_name,
                 popup = paste0("<b>Name: </b>", filtered_mt_cause()$fire_name, "<br>",
                                "<b>Year: </b>", filtered_mt_cause()$fire_year, "<br>",
                                "<b>Cause: </b>", filtered_mt_cause()$stat_cause_descr, "<br>",
                                "<b>Size (acres): </b>", filtered_mt_cause()$fire_size, "<br>",
                                "<b>Discovery Date: </b>", filtered_mt_cause()$full_dis_date, "<br>",
                                "<b>Burn Time (D H:M:S): </b>", filtered_mt_cause()$burn_time, "<br>"
                                 ),
                 clusterOptions = markerClusterOptions()) %>% 
      addLayersControl(
        baseGroups = c("Stamen Terrain", "Open Street Map", "Carto DB Dark Matter", "Esri NatGeo World Map"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
      
  })
  
# ----------------------------------------
# Fire Name Output:
# ---------------------------------------
  
  fire_name_map <- reactive({
    fire_name_df %>% 
      filter(fire_name == input$mt_fire_name)
  }) 
  
  output$fire_map <- renderLeaflet({
    leaflet(data = fire_name_map()) %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Stamen Terrain", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Carto DB Dark Matter", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri NatGeo World Map ", options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(icon = icons,
                 label = c("Size (acres):", ~fire_size)) %>% 
      addLayersControl(
      baseGroups = c("Stamen Terrain", "Open Street Map", "Carto DB Dark Matter", "Esri NatGeo World Map"),
        position = c("topleft"),
      options = layersControlOptions(collapsed = TRUE)
      )
  })
  
# -------------- Fire Summary ------------
  
  fire_name_stats <- reactive({
    fire_mt %>%  
      select(fire_name, fire_year, stat_cause_descr, fire_size, source_reporting_unit_name, burn_time, geometry) %>%
      filter(fire_name == input$mt_fire_name) %>% 
      gt() %>% 
      tab_header(
        title = "Fire Summary") %>% 
      cols_label(
        fire_name = "Name of Fire",
        fire_year = "Year",
        stat_cause_descr = "Cause of Fire",
        fire_size = "Fire Area (acres)",
        source_reporting_unit_name = "Name of Reporting Source",
        burn_time = "Burn Time (D H:M:S)",
        geometry = "Lat Long Coordinates") %>% 
      tab_options(
        table.width = pct(100))
  })
  
  output$fire_summary <- render_gt({
    expr = fire_name_stats()
  })
  
  # ---------------------------------
  # Burn Time Output:
  # --------------------------------
  
  fire_time <- fire_unit_df %>%
    mutate(burn_time = difftime(full_cont_date,full_dis_date,units = "mins"))
  
  unit_burn_time <- reactive({
    fire_time %>% 
      filter(source_reporting_unit_name == input$reporting_unit) %>% 
      group_by(fire_year) %>% 
      summarize(
        mean_burn_time = round(mean(burn_time),2)
      )
  })
  
  output$burn_time_plot <- renderPlot({
    ggplot(data = unit_burn_time(), aes(x = fire_year, y = mean_burn_time)) +
      geom_line(color = "red",
                size = 1) + 
      geom_point(size = 2) + 
      geom_ribbon(aes(ymin = 0, 
                      ymax = mean_burn_time), 
                  fill = "red", 
                  alpha = 0.1) +
      scale_x_continuous(breaks = seq(1992, 2015, by = 2),
                         expand = c(0, 0)) + 
      labs(title = "Montana Fire Containment Times",
           subtitle = "By Land Management Reporting Agency (1992-2015)",
           caption = "Time series of how Montana wildfire containment times (in hours) have fluctuated annually between 1992-2015 within different land manangement units.",
           x = "Year",
           y = "Containment Time (D H:M:S)") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 14))
    
  })
  
}

# ----------------------------
# End of app.R
# ---------------------------

shinyApp(ui = ui, server = server)

# ------------------------------------------
# ------------------------------------------
# END
# ------------------------------------------
# ------------------------------------------



