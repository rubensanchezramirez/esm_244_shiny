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

# Read in data and create `sf` file:
fire_mt <- read_sf(here::here("data", "montana_fire_data.csv")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)%>% 
  mutate(fire_year = as.numeric(fire_year)) %>%
  mutate(fire_name = order(fire_name))

# Color palette for cause map:
pal <- colorFactor(
  palette = "YlOrRd",
  domain = fire_mt$stat_cause_descr)

# Make an icon for our map:
icons <- awesomeIcons(
  icon = 'free-code-camp',
  iconColor = '#FFFFFF',
  library = 'fa',
  markerColor = pal)

#get montana state and counties shapefile 
montana_counties <- us_counties(states = "Montana")

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
                 
                 tabPanel("About", icon = icon("bars")#,
                          #includeMarkdown("www/home.md")
                          ),
                 
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
                 
                 tabPanel("Search by Fire", value = "search", icon = icon("search"),
                          sidebarLayout(
                            sidebarPanel(width = 3, align = 'left',
                                         selectInput(inputId = "mt_fire_name", label = "Choose a Fire", 
                                                     choices = (fire_mt$fire_name), 
                                                     selected = "Treasure County Complex"),
                            ),
                            mainPanel(width = 8,
                                      fluidRow(
                                        plotOutput("fire_map", height = 500)
                                        )#,
                                      #fluidRow(
                                        #gt_output("fire_summary", height = 180)
                                        #)
                                      )
                            )
                          ),
                 
                 tabPanel("Containment Time", value = "time", icon = icon("clock"))
                 
                 )
server <- function(input, output){
  
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
  
  fire_name_map <- reactive({
    fire_mt %>% 
      filter(fire_name == input$mt_fire_name)
  }) 
  
  output$fire_map <- renderLeaflet({
    leaflet(data = fire_name_map()) %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Stamen Terrain", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Carto DB Dark Matter", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri NatGeo World Map ", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(icon = icon("fire"),
                 label = ~fire_size) %>% 
      addLayersControl(
      baseGroups = c("Stamen Terrain", "Open Street Map", "Carto DB Dark Matter", "Esri NatGeo World Map"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  #mt_gt <- reactive({
   # mt_date_combined %>%  
      #select(fire_name, fire_year, stat_cause_descr, fire_size, source_reporting_unit_name, final_dis_date, final_cont_date) %>% 
      #filter(fire_name == input$mt_fire_name) %>% 
      #gt() %>% 
      #tab_header(
        #title = "Fire Summary") %>% 
      #cols_label(
        #fire_name = "Name of Fire",
        #fire_year = "Year",
        #stat_cause_descr = "Cause of Fire",
        #fire_size = "Fire Area",
        #source_reporting_unit_name = "Name of Reporting Source",
        #final_dis_date = "Discovery Time",
        #final_cont_date = "Contained Time") %>% 
      #tab_options(
        #table.width = pct(100))
  #})
  
}
shinyApp(ui = ui, server = server)



