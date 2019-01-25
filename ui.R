
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(leaflet)
library(shiny)
library(googleVis)
library(plotly)
library(raster)

options(encoding = "UTF-8")

source('text.R', encoding = "UTF-8")

shinyUI(bootstrapPage(
  
  tags$head(
    includeCSS("style.css")
  ),
  
  # Allow map to be full screen
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # Add map
  leafletOutput("map", width = '100%', height = '100%'),
  
  # Add language selection flags
  absolutePanel(id = "lang_select", class = "panel panel-default",
                fixed = TRUE, draggable = FALSE, top = 5,
                left = "auto", right = 20, bottom =  'auto',
                actionLink("en", tags$img(src = 'flags/UK.png')),
                actionLink("fr", tags$img(src = 'flags/France.png'))),
  
  # Add logos
  absolutePanel(id = "logos", class = "panel panel-default",
                fixed = TRUE, draggable = FALSE, top = 'auto',
                left = 5, right = 'auto', bottom =  -15,
                img(src = 'logos/facce_small.png', align = 'left')),
  
  # Add tools pane
  absolutePanel(id = "tools_pane", class = "panel panel-default",
                fixed = TRUE, draggable = TRUE, top = 45,
                left = "auto", right = 20, bottom =  "auto",
                width = "auto",
                fluidRow(
                  column(6, uiOutput('rcp_select')),
                  column(6, uiOutput('year_select'))),
                tabsetPanel(
                 tabPanel(textOutput('rotation_title'),
                          textOutput('rotation_desc'),
                          fluidRow(
                            column(6, uiOutput('nyr_select')),
                            column(6, uiOutput('add_compare'))),
                          #htmlOutput('choose_crops'),
                          uiOutput('crop_boxes'),
                          #dataTableOutput('DT'),
                          # htmlOutput('timeline'),
                          #textOutput('selected_grid'),
                          plotlyOutput("barplot")
                          ),
                 tabPanel(textOutput('map_title'),
                          textOutput('map_desc'),
                          uiOutput('map_crop_select')),
                 tabPanel(textOutput('about_title'),
                          htmlOutput('about_text'),
                          img(src = 'logos/facce_small.png', align = 'left'),
                          img(src = 'logos/PREAR logo_dark_small.png', align = 'left')),
                 tabPanel(textOutput('compare_title'),
                          DT::dataTableOutput('compareDT'),
                          uiOutput('clear_data'))
               ))
))