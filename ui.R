
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(leaflet)
library(shiny)
library(googleVis)

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
  
  # Add tools pane
  absolutePanel(id = "tools_pane", class = "panel panel-default",
                fixed = TRUE, draggable = FALSE, top = 45,
                left = "auto", right = 20, bottom =  30,
                width = '40%',
                htmlOutput('title'),
                uiOutput('nyr_select'),
                htmlOutput('choose_crops'),
                uiOutput('crop_boxes'),
                #dataTableOutput('DT'),
                htmlOutput('timeline'))
))