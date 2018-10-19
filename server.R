
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(googleVis)

# read in text
source('text.R')

shinyServer(function(input, output) {
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -1, lat = 53, zoom = 6)
  })

  # Language selection
  lang <- reactiveVal(value = 'en')
  observeEvent(input$en, {lang('en')})
  observeEvent(input$fr, {lang('fr')})

  # Render text elements
  output$title <- renderUI({
    h1(text$title[[lang()]])
  })
  output$choose_crops <- renderUI({
    h3(text$choose_crops[[lang()]])
  })
  
  
  # Select the number of years you want
  output$nyr_select <- renderUI({
    selectInput('nyr',
                label = text$nyr[[lang()]],
                choices = 2:6, width = 120)   
  })
  output$nyr <- renderText({input$nyr})
  
  # Build and display the crop selection boxes
  output$crop_boxes <- renderUI({
    if(!is.null(input$nyr)){
      tags$div(
        lapply(1:input$nyr, function(i) {
          tags$div(class = "divleft",
                   selectInput(paste0('crop', i),
                               label = paste(text$year[[lang()]], i),
                               choices = text$crops[[lang()]],
                               width = 150)
          )
        }),
        style = paste("height:", ifelse(input$nyr > 3, '175px', '90px'))
      )
    } else {
      NULL
    }
  })
  
  # Create timeline of crops
  crop_seq <- reactive({
    if('crop1' %in% names(input)){
    all <- lapply(1:input$nyr,
                  function(i){
      input[[c(paste0('crop', i))]]
    })
    unlist(all)
    } else {
      NULL
    }
  })
  
  TL <- reactive({
    if(!is.null(crop_seq())){
      data.frame(Crop = crop_seq(),
                 start = (1:input$nyr) - 1,
                 end = 1:input$nyr)
    } else {
      NULL
    }
  })

  output$timeline <- renderGvis({
    if(!is.null(TL())){
      gvisTimeline(data = TL(),
                   rowlabel = "Crop",
                   start = "start",
                   end = "end",
                   options = list(timeline = "{groupByRowLabel:true,}",
                                  width = '726px',
                                  enableInteractivity = FALSE))
    } else {
      NULL
    }
  })
  
})
