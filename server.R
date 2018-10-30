
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(googleVis)
library(mapview)
library(plotly)

# read in text
source('text.R')

# read in grid geometry
load('data/grid_geometry.rdata')

shinyServer(function(input, output) {
  
  # Create the map
  output$map <- renderLeaflet({

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    isolate(progress$set(message = text$building_map[[lang()]], value = 0.1))
    
    L <- leaflet(data = grid_geometry) %>%
            addTiles() %>%
            setView(lng = -1, lat = 53, zoom = 6) %>%
            addFeatures(grid_geometry, weight = 1, color = '#000000',
                        fillOpacity = 0.2,
                        layerId = ~grid_code,
                        smoothFactor = 1, 
                        highlightOptions = highlightOptions(
                          weight = 5,
                          color = "#ff0000",
                          fillOpacity = 0.7,
                          bringToFront = TRUE))
    
    isolate(progress$set(message = text$complete[[lang()]], value = 1))
    
    L 
    
  })
    
  # Observe clicks on the map
  selected_grid <- eventReactive(input$map_shape_click, { 
    
    p <- input$map_shape_click
    print(p)
    p$id
    
  })

  # Selected grid cell
  output$selected_grid <- renderPrint({
    print(selected_grid())
  })
  
  # read in the data needed
  selected_data_all <- reactive({
    
    load('data/RCP45/ch2015.2050.rdata')
    selected_data
    
  })
  
  # data for only the cell selected
  selected_data_cell <- reactive({
    
    if(!is.null(selected_grid())){
      SEL <- selected_data_all()
      SEL[SEL$grid_code == selected_grid(),]
    } else {
      NULL
    }
    
  })
  
  # Plot data for crops
  output$barplot <- renderPlotly({
    
    dat <- selected_data_cell()
    crop_lang <- crop_seq()
    crop_label <- crops_names$label[match(crop_lang, crops_names[,lang()])]
    
    # Add year to label
    lab <- paste(paste('Year', 1:length(crop_lang)), ' - ', crop_lang)
    
    bar_plot <- plot_ly(
      x = lab,
      y = dat$value[match(crop_label, dat$crop)],
      name = "Change in yield",
      type = "bar"
    )
    
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
                               choices = crops_names[,lang()],
                               width = 150)
          )
        }),
        style = paste("height:", ifelse(input$nyr > 4, '175px', '90px'))
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
      if(length(crop_seq()) == length(1:input$nyr)){
        data.frame(Crop = crop_seq(),
                   start = (1:input$nyr) - 1,
                   end = 1:input$nyr)
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  # output$timeline <- renderGvis({
  #   if(!is.null(TL())){
  #     gvisTimeline(data = TL(),
  #                  rowlabel = "Crop",
  #                  start = "start",
  #                  end = "end",
  #                  options = list(timeline = "{groupByRowLabel:true,}",
  #                                 width = '726px',
  #                                 enableInteractivity = FALSE))
  #   } else {
  #     NULL
  #   }
  # })
  
})
