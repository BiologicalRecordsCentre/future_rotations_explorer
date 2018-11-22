
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
library(rgdal)
library(sf)
# library(promises)
# library(future)
# plan(multisession)

options(encoding = "UTF-8")

# read in text
source('text.R', encoding = "UTF-8")

# read other functions
for(i in list.files(path = 'scripts/', pattern = '.R$', full.names = TRUE)) source(i)

# read in grid geometry
load('data/grid_geometry.rdata')
load('data/leaflet_start.rdata')

shinyServer(function(input, output) {
  
  # Load raster to display
  ras <- reactive({
    
    if(is.character(input$rcp) & is.character(input$yr)){
    
      # Lookup rcp model
      rcp_model <- rcp_models_lookup$label[rcp_models_lookup[lang()] == input$rcp]
      
      path <- paste0('data/rasters/',
                     paste(rcp_model, input$crop, input$yr, sep = '_'),
                     '.rdata')
      load(path)
      ras
      
    } else {

      load('data/rasters/RCP85_Wheat_2050.rdata')
      ras
      
    }
    
  })
  
  output$map <- renderLeaflet({
    L
  })
  
  # Add raster on change
  observeEvent({
    input$rcp
    input$crop
    input$yr
  }, {

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    isolate(progress$set(message = 'Updating...', value = 0.1))
    
    pal <- colorNumeric('Spectral', raster::values(ras()),
                        na.color = "transparent")
    
    leafletProxy("map") %>%
          removeTiles('raster') %>%
          addRasterImage(ras(), layerId = 'raster',
                         colors = pal,
                         opacity = 0.5,
                         project = FALSE) %>%
          removeControl(layerId = 'legend') %>%
          addLegend_decreasing(position = 'topleft', layerId = 'legend',
                               pal = pal, values = raster::values(ras()),
                               labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                               title = '% Change', decreasing = TRUE)
    
    isolate(progress$set(message = text$complete[[lang()]], value = 1))

  })
  
  # Add polygon on select
  observeEvent({
    input$map_shape_click
  }, {
    
    if(selected_grid() != -999){
    
      coords <- st_coordinates(grid_geometry[grid_geometry$grid_code == selected_grid(),])
      
      leafletProxy("map") %>%
        removeShape("-999") %>%
        addPolylines(lng = coords[,1], lat = coords[,2],
                     layerId = "-999",
                     opacity = 1, fillColor = 'black')
        # removeShape('selected_polygon') %>%
        # addPolygons(lng = coords[,1], lat = coords[,2],
        #             layerId = 'selected_polygon',
        #             opacity = 1, fillColor = 'black')
    }
  })
    
  # Observe clicks on the map
  selected_grid <- eventReactive(input$map_shape_click, { 
    
    p <- input$map_shape_click
    print(p)
    as.numeric(p$id)
    
    
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
    
    if(!is.null(selected_grid()) & selected_grid() != -999){
      SEL <- selected_data_all()
      SEL[SEL$grid_code == selected_grid(),]
    } else {
      NULL
    }
    
  })
  
  # Plot data for crops
  output$barplot <- renderPlotly({
  
    dat <- selected_data_cell()
    
    if(!is.null(dat)){  
    
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
      
    } else {
      
      NULL
      
    }
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
  
  # Select RCP model
  output$rcp_select <- renderUI({
    selectInput('rcp',
                label = text$rcp_label[[lang()]],
                choices = text$rcp_models[[lang()]],
                selected = text$rcp_models[[lang()]][2])
  })
  
  # Select the year we are projecting to
  output$year_select <- renderUI({
    selectInput('yr',
                label = text$forecast_year[[lang()]],
                choices = c('2030', '2050'),
                selected = '2050')
  })
  
  # Tab titles
  output$rotation_title <- renderText({
    text$rotation_explorer[[lang()]]
  })
  output$map_title <- renderText({
    text$map[[lang()]]
  })
  output$about_title <- renderText({
    text$about[[lang()]]
  })

  # Build and display the crop selection boxes
  output$crop_boxes <- renderUI({
    if(!is.null(input$nyr)){
      tags$div(
        lapply(1:input$nyr, function(i) {
          tags$div(class = "divleft",
                   selectInput(paste0('crop', i),
                               label = paste(text$year[[lang()]], i),
                               choices = crops_names[,lang()],
                               width = 150,
                               selected = 'Wheat')
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
