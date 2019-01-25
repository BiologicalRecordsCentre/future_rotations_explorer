
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
library(DT)
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

shinyServer(function(input, output, session) {
  
  # Load raster to display ----
  ras <- reactive({
    
    if(is.character(input$rcp) & is.character(input$yr)){
    
      # Lookup rcp model
      rcp_model <- rcp_models_lookup$label[rcp_models_lookup[lang()] == input$rcp]
      
      # Lookup crop
      if(is.null(input$crop)){
        crop <- crops_names$label[8]
      } else {
        crop <- crops_names$label[grep(as.character(input$crop),
                                       crops_names[,lang()])]
      }
      
      print(crop)
      path <- paste0('data/rasters/',
                     paste(rcp_model, crop, input$yr, sep = '_'),
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
  
  # Add raster on change ----
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
  
  # Add polygon on select ----
  observeEvent({
    input$map_shape_click
  }, {
    
    if(selected_grid() != -999){
      
      print(selected_grid())
    
      coords <- st_coordinates(grid_geometry[grid_geometry$grid_code == selected_grid(),])
      
      leafletProxy("map") %>%
        removeShape("-999") %>%
        addPolylines(lng = coords[,1], lat = coords[,2],
                     layerId = "-999",
                     opacity = 1, fillColor = 'black')
    } else {
      leafletProxy("map") %>%
        removeShape("-999")
    }
  })
    
  # Observe clicks on the map ----
  click_point <- eventReactive(input$map_shape_click, { 
    
    ic <- input$map_shape_click
    p <- data.frame(lat = ic$lat, lng = ic$lng)
    sf::st_as_sf(p, coords = c('lng', 'lat'), crs = 4326)
    
  })

  selected_grid <- reactive({ 
    
    p <- click_point()
    suppressWarnings({is <- st_intersection(p, grid_geometry)$grid_code})
    if(length(is) > 0){
      return(is)
    } else {
      return(-999)
    }
    
  })
  
  # Selected grid cell ----
  output$selected_grid <- renderPrint({
    print(selected_grid())
  })
  
  # read in the data needed ----
  selected_data_all <- reactive({
    
    # RCP model can be 'RCP45' or 'RCP85'
    rcp_model <- rcp_models_lookup$label[rcp_models_lookup[lang()] == input$rcp]
    
    # Year can be '2030' or '2050'
    year <- input$yr
    
    data_path <- file.path('data', rcp_model, paste0('ch2015.', year, '.rdata'))
    
    # Load data and return it
    load(data_path)
    selected_data
    
  })
  
  # data for only the cell selected ----
  selected_data_cell <- reactive({
    
    if(!is.null(selected_grid()) & selected_grid() != -999){
      SEL <- selected_data_all()
      SEL[SEL$grid_code == selected_grid(),]
    } else {
      NULL
    }
    
  })

  # Get crop labels for plotting ----
  crop_label <- reactive({
    crop_lang <- crop_seq()
    crop_label <- crops_names$label[match(crop_lang, crops_names[,lang()])]
  })
  
  # average yield ----
  av_change <- reactive({
    dat <- selected_data_cell()
    if(!is.null(dat)){  
      av_change <- mean(dat$value[match(crop_label(), dat$crop)],
                        na.rm = TRUE)
    } else {
      NULL
    }
  })

  # Plot data for crops ----
  output$barplot <- renderPlotly({
  
    dat <- selected_data_cell()
    # str(dat)
    
    if(!is.null(dat)){  

      # crop_lang <- crop_seq()
      # crop_label <- crops_names$label[match(crop_lang, crops_names[,lang()])]

      # Add year to label
      lab <- paste(paste(text$year[[lang()]],
                         1:length(crop_seq())),
                   ' - ',
                   crop_seq())
      # av_change <- mean(dat$value[match(crop_label(), dat$crop)],
      #                   na.rm = TRUE)
      bar_plot <- plot_ly(
        x = lab,
        y = dat$value[match(crop_label(), dat$crop)],
        name = text$change_in_yield[[lang()]],
        type = "bar") %>%
        config(displayModeBar = F) %>%
        add_segments(x = lab[1], xend = tail(lab, 1),
                     y = av_change(), yend = av_change()) %>%
        layout(showlegend = FALSE) %>%
        layout(xaxis=list(fixedrange = TRUE)) %>% 
        layout(yaxis=list(fixedrange = TRUE))
      
      # bar_plot <- ggplot(data = dat,
      #                    aes(x = lab,
      #                        y = dat$value[match(crop_label, dat$crop)])) +
      #             geom_bar(stat="identity")
      
    } else {
      
      NULL
      
    }
  })
  
  # Language selection ----
  lang <- reactiveVal(value = 'fr')
  observeEvent(input$en, {lang('en')})
  observeEvent(input$fr, {lang('fr')})

  # Render text elements ----
  output$title <- renderUI({
    h1(text$title[[lang()]])
  })
  output$choose_crops <- renderUI({
    h3(text$choose_crops[[lang()]])
  })
  
  # Select the number of years you want ----
  output$nyr_select <- renderUI({
    selectInput('nyr',
                label = text$nyr[[lang()]],
                choices = 2:6, width = 120,
                selectize = FALSE)   
  })
  output$nyr <- renderText({input$nyr})
  
  # Select RCP model ----
  output$rcp_select <- renderUI({
    selectInput('rcp',
                label = text$rcp_label[[lang()]],
                choices = text$rcp_models[[lang()]],
                selected = text$rcp_models[[lang()]][2],
                width = "200px",
                selectize = FALSE)
  })
  
  # Select the year we are projecting to ----
  output$year_select <- renderUI({
    selectInput('yr',
                label = text$forecast_year[[lang()]],
                choices = c('2030', '2050'),
                selected = '2050',
                width = "200px",
                selectize = FALSE)
  })
  
  # Tab titles ----
  output$rotation_title <- renderText({
    text$rotation_explorer[[lang()]]
  })
  output$map_title <- renderText({
    text$map[[lang()]]
  })
  output$about_title <- renderText({
    text$about[[lang()]]
  })
  output$compare_title <- renderText({
    text$compare[[lang()]]
  })
  
  # Tab descriptions ----
  output$rotation_desc <- renderText({
    text$rotation_desc[[lang()]]
  })
  output$map_desc <- renderText({
    text$map_desc[[lang()]]
  })
  
  # About text ----
  output$about_text <- renderUI({
    HTML(text$about_text[[lang()]])
  })
  
  # Build and display the crop selection boxes ----
  output$crop_boxes <- renderUI({
    if(!is.null(input$nyr)){
      tags$div(id = 'crop_boxes',
        lapply(1:input$nyr, function(i) {
          tags$div(class = "divleft",
                   selectInput(paste0('crop', i),
                               label = paste(text$year[[lang()]], i),
                               choices = crops_names[,lang()],
                               width = 150,
                               selected = crops_names[,lang()][8],
                               selectize = FALSE)
          )
        }),
        style = paste("overflow: hidden")
      )
    } else {
      NULL
    }
  })
  
  # Build map crop selection box ----
  output$map_crop_select <- renderUI({
    selectInput('crop',
                label = text$crop[[lang()]],
                choices = crops_names[,lang()],
                selected = crops_names[,lang()][8],
                selectize = FALSE)
  })
  
  # Create timeline of crops ----
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

  # Create the table of comparisons ----
  output$add_compare <- renderUI({
    actionButton("compare", text$add_to_compare[[lang()]])
  })
  
  output$clear_data <- renderUI({
    actionButton("clear_data", text$clear_data[[lang()]])
  })
  
  df_compare <- reactiveVal()
  
  observeEvent(input$compare, {
    # Sys.sleep(0.2)
    # create data to add
    df_temp <- data.frame(location = selected_grid(),
                          to_year = input$yr,
                          model = input$rcp,
                          Y1 = crop_label()[1],
                          Y2 = crop_label()[2],
                          Y3 = crop_label()[3],
                          Y4 = crop_label()[4],
                          Y5 = crop_label()[5],
                          Y6 = crop_label()[6],
                          yield_change = round(av_change(), digits = 2))
    
    names(df_temp) <- text$table_names[[lang()]]
    
    if(is.null(df_compare())){
      
      df_compare(unique(df_temp))
    
    } else {
      
      df_compare <- df_compare()
      
      if(!identical(colnames(df_compare), colnames(df_temp))){
        colnames(df_compare) <- colnames(df_temp)
      }
      
      # add this and keep only unique values
      df_temp <- unique(rbind(df_compare, df_temp))
      
      df_compare(df_temp)
      
    }
    showNotification('Data has been added to comparison',
                     duration = 3, type = 'message')
  })

  observeEvent(input$clear_data,{
    df_compare(NULL)
  })
  
  output$compareDT <- DT::renderDataTable({
    # remove columns with no data
    Filter(function(x)!all(is.na(x)), df_compare())
  })
  
})
