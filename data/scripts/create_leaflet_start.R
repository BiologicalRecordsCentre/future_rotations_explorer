rm(list = ls())

library(leaflet)
library(mapview)

load('data/grid_geometry.rdata')
load('data/rasters/RCP85_Wheat_2050.rdata')

# grid_geometry <- st_transform(grid_geometry, crs = 3857)

L <- leaflet(data = grid_geometry) %>%
  addTiles() %>%
  setView(lng = -1, lat = 53, zoom = 6) %>%
  addMapPane("base_grid", zIndex = 410) %>%
  addMapPane("selected", zIndex = 400) %>%
  addFeatures(grid_geometry, weight = 1, color = '#000000',
              fillOpacity = 0,
              opacity = 0.1,
              layerId = ~grid_code,
              smoothFactor = 1, 
              options = pathOptions(pane = "base_grid"),
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#ff0000",
                fillOpacity = 0,
                bringToFront = TRUE)) %>%
  addRasterImage(ras, layerId = 'raster',
                 opacity = 0.5,
                 project = TRUE)

L 

# save(L, file = 'data/leaflet_start.rdata')