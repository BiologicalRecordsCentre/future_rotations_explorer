rm(list = ls())

library(sf)
library(mapview)
library(leaflet)
library(tidyverse)


load('data/final1.Rdata')

geoms <- unique(final1[,c('geometry','grid_code')])
geoms <- st_transform(geoms, crs = 4326)

L <- leaflet() %>%
  addTiles() %>%
  setView(lng = -1, lat = 53, zoom = 6) %>%
  addFeatures(geoms, weight = 1, color = '#000000',
              fillOpacity = 0.2,
              highlightOptions = highlightOptions(
                            weight = 5,
                            color = "#ff0000",
                            fillOpacity = 0.7,
                            bringToFront = TRUE))

grid_geometry <- geoms

save(grid_geometry, file = 'data/grid_geometry.rdata')