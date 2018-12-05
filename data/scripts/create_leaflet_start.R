rm(list = ls())

library(leaflet)
library(mapview)

load('data/grid_geometry.rdata')
load('data/rasters/RCP85_Wheat_2050.rdata')

# grid_geometry <- st_transform(grid_geometry, crs = 3857)
bbox <- st_bbox(grid_geometry)
box_coords <- rbind(bbox[c(1,2)],
                    bbox[c(1,4)],
                    bbox[c(3,4)],
                    bbox[c(3,2)],
                    bbox[c(1,2)])
bbox_poly <- st_polygon(list(box_coords))
plot(bbox_poly)


(L <- leaflet(data = bbox_poly) %>%
  addTiles() %>%
  setView(lng = -1, lat = 53, zoom = 6) %>%
  addMapPane("base_grid", zIndex = 410) %>%
  addMapPane("selected", zIndex = 400) %>%
  addFeatures(bbox_poly, weight = 1, color = '#000000',
              fillOpacity = 0,
              opacity = 0.1,
              smoothFactor = 1)) #%>%
  # addFeatures(grid_geometry, weight = 1, color = '#000000',
  #             fillOpacity = 0,
  #             opacity = 0.1,
  #             layerId = ~grid_code,
  #             smoothFactor = 1, 
  #             options = pathOptions(pane = "base_grid"),
  #             highlightOptions = highlightOptions(
  #               weight = 5,
  #               color = "#ff0000",
  #               fillOpacity = 0,
  #               bringToFront = TRUE)) %>%
  # addRasterImage(ras, layerId = 'raster',
  #                opacity = 0.5,
  #                project = TRUE)


save(L, file = 'data/leaflet_start.rdata')