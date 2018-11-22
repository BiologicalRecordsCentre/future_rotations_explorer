# Create rasters
rm(list = ls())

library(sf)
library(fasterize)
library(raster)
library(leaflet)

load('data/final1.Rdata')
# it isn't lat long (which it says it is)
final1 <- st_transform(final1, crs = 4326)

# Create a template raster for rasterisation
bbox <- st_bbox(final1)
template <- raster::raster(ext = extent(bbox[c(1,3,2,4)]),
                           nrow = ceiling((bbox[3] - bbox[1])*30),
                           ncol = ceiling((bbox[4] - bbox[2])*30))

rpcs <- c('RCP45', 'RCP85')
crops <- unique(final1$crop)[unique(final1$crop) != 'other']

for(rpc in rpcs){
  
  for(crop in crops){
    
    for(yr in c("ch2015.2030", "ch2015.2050")){
    
      cat(paste(rpc, crop, gsub('^.*\\.', '', yr), sep = '_'), '\n')
      
      dat <- final1[final1$RPC == rpc &
                    final1$crop == crop &
                    final1$attribute == yr, ]
      
      ras <- fasterize(dat, template, field = 'value')
      ras <- trim(ras)
      ras <- projectRasterForLeaflet(x = ras, method = 'ngb')
      
      save(ras, file = paste0('data/rasters/',
                              paste(rpc, crop, gsub('^.*\\.', '', yr), sep = '_'),
                              '.rdata'))
        
    } 
    
  }
  
}

plot(ras)
