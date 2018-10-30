# Split up the raw data into more managable pieces
rm(list = ls())

library(sf)

load('data/final1.Rdata')

st_geometry(final1) <- NULL

for(mod in unique(final1$RPC)){
  
  dir.create(path = paste0('data/', mod), showWarnings = FALSE)
  
  for(att in unique(final1$attribute)){
    
    cat(paste0('data/', mod, '/', att, '.rdata\n'))
    selected_data <- final1[final1$RPC == mod & final1$attribute == att, ]
    save(selected_data,
         file = paste0('data/', mod, '/', att, '.rdata'))
    rm(list = 'selected_data')
    
  }
}