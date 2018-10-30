# Function to return text, makes translation easy

text <- list(title = list(en = 'Future Rotations Explorer',
                          fr = 'Explorateur de rotations futures'),
             model = list(en = 'Model three',
                          fr = 'Model trois'),
             this_thing = list(en = 'This',
                               fr = 'Ce'),
             crops = list(en = c('Wheat', 'Oilseed rape', 'Potato'),
                          fr = c('Ble', 'Colza', 'Pomme de terre')),
             year = list(en = 'Year',
                         fr = 'Annee'),
             nyr = list(en = 'Number of years',
                        fr = "Nombre d'annee"),
             choose_crops = list(en = 'Choose crops',
                                 fr = 'Choisissez la culture'),
             building_map = list(en = 'Building the map',
                                 fr = 'Construire le carte'),
             complete = list(en = 'Complete',
                             fr = 'Achevée')
             )


crops_names <- data.frame(label = c("FieldBeans", "Maize", "Potato", "Rye", "SpringBarley", "SugarBeet", 
                              "Sunflower", "Wheat", "WinterRapeseed", "Rice", "other"),
                    en = c("FieldBeans", "Maize", "Potato", "Rye", "SpringBarley", "SugarBeet", 
                           "Sunflower", "Wheat", "WinterRapeseed", "Rice", "other"),
                    fr = c("FieldBeans", "Maize", "Potato", "Rye", "SpringBarley", "SugarBeet", 
                           "Sunflower", "Wheat", "WinterRapeseed", "Rice", "other"))
