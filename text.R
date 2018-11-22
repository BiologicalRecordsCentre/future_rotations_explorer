# Function to return text, makes translation easy

text <- list(rotation_explorer = list(en = 'Rotations Explorer',
                          fr = 'Explorateur de rotations'),
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
                             fr = 'Achevée'),
             rcp_label = list(en = 'Select climate model',
                              fr = 'Sélectionner le modèle climatique'),
             rcp_models = list(en = c('4.5 degree warming', '8.5 degree warming'),
                               fr = c('4.5 degrés de réchauffement', '8.5 degrés de réchauffement')),
             forecast_year = list(en = 'Select forecast year',
                                  fr = "Sélectionnez l'année de prévision"),
             map = list(en = 'Map',
                        fr = 'Carte'),
             about = list(en = 'About',
                          fr = 'Information')
             )


crops_names <- data.frame(label = c("FieldBeans", "Maize", "Potato", "Rye", "SpringBarley", "SugarBeet", 
                              "Sunflower", "Wheat", "WinterRapeseed"),
                    en = c("FieldBeans", "Maize", "Potato", "Rye", "SpringBarley", "SugarBeet", 
                           "Sunflower", "Wheat", "WinterRapeseed"),
                    fr = c("FieldBeans", "Maize", "Potato", "Rye", "SpringBarley", "SugarBeet", 
                           "Sunflower", "Wheat", "WinterRapeseed"))

rcp_models_lookup <- data.frame(label = c('RCP45', 'RCP85'),
                                en = c('4.5 degree warming', '8.5 degree warming'),
                                fr = c('4.5 degrés de réchauffement', '8.5 degrés de réchauffement'))

