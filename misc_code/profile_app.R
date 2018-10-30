# profile the app
library(shiny)
library(profvis)

profvis({
  runApp()
})
