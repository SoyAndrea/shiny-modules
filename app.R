library(shiny)
library(ggplot2)
library(dplyr)
library(datos)

source("mod_select_location.R")
source("mod_outputs.R")

ui <- fluidPage(
  titlePanel("Análisis de países"),
  sidebarLayout(
    sidebarPanel(
      mod_select_location_ui("loc")
    ),
    mainPanel(
      mod_outputs_ui("grafs")
    )
  )
)

server <- function(input, output, session) {
  selected_country <- mod_select_location_server("loc")
  mod_outputs_server("grafs", selected_country)
}

shinyApp(ui, server)
