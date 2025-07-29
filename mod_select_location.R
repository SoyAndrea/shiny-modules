mod_select_location_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("continente"), "Seleccioná un continente:",
                choices = unique(paises$continente)),
    uiOutput(ns("pais_ui"))
  )
}

mod_select_location_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    paises_filtrados <- reactive({
      req(input$continente)
      paises %>%
        filter(continente == input$continente) %>%
        distinct(pais) %>%
        pull(pais)
    })
    
    output$pais_ui <- renderUI({
      selectInput(session$ns("pais"), "Seleccioná un país:", 
                  choices = paises_filtrados())
    })
    
    reactive({
      req(input$pais)
      input$pais
    })
  })
}
