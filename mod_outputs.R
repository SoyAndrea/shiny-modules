mod_outputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("graf_vida")),
    plotOutput(ns("graf_pib"))
  )
}

mod_outputs_server <- function(id, pais_input) {
  moduleServer(id, function(input, output, session) {
    
    datos_filtrados <- reactive({
      req(pais_input())
      paises %>% filter(pais == pais_input())
    })
    
    output$graf_vida <- renderPlot({
      df <- datos_filtrados()
      ggplot(df, aes(x = anio, y = esperanza_de_vida)) +
        geom_line() +
        geom_point() +
        labs(title = paste("Esperanza de vida en", pais_input()),
             x = "Año", y = "Esperanza de vida")
    })
    
    output$graf_pib <- renderPlot({
      df <- datos_filtrados()
      ggplot(df, aes(x = anio, y = pib_per_capita)) +
        geom_col(fill = "#3182bd") +
        labs(title = paste("PIB per cápita en", pais_input()),
             x = "Año", y = "PIB per cápita")
    })
  })
}
