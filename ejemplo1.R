### paquetes  --------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

### datos  --------------------------------------------------------

# Cargar el dataset paises de la biblioteca datos
# Si datos no está disponible, instálalo con: install.packages("datos")
library(datos)

### Interfaz de usuario  -------------------------------------------------------- 

ui <- fluidPage(
  titlePanel("Explorador de Esperanza de Vida"),
  
  fluidRow(
    column(8,
           p("Controles"),
           selectInput("continente", "Seleccionar Continente:",
                       choices = levels(paises$continente),
                       selected = levels(paises$continente)[1]),
           selectInput("pais", "Seleccionar País:",
                       choices = NULL),
           # Añadir botón de descarga del gráfico
           conditionalPanel(
             condition = "input.pais !== null && input.pais !== ''",
             hr(),
             downloadButton("descargarGrafico", "Descargar Gráfico (PNG)")
           )
    ),
    column(4,
           h4("Esperanza de Vida a lo Largo del Tiempo"),
           # Añadir un panel condicional para mostrar instrucciones cuando no se selecciona un país
           conditionalPanel(
             condition = "input.pais === null || input.pais === ''",
             div(
               style = "text-align: center; margin-top: 100px; color: #888;",
               h3("Por favor, seleccione un continente y un país para ver el gráfico de esperanza de vida")
             )
           ),
           # Mostrar el gráfico solo cuando se selecciona un país
           conditionalPanel(
             condition = "input.pais !== null && input.pais !== ''",
             plotOutput("graficoEsperanzaVida")
           )
    )
  ) #cierra fluid row
) # cierra fluid page

### SERVER  --------------------------------------------------------
server <- function(input, output, session) {
  
  # Filtrar países basados en la selección de continente
  datos_filtrados <- reactive({
    paises %>% filter(continente == input$continente)
  })
  
  # Actualizar el desplegable de países basado en la selección de continente
  observe({
    paises_lista <- unique(datos_filtrados()$pais)
    paises_ordenados <- c("", sort(as.character(paises_lista)))
    
    # Actualizar el desplegable de país
    updateSelectInput(session, "pais", 
                      choices = paises_ordenados,
                      selected = "")
  })
  
  # Crear la función para generar el gráfico (para reutilizar en la descarga)
  crear_grafico <- function() {
    # Obtener datos del país seleccionado
    datos_pais <- paises %>% 
      filter(pais == input$pais)
    
    # Crear gráfico con el nuevo estilo
    ggplot(datos_pais) + 
      geom_point(aes(y = esperanza_de_vida, x = anio, colour = pais), size = 2) +
      geom_line(aes(y = esperanza_de_vida, x = anio, group = pais, colour = pais), linewidth = 1) +
      scale_x_continuous(
        limits = c(1950, 2010),
        breaks = seq(1950, 2010, by = 10)  
      ) +
      scale_color_manual(values = c("#61007d")) +  
      labs(
        y = "Esperanza de vida",
        x = "Año",
        title = paste("Esperanza de vida en", input$pais),
        subtitle = "Evolución quinquenal, años 1952 a 2007",
        caption = "Fuente: paquete datos, dataset paises"
      ) +
      theme_bw() +
      theme(legend.position = "none")
  }
  
  # Renderizar el gráfico para la UI
  output$graficoEsperanzaVida <- renderPlot({
    req(input$pais)
    
    # Comprobación adicional para asegurar que tenemos una selección válida de país
    validate(
      need(input$pais != "", "Por favor, seleccione un país")
    )
    
    crear_grafico()
  })
  
  # Configurar la descarga del gráfico en PNG
  output$descargarGrafico <- downloadHandler(
    filename = function() {
      # Generar nombre de archivo con el país seleccionado
      paste("esperanza_vida_", gsub(" ", "_", input$pais), ".png", sep = "")
    },
    content = function(file) {
      # Guardar el gráfico en formato PNG
      ggsave(file, plot = crear_grafico(), device = "png", width = 10, height = 6, dpi = 300)
    }
  )
}

shinyApp(ui, server)
