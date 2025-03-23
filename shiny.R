library(shiny)
library(bslib)
library(ggplot2)
source("~/Documents/euria/informatique/projet_info/algorithme.R")



ui <- page_sidebar(
  title = "Carte auto-organisatrice",
  sidebar = sidebar(
                    position = "right",
                    radioButtons(
                      "d_input",
                      "Dimension du réseau",
                      choices = list("1" = 1, "2" = 2),
                      selected = 1
                    ),
                    numericInput("l_input", "Longueur du reseau", value = 10),
                    # sliderInput(
                    #   "l_input",
                    #   "Longueur du réseau",
                    #   min = 1,
                    #   max = 1000,
                    #   value = 5
                    # ),
                    radioButtons(
                      "X_input",
                      "Données",
                      choices = list("Loi uniforme" = 1, "Vecteurs gaussiens" = 2),
                      selected = 1
                    ),
                    radioButtons(
                      "affichage_input",
                      "Type d'affichage",
                      choices = list("1" = 1, "2" = 2),
                      selected = 1
                    ),
                    sliderInput(
                      "M_input",
                      "Nombre d'itération",
                      min = 1,
                      max = 10000,
                      value = 1000
                    ),
                    # numericInput("M_input", "Nombre d'iterations", value = 1000),
                    actionButton("action", "Lancer l'algorithme"),
            ),
  plotOutput("plot")
)




server <- function(input, output) {
  
  observeEvent(input$action, {
    output$plot <- renderPlot({
      X <- if (input$X_input == 1) Z else Y
      
      l <- input$l_input
      d <- as.integer(input$d_input)
      affichage <- as.integer(input$affichage_input)
      M <- as.integer(input$M_input)
      
      algo_shiny(X, d, l, M, affichage)
      
    })
  })
}

shinyApp(ui = ui, server = server)



