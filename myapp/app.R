# Installer les packages si ce n'est pas déjà fait
# install.packages(c("shiny", "dplyr", "highcharter"))

# Charger les packages
library(shiny)
library(dplyr)
library(highcharter)
library(here)

# Charger les données
data <- read.csv2(here("data", "preds.csv"), stringsAsFactors = FALSE)
data <- mutate(data, horse_label = paste0(saddle, '-', horseName))
data <- mutate(data, course_label = paste0(C_number, ' - ', C_name))

# Définir l'interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Analyse des probabilités de gagner"),
  
  sidebarLayout(
    sidebarPanel(
      # Filtres
      selectInput("hippodrome", "Choisissez un hippodrome", choices = unique(data$R_name)),
      uiOutput("course_filter") 
      
      
    ),
    
    mainPanel(
      # Graphique Highcharter
      highchartOutput("mychart")
    )
  )
)

# Définir le serveur Shiny
server <- function(input, output) {
  
  # Mettre à jour les options du filtre de course en fonction de l'hippodrome sélectionné
  output$course_filter <- renderUI({
    selected_hippodrome <- input$hippodrome
    courses <- unique(filter(data, R_name == selected_hippodrome)$course_label)
    selectInput("course_filter", "Choisissez une course", choices = courses)
  })
  
  # Fonction de filtrage des données
  filtered_data <- reactive({
    filter(data, R_name == input$hippodrome, course_label == input$course_filter)
  })
  
  # Réaction pour mettre à jour les données filtrées
  observeEvent(input$update, {
    filtered_data()
  })
  
  # Créer le graphique Highcharter
  output$mychart <- renderHighchart({
    filtered <- filtered_data()
    
    if (nrow(filtered) == 0) {
      return(NULL)
    }
    
    hc = highchart() %>%
      hc_chart(type = "bar") %>%
      hc_yAxis(title = list(text = "Probabilité de gagner")) %>%
      hc_xAxis(categories = filtered$horse_label, title = list(text = "Cheval")) %>%
      hc_add_series(
        name = "Probabilité de gagner",
        data = filtered$.pred_win,
        dataLabels = list(
          enabled = TRUE,
          formatter = JS("function() { return Highcharts.numberFormat(this.y * 100, 2) + '%'; }")
        )
      )
    
    hc
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
