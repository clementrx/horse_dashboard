# Installer les packages si ce n'est pas déjà fait
# install.packages(c("shiny", "dplyr", "highcharter"))

# Charger les packages
library(shiny)
library(dplyr)
library(ggplot2)
library(gt)
library(gtExtras)
library(formattable)
library(RColorBrewer)
library(shinythemes)
# webr::install('plotly')

# library(shinymanager)
# library(highcharter)

# Charger les données
data <- read.csv2('preds.csv', stringsAsFactors = FALSE)
data = data %>% 
  arrange(R_pmuNumber, C_number) %>% 
  distinct(horseName, saddle, C_uuid, .keep_all = TRUE)

data <- mutate(data, horse_label = paste0(saddle, '-', horseName))
data <- mutate(data, reunion_label = paste0(R_pmuNumber, ' - ', R_name))
data <- mutate(data, course_label = paste0(C_number, ' - ', C_name))

# data.frame with credentials info
# credentials <- data.frame(
#   user = c("test", "fanny", "victor", "benoit"),
#   password = c("1234", "azerty", "12345", "azerty"),
#   # comment = c("alsace", "auvergne", "bretagne"), %>% 
#   stringsAsFactors = FALSE
# )

ui <- fluidPage(
  
  navbarPage(
    theme = shinytheme('cerulean'),
    
    "PMU",
    tabPanel("Connexion",
             
             sidebarPanel(width = 3,
                          passwordInput("password", "Mot de passe svp"),
                          #       # Bouton pour soumettre le mot de passe
                          actionButton("submit", "Soumettre mot de passe"),
                          br(),
                          
                          # uiOutput('hipp_id'),
                          # uiOutput('course_filter_ui'),
                          
                          # dateRangeInput("daterange",
                          #                "Période : " ,
                          #                start = today()-90,
                          #                end   = today(),
                          #                # min = NULL,
                          #                # max = NULL,
                          #                format = "yyyy-mm-dd",
                          #                # startview = "month",
                          #                # weekstart = 0,
                          #                language = "fr",
                          #                separator = " à "),
                          br()
             ),
             
             
             mainPanel(width = 9
             )),
    
    tabPanel("Graphiques",
             
             sidebarPanel(width = 3,
                          
                          uiOutput('hipp_id_graph'),
                          uiOutput('course_filter_ui_graph'),
                          
                          # dateRangeInput("daterange",
                          #                "Période : " ,
                          #                start = today()-90,
                          #                end   = today(),
                          #                # min = NULL,
                          #                # max = NULL,
                          #                format = "yyyy-mm-dd",
                          #                # startview = "month",
                          #                # weekstart = 0,
                          #                language = "fr",
                          #                separator = " à "),
                          br()
             ),
             
             
             mainPanel(width = '100%',
                       
                       fluidRow(plotOutput("mychart"))
             )),
    tabPanel("Tableau",
             
             sidebarPanel(width = 3,
                          
                          uiOutput('hipp_id_tab'),
                          uiOutput('course_filter_tab'),
                          
                          # dateRangeInput("daterange",
                          #                "Période : " ,
                          #                start = today()-90,
                          #                end   = today(),
                          #                # min = NULL,
                          #                # max = NULL,
                          #                format = "yyyy-mm-dd",
                          #                # startview = "month",
                          #                # weekstart = 0,
                          #                language = "fr",
                          #                separator = " à "),
                          br()
             ),
             
             
             mainPanel(width = '100%',
                       
                       fluidRow(gt_output("mytable"))
             ))
    
    
  )
  
)


# Définir l'interface utilisateur Shiny
# ui <- fluidPage(
#   titlePanel("Analyse des probabilités de gagner"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       # Filtres
#       selectInput("hippodrome", "Choisissez une Réunion", choices = unique(data$reunion_label)),
#       uiOutput("course_filter"),
#       
#       passwordInput("password", "Mot de passe svp"),
#       # Bouton pour soumettre le mot de passe
#       actionButton("submit", "Soumettre mot de passe")
#       
#       
#     ),
#     
#     mainPanel(
#       # Graphique Highcharter
#       # highchartOutput("mychart")
#       
#       plotOutput("mychart"),
#       
#       # Tableau
#       gt_output("mytable")
#     )
#   )
# )

# ui = secure_app(ui)



# Définir le serveur Shiny
server <- function(input, output, session) {
  
  # result_auth <- secure_server(check_credentials = check_credentials(credentials))
  # 
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(result_auth)
  # })
  
  password_correct <- reactiveVal(FALSE)
  observeEvent(input$submit, {
    # Vérifier le mot de passe
    password_correct(input$password == "1234")
  })
  
  output$hipp_id <- renderUI({
    hipp = unique(data$reunion_label)
    selectInput('hipp_filter_id', 'Réunion', hipp)
  })
  
  output$hipp_id_graph <- renderUI({
    hipp = unique(data$reunion_label)
    selectInput('hipp_filter_id_graph', 'Réunion', hipp)
  })
  
  output$hipp_id_tab <- renderUI({
    hipp = unique(data$reunion_label)
    selectInput('hipp_filter_id_tab', 'Réunion', hipp)
  })
  
  
  # Mettre à jour les options du filtre de course en fonction de l'hippodrome sélectionné
  output$course_filter_ui  <- renderUI({
    selected_hippodrome <- input$hipp_filter_id
    courses <- unique(filter(data, reunion_label == selected_hippodrome)$course_label)
    selectInput("course_filter", "Choisissez une course", choices = courses)
  })
  
  output$course_filter_ui_graph <- renderUI({
    selected_hippodrome <- input$hipp_filter_id
    courses <- unique(filter(data, reunion_label == selected_hippodrome)$course_label)
    selectInput("course_filter_graph", "Choisissez une course", choices = courses)
  })
  
  output$course_filter_tab  <- renderUI({
    selected_hippodrome <- input$hipp_filter_id
    courses <- unique(filter(data, reunion_label == selected_hippodrome)$course_label)
    selectInput("course_filter_tab", "Choisissez une course", choices = courses)
  })
  
  observe({
    updateSelectInput(session, "hipp_filter_id_tab", selected = input$hipp_filter_id)
    
  })
  observe({
    updateSelectInput(session, "hipp_filter_id_tab", selected = input$hipp_filter_id_graph)
  })
  observe({
    updateSelectInput(session, "hipp_filter_id", selected = input$hipp_filter_id_graph)
  })
  observe({
    updateSelectInput(session, "hipp_filter_id", selected = input$hipp_filter_id_tab)
  })
  observe({
    updateSelectInput(session, "hipp_filter_id_graph", selected = input$hipp_filter_id)
  })
  
  
  observe({
    updateSelectInput(session, "course_filter_graph", selected = input$course_filter)
  })
  observe({
    updateSelectInput(session, "course_filter", selected = input$course_filter_graph)
  })
  observe({
    updateSelectInput(session, "course_filter", selected = input$course_filter_tab)
  })
  observe({
    updateSelectInput(session, "course_filter_tab", selected = input$course_filter_graph)
  })
  observe({
    updateSelectInput(session, "course_filter_tab", selected = input$course_filter)
  })
  
  # observe({
  #   cur_val <- output$hipp_id
  #   # This observer depends on text2 and updates text1 with any changes
  #   if (cur_val != input$text2){
  #     # Then we assume text2 hasn't yet been updated
  #     updateTextInput(session, "text1", NULL, input$text2)
  #     cur_val <<- input$text2
  #   }
  # })
  
  
  # Fonction de filtrage des données
  filtered_data <- reactive({
    filter(data, reunion_label == input$hipp_filter_id, course_label == input$course_filter)
  })
  
  # Réaction pour mettre à jour les données filtrées
  observeEvent(input$update, {
    filtered_data()
  })
  
  # Créer le graphique Highcharter
  # output$mychart <- renderHighchart({
  #   filtered <- filtered_data()
  
  output$mychart <- renderPlot({
    filtered <- filtered_data()
    
    if (nrow(filtered) == 0 | !password_correct()) {
      return(NULL)
    }
    
    # hc = highchart() %>%
    #   hc_chart(type = "bar") %>%
    #   hc_yAxis(title = list(text = "Probabilité de gagner")) %>%
    #   hc_xAxis(categories = filtered$horse_label, title = list(text = "Cheval")) %>%
    #   hc_add_series(
    #     name = "Probabilité de gagner",
    #     data = filtered$.pred_win,
    #     dataLabels = list(
    #       enabled = TRUE,
    #       formatter = JS("function() { return Highcharts.numberFormat(this.y * 100, 2) + '%'; }")
    #     )
    #   )
    # 
    # hc
    
    nb.cols <- nrow(filtered)
    mycolors <- colorRampPalette(c("#2CA25F", "red"))(nb.cols)
    
    filtered = cbind(filtered %>% 
                   arrange(desc(.pred_win)),
                 mycolors)
    
    ggplot(filtered, aes(x = .pred_win, y = reorder(horse_label,  .pred_win), 
                     label = paste0(round(.pred_win * 100, 2), "%"))) +
      geom_bar(stat = "identity", fill = mycolors) +
      labs(x = "Cheval", y = "Probabilité de gagner") +
      geom_text(position = position_dodge(width = .9),
                hjust = -0.5,
                size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_vline(xintercept = 0.9,
                 color = "blue") +
      scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1),
                         breaks = seq(0, 1, 0.1))+
      scale_fill_gradientn(colors = mycolors, limits = c(0, 1)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=14,face="bold"))
    
  })
  
  # Fonction de rendu pour le tableau
  output$mytable <- render_gt({
    
    filtered <- filtered_data()
    if (nrow(filtered) == 0 | !password_correct()) {
      return(NULL)
    }
    
    filtered %>% 
      select(saddle, horseName, trainerName, jockeyName, 
             #totalPrize,
             driver_ratio_topp, trainer_ratio_topp, horse_ratio_topp,
             mean_ratio_temps_last24_month_hipp, mean_ratio_temps_last12_month,
             # fav_ko_last, outsider_last,
             .pred_win) %>%
      arrange(desc(.pred_win)) %>%  
      mutate(#.pred_win = formattable::percent(.pred_win),
        .pred_win = .pred_win*100,
        mean_ratio_temps_last24_month_hipp = digits(mean_ratio_temps_last24_month_hipp, 2),
        mean_ratio_temps_last12_month = digits(mean_ratio_temps_last12_month, 2),
        driver_ratio_topp = driver_ratio_topp*100,
        trainer_ratio_topp = trainer_ratio_topp*100,
        horse_ratio_topp = horse_ratio_topp*100) %>% 
      gt() %>%
      gt_theme_espn() %>% 
      cols_label(
        saddle = "Numéros",
        .pred_win = 'Proba',
        horseName = 'Cheval',
        trainerName = 'Entr.',
        jockeyName = 'Jockey', 
        driver_ratio_topp = "Ratio<br>Jockey",
        trainer_ratio_topp = "Ratio<br>Entr.",
        horse_ratio_topp = "Ratio<br>Cheval",
        mean_ratio_temps_last24_month_hipp = 'Temps<br>Piste',
        mean_ratio_temps_last12_month = 'Temps<br>1 an',
        # fav_ko_last = 'Fav<br>Dernière course',
        # outsider_last = 'Outsider<br>Dernière course',
        .fn = md) %>% 
      # gt_color_rows(.pred_win, palette = "ggsci::blue_material", domain = c(0,1)) %>% 
      gt_color_rows(mean_ratio_temps_last24_month_hipp, palette = "ggsci::green_material", direction = -1) %>% 
      gt_color_rows(mean_ratio_temps_last12_month, palette = "ggsci::teal_material", direction = -1) %>% 
      gt_plt_bar_pct(
        column = driver_ratio_topp,
        scaled = TRUE,
        labels = TRUE,
        decimals = 2,
        label_cutoff = 0.1,
        fill = "#FFD700", background = "lightblue"
      ) %>% 
      gt_plt_bar_pct(
        column = trainer_ratio_topp,
        scaled = TRUE,
        labels = TRUE,
        label_cutoff = 0.1,
        fill = "#4682B4", background = "lightblue"
      ) %>% 
      gt_plt_bar_pct(
        column = horse_ratio_topp,
        scaled = TRUE,
        labels = TRUE,
        label_cutoff = 0.1,
        fill = "#8B4513", background = "lightblue"
      ) %>% 
      gt_plt_bar_pct(
        column = .pred_win,
        scaled = TRUE,
        labels = TRUE,
        decimals = 3,
        label_cutoff = 0.1,
        fill = "#2CA25F", background = "lightblue",
        font_size = '13px'
        # height = '17px'
      ) %>% 
      tab_footnote(
        footnote = "% d'arrivées placées lors des 12 derniers mois",
        locations = cells_column_labels(
          columns = c(driver_ratio_topp, trainer_ratio_topp, horse_ratio_topp))
      ) %>% 
      tab_style(
        style = list(
          # cell_fill(color = "#F9E3D6"),
          cell_text(style = "oblique")
        ),
        locations = cells_body(
          columns = horseName,
        )
      ) %>% 
      tab_style(
        style = list(
          # cell_fill(color = "#F9E3D6"),
          cell_text(style = "oblique", size = px(12))
        ),
        locations = cells_body(
          columns = c(trainerName, jockeyName)
        )
      ) %>% 
      cols_width(
        saddle ~ px(60),
        .pred_win ~ px(120),
        trainerName ~ px(80),
        jockeyName ~ px(80),
        everything() ~ px(100))
    
    
    #   datatable(
    #   filtered %>% 
    #     select(saddle, horseName, trainerName, jockeyName, 
    #            #totalPrize,
    #            driver_ratio_topp, trainer_ratio_topp, horse_ratio_topp,
    #            mean_ratio_temps_last24_month_hipp, mean_ratio_temps_last12_month,
    #            fav_ko_last, outsider_last, .pred_win) %>% 
    #     arrange(desc(.pred_win)),
    #   colnames = c('Temps hipp' = 'mean_ratio_temps_last24_month_hipp',
    #                'Temps 365' = 'mean_ratio_temps_last12_month',
    #                'Numéros' = 'saddle',
    #                'Cheval' = 'horseName',
    #                'Entraineur' = 'trainerName',
    #                'Jockey' = 'jockeyName',
    #                'Ratio Driver' = 'driver_ratio_topp',
    #                'Ratio Entr.' = 'trainer_ratio_topp',
    #                'Ratio Cheval' = 'horse_ratio_topp'),
    #   options = list(dom = 't'),
    #   rownames = FALSE # Supprimer la colonne de numérotation des lignes
    # ) %>% 
    #   formatPercentage(columns = c("Ratio Driver", "Ratio Entr.", 'Ratio Cheval', '.pred_win'), digits = 2) %>% 
    #     # Remplacez "col1", "col2", ... par les noms des colonnes à formater en pourcentage
    #   formatRound(columns = c("Temps hipp", "Temps 365"), digits = 2) 
    
  })
  
}

# Lancer l'application Shiny
shinyApp(ui, server)