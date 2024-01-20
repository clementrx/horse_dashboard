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
library(shinyjs)
library(scales)
library(shinydashboard)
library(data.table)
library(stringr)
# webr::install('plotly')

# library(shinymanager)
# library(highcharter)

# Charger les données
data <- read.csv2('pred.csv', stringsAsFactors = FALSE)
result <- read.csv2('result.csv', stringsAsFactors = FALSE)

setDT(result)
result[,PMU_fr_liveOdd:= as.numeric(PMU_fr_liveOdd)]
result[,PMU_liveOdd:= as.numeric(PMU_liveOdd)]
result[,GENYBET_liveOdd:= as.numeric(GENYBET_liveOdd)]
result[,SOREC_liveOdd:= as.numeric(SOREC_liveOdd)]
result[,BETCLIC_liveOdd:= as.numeric(BETCLIC_liveOdd)]
result[,ZETURF_liveOdd:= as.numeric(ZETURF_liveOdd)]


result[, cote := ifelse(!is.na(PMU_fr_liveOdd), PMU_fr_liveOdd,
                        ifelse(!is.na(PMU_liveOdd), PMU_liveOdd,
                               ifelse(!is.na(ZETURF_liveOdd), ZETURF_liveOdd,
                                      ifelse(!is.na(GENYBET_liveOdd), GENYBET_liveOdd,
                                             ifelse(!is.na(SOREC_liveOdd), SOREC_liveOdd, BETCLIC_liveOdd)))))]

result = result %>%
  group_by(raceDate, C_uuid) %>%
  arrange(-PP, .by_group = T) %>%
  mutate(rank_PP = row_number()) %>%
  arrange(-P1, .by_group = T) %>%
  mutate(rank_P1 = row_number()) %>%
  ungroup()


data = data %>% 
  arrange(R_pmuNumber, C_number) %>% 
  distinct(horseName, saddle, C_uuid, .keep_all = TRUE)

data <- mutate(data, horse_label = paste0(saddle, '-', horseName))
data <- mutate(data, reunion_label = paste0(R_pmuNumber, ' - ', R_name))
data <- mutate(data, course_label = paste0(C_number, ' - ', C_name))
data$C_time <- sub("^(\\d{2}:\\d{2}).*$", "\\1", data$C_time )

up_arrow <- "<span style=\"color:green\">&#9650;</span>"
down_arrow <- "<span style=\"color:red\">&#9660;</span>"
Logged = FALSE
my_password <- "quinte"

rating_stars <- function(i) {
  
  stars <-  ifelse(i  == 1,
                   fontawesome::fa("star", fill= "orange"),
                   fontawesome::fa("star", fill= "grey"))
  
  label <- sprintf("Préparé pour cette course")
  div_out <- div(title = label, "aria-label" = label, role = "img", stars)
  
  glue::glue(as.character(div_out) %>% 
               gt::html())
}

ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    theme = shinytheme('cerulean'),
    
    "PMU",
    
    tabPanel("Graphiques",
             fluidRow(
               div(id = "Sidebar", sidebarPanel(width = 12,
                                                fluidRow(
                                                  column(4, uiOutput('hipp_id_graph')),
                                                  column(6, uiOutput('course_filter_ui_graph'))),
                                                fluidRow(textOutput("heure")))
               ),
               mainPanel(width = "100%",
                         fluidRow(
                           column(4, actionButton("hideSidebar", "Cacher/afficher les filtres", class = "btn btn-sm")),
                           # column(3, actionButton("showSidebar", "Afficher les filtres", class = "btn btn-sm"))
                         ),
                         fluidRow(plotOutput("mychart")),
                         hr(),
                         fluidRow(gt_output("mytable"))))),
    # tabPanel("Chevaux à jouer",
    #          
    #          fluidRow(
    #            div(id = "Sidebar", sidebarPanel(width = 12,
    #                                             fluidRow( sliderInput("tresh",
    #                                                                   label="Proba mini.",
    #                                                                   min = 0, max = 100, post  = " %",
    #                                                                   value = 80))))),
    #          
    #          mainPanel(width = '100%',
    #                    fluidRow(gt_output("mytable_today"))
    #                    
    #          )),
    tabPanel("Backtest",
             
             fluidRow(
               div(id = "Sidebar_back", sidebarPanel(width = 12,
                                                     fluidRow( column(2,sliderInput("tresh2",
                                                                                    label="Proba mini. P1",
                                                                                    min = 0, max = 100, post  = " %",
                                                                                    value = 10)),
                                                               column(2,sliderInput("treshPP",
                                                                                    label="Proba mini. PP",
                                                                                    min = 0, max = 100, post  = " %",
                                                                                    value = 50)),
                                                               column(2,sliderInput("cote_max",
                                                                                    label="Cote max",
                                                                                    min = 1.1, max = 100, post  = " Є",
                                                                                    value = 20))
                                                     ),
                                                     fluidRow(
                                                       column(2, numericInput("SG", "Mise G:", 2, min = 0, max = 1000)),
                                                       column(2, numericInput("SP", "Mise P:", 8, min = 0, max = 1000)),
                                                       column(2, numericInput("rank_tresh", "Rank de proba mini P1", 20, min = 1, max = 20)),
                                                       column(2, numericInput("rank_treshPP", "Rank de proba mini PP", 1, min = 1, max = 20)))))),
             
             mainPanel(width = '100%',
                       dashboardBody(
                         fluidRow(
                           infoBoxOutput('sg_stat'),
                           infoBoxOutput('total_paris'),
                           infoBoxOutput('roi_G')
                         ),
                         fluidRow(
                           infoBoxOutput('sp_stat'),
                           infoBoxOutput('total_dep'),
                           infoBoxOutput('roi_P')
                         ),
                         fluidRow(
                           infoBoxOutput('roi'),
                           infoBoxOutput('gain_tot'),
                           infoBoxOutput('roi_moyen'),
                         )),
                       # summaryBox2("% Pred. SP",
                       #             textOutput('sp_stat'),
                       #             width = 3,
                       #             icon = "fas fa-clipboard-list",
                       #             style = "info"),
                       # summaryBox2("% Pred. SG",
                       #             textOutput('sg_stat'),
                       #             width = 3,
                       #             icon = "fas fa-clipboard-list",
                       #             style = "info"),)
                       fluidRow(plotOutput("plot_backtest"))
                       
             )),
    
    
    
    
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
  
  observeEvent(input$hideSidebar, {
    if (input$hideSidebar %% 2 == 1) {
      shinyjs::hide(id = "Sidebar")
    } else {
      shinyjs::show(id = "Sidebar")
    }
  })
  
  values <- reactiveValues(authenticated = FALSE)
  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      title = "Bienvenue sur l'algo PMU",
      easyClose = F,
      passwordInput("password", "Mot de passe :"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Password <- input$password
    })
    Id.password <- which(my_password == Password)
    if (length(Id.password) > 0) {
      Logged <<- TRUE
      values$authenticated <- TRUE
      obs1$suspend()
      removeModal()
    }else {
      values$authenticated <- FALSE
    }
  })
  
  output$hipp_id_graph <- renderUI({
    hipp = unique(data$reunion_label)
    selectInput('hipp_filter_id_graph', 'Réunion', hipp)
  })
  
  output$course_filter_ui_graph <- renderUI({
    selected_hippodrome <- input$hipp_filter_id_graph
    courses <- unique(filter(data, reunion_label == selected_hippodrome)$course_label)
    selectInput("course_filter_graph", "Choisissez une course", choices = courses)
  })
  
  # 
  # observe({
  #   y <- input$course_filter_tab
  #   output$course_filter_graph <- y
  # })
  
  # Fonction de filtrage des données
  filtered_data <- reactive({
    filter(data, reunion_label == input$hipp_filter_id_graph, course_label == input$course_filter_graph)
  })
  
  # Réaction pour mettre à jour les données filtrées
  observeEvent(input$update, {
    filtered_data()
  })
  
  output$heure <- renderText({
    
    filtered <- filtered_data()
    paste0('Heure de la course : ', unique(filtered$C_time))
  })
  
  # Créer le graphique Highcharter
  # output$mychart <- renderHighchart({
  #   filtered <- filtered_data()
  
  output$mychart <- renderPlot({
    filtered <- filtered_data()
    
    if (nrow(filtered) == 0 | !values$authenticated) {
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
                       arrange(desc(PP)),
                     mycolors)
    
    ggplot(filtered, aes(x = PP, y = reorder(horse_label,  PP), 
                         label = paste0(round(PP * 100, 2), "%"))) +
      geom_bar(stat = "identity", fill = mycolors) +
      labs(x = "Probabilité de finir SP", y = "Cheval") +
      geom_text(position = position_dodge(width = .9),
                hjust = -0.5,
                size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      # geom_vline(xintercept = 0.8,
      #            color = "blue") +
      scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1),
                         breaks = seq(0, 1, 0.1))+
      scale_fill_gradientn(colors = mycolors, limits = c(0, 1)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=14,face="bold"))
    
  })
  
  # Fonction de rendu pour le tableau
  output$mytable <- render_gt({
    
    filtered <- filtered_data()
    if (nrow(filtered) == 0 | !values$authenticated) {
      return(NULL)
    }
    
    filtered %>% 
      select(saddle, horseName, trainerName, jockeyName, 
             #totalPrize,
             driver_ratio_topp, trainer_ratio_topp, horse_ratio_topp,
             cote, jour_last_course, mean_ratio_temps_last12_month,
             mean_temps_last12_month,
             CLASS_INF, PREP_D4, 
             driver_ratio_topp_evol, trainer_ratio_topp_evol,
             # fav_ko_last, outsider_last,
             P1, PP) %>%
      arrange(desc(PP)) %>%  
      mutate(#.pred_win = formattable::percent(.pred_win),
        P1 = P1*100,
        PP = PP*100,
        mean_ratio_temps_last12_month = digits(mean_ratio_temps_last12_month*100, 2),
        # label = paste0(horseName, ";", jockeyName, ";", trainerName),
        mean_temps_last12_month = digits(mean_temps_last12_month, 2),
        driver_ratio_topp = driver_ratio_topp*100,
        trainer_ratio_topp = trainer_ratio_topp*100,
        horse_ratio_topp = horse_ratio_topp*100,
        D4 = ifelse(PREP_D4 == 1, 'star', ''),
        INF = ifelse(CLASS_INF == 1, 'circle-down', '')) %>% 
      # select(-c(horseName, jockeyName, trainerName)) %>% 
      gt() %>%
      gt_fa_column(D4) %>% 
      gt_fa_column(INF) %>% 
      cols_merge(
        columns = c(horseName, jockeyName, trainerName),
        pattern = "{1};{2};{3}"
      ) %>% 
      text_transform(
        locations = cells_body(
          columns = c(horseName)
        ),
        fn = function(x){
          
          horseName <- word(x, 1, sep = ";")
          jockeyName <- word(x, 2, sep = ";")
          trainerName <- word(x, 3, sep = ";")
          glue::glue(
            "<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{horseName}</div>
        <div><span style ='font-weight:bold;color:grey;font-size:12px'>{jockeyName}</span></div>
             <div><span style ='font-weight:bold;color:grey;font-size:10px'>{trainerName}</span></div>"
          )
        }
      ) %>% 
      # gt_theme_espn() %>% 
      cols_label(
        saddle = "Numéros",
        P1 = 'Proba<br>Gagnant',
        PP = 'Proba<br>Placé',
        horseName = 'Cheval',
        driver_ratio_topp = "Ratio<br>Jockey",
        trainer_ratio_topp = "Ratio<br>Entr.",
        horse_ratio_topp = "Ratio<br>Cheval",
        mean_ratio_temps_last12_month = 'Score (100)<br>1 an',
        mean_temps_last12_month = 'Red.kil<br>1 an',
        jour_last_course = 'Repos',
        # fav_ko_last = 'Fav<br>Dernière course',
        # outsider_last = 'Outsider<br>Dernière course',
        .fn = md) %>% 
      fmt_currency(columns = cote, decimals = 1, currency = 'EUR', placement = 'right') %>% 
      # gt_color_rows(.pred_win, palette = "ggsci::blue_material", domain = c(0,1)) %>% 
      gt_color_rows(mean_ratio_temps_last12_month, palette = "ggsci::green_material", direction = 1) %>% 
      gt_color_rows(mean_temps_last12_month, palette = "ggsci::teal_material", direction = -1) %>% 
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
        column = PP,
        scaled = TRUE,
        labels = TRUE,
        # decimals = 3,
        label_cutoff = 0.1,
        fill = "#2CA25F", background = "lightblue",
        font_size = '13px'
        # height = '17px'
      ) %>% 
      gt_plt_bar_pct(
        column = P1,
        scaled = TRUE,
        labels = TRUE,
        # decimals = 3,
        label_cutoff = 0.1,
        fill = "#b8711a", background = "lightblue",
        font_size = '13px'
        # height = '17px'
      ) %>% 
      tab_footnote(
        footnote = "% d'arrivées placées lors des 12 derniers mois, et indicateur de ratio des 2 derniers mois",
        locations = cells_column_labels(
          columns = c(driver_ratio_topp, trainer_ratio_topp, horse_ratio_topp))
      ) %>% 
      tab_footnote(
        footnote = "Nombre de jours depuis la dernière course",
        locations = cells_column_labels(
          columns = c(jour_last_course))
      ) %>% 
      tab_footnote(
        footnote = "Ferrure les 3 dernières courses, et D4 aujourd'hui",
        locations = cells_column_labels(
          columns = c(D4))
      ) %>% 
      tab_footnote(
        footnote = "Course de catégorie inférieure à la précédente",
        locations = cells_column_labels(
          columns = c(INF))
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
          cell_text(size = px(12))
        ),
        locations = cells_body(
          columns = c(mean_ratio_temps_last12_month, mean_ratio_temps_last12_month)
        )
      ) %>% 
      text_transform(
        locations = cells_body(
          columns = driver_ratio_topp,
          rows = driver_ratio_topp_evol >= 0
        ),
        fn = function(x) paste(x, up_arrow)
      ) %>%
      text_transform(
        locations = cells_body(
          columns = driver_ratio_topp,
          rows = driver_ratio_topp_evol < 0
        ),
        fn = function(x) paste(x, down_arrow)
      ) %>% 
      text_transform(
        locations = cells_body(
          columns = trainer_ratio_topp,
          rows = trainer_ratio_topp_evol >= 0
        ),
        fn = function(x) paste(x, up_arrow)
      ) %>%
      text_transform(
        locations = cells_body(
          columns = trainer_ratio_topp,
          rows = trainer_ratio_topp_evol < 0
        ),
        fn = function(x) paste(x, down_arrow)
      ) %>% 
      cols_width(
        saddle ~ px(80),
        PP ~ px(80),
        horseName ~ px(100),
        driver_ratio_topp ~ px(80),
        trainer_ratio_topp ~ px(80),
        horse_ratio_topp ~ px(80),
        # mean_ratio_temps_last12_month, mean_ratio_temps_last12_month ~ px(60),
        # mean_ratio_temps_last12_month, mean_ratio_temps_last12_month~ px(60),
        jour_last_course~ px(90),
        everything() ~ px(90)) %>% 
      cols_hide(c(driver_ratio_topp_evol,
                  trainer_ratio_topp_evol,
                  PREP_D4,
                  CLASS_INF))
    
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
  
  
  output$mytable_today <- render_gt({
    
    filtered <- filter(data, .pred_win >= input$tresh/100)
    
    if (nrow(filtered) == 0 | !values$authenticated) {
      return(NULL)
    }
    
    filtered %>% 
      mutate(id = paste0('R',R_pmuNumber, 'C', C_number)) %>% 
      select(id, C_time, R_name, C_name,
             saddle, horseName, trainerName, jockeyName, 
             jour_last_course,
             # fav_ko_last, outsider_last,
             .pred_win) %>%
      arrange(desc(.pred_win)) %>%  
      mutate(#.pred_win = formattable::percent(.pred_win),
        .pred_win = .pred_win*100) %>% 
      gt() %>%
      gt_theme_espn() %>% 
      cols_label(
        saddle = "Numéros",
        C_time = 'Heure',
        .pred_win = 'Proba',
        horseName = 'Cheval',
        trainerName = 'Entr.',
        jockeyName = 'Jockey', 
        jour_last_course = 'Repos',
        R_name = 'Réunion',
        C_name = 'Course',
        id = 'R-C',
        # fav_ko_last = 'Fav<br>Dernière course',
        # outsider_last = 'Outsider<br>Dernière course',
        .fn = md) %>% 
      # gt_color_rows(.pred_win, palette = "ggsci::blue_material", domain = c(0,1)) %>% 
      gt_plt_bar_pct(
        column = .pred_win,
        scaled = TRUE,
        labels = TRUE,
        # decimals = 3,
        label_cutoff = 0.1,
        fill = "#2CA25F", background = "lightblue",
        font_size = '13px'
        # height = '17px'
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
          columns = c(trainerName, jockeyName, jour_last_course)
        )
      ) %>% 
      cols_width(
        saddle ~ px(60),
        id ~ px(50),
        C_time~ px(50),
        .pred_win ~ px(120),
        trainerName ~ px(60),
        jockeyName ~ px(60),
        jour_last_course~ px(50),
        everything() ~ px(90)) 
    
    
    
    
  })
  
  output$sp_stat <- renderInfoBox({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP)
    
    text = filtered %>%
      group_by(TOPP) %>%
      tally() %>%
      mutate(perc = n/sum(n)) %>% 
      filter(TOPP == 1)
    
    infoBox(
      "% Pred. SP", paste0(round(text$perc, 2)*100,"%"), icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$sg_stat <- renderInfoBox({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP,
             cote <= input$cote_max)
    
    text = filtered %>%
      group_by(TOP1) %>%
      tally() %>%
      mutate(perc = n/sum(n)) %>% 
      filter(TOP1 == 1)
    
    infoBox(
      "% Pred. SG", paste0(round(text$perc, 2)*100,"%"), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
    
    
  })
  
  output$total_paris <- renderInfoBox({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP,
             cote <= input$cote_max)
    
    text = nrow(filtered)
    
    infoBox(
      "Nombre total de paris : ", paste0(text), icon = icon("book", lib = "glyphicon"),
      color = "yellow"
    )
    
  })
  
  output$total_dep <- renderInfoBox({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP,
             cote <= input$cote_max)
    
    text = (nrow(filtered)*input$SP) + (nrow(filtered)*input$SG)
    
    infoBox(
      "Dépenses total : ", paste0(text, "€"), icon = icon("credit-card", lib = "glyphicon"),
      color = "yellow"
    )
    
  })
  
  output$gain_tot <- renderInfoBox({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP,
             cote <= input$cote_max)
    
    mise_p = input$SP
    mise_g = input$SG
    
    stat = filtered %>%
      mutate(
        gain_place = ifelse(P > 0,
                            (P*mise_p)-mise_p,
                            -mise_p),
        gain_gagnant = ifelse(G > 0,
                              (G*mise_g)-mise_g,
                              -mise_g),
      )
    
    text = sum(stat$gain_place) + sum(stat$gain_gagnant)
    
    infoBox(
      "Gains totaux : ", paste0(round(text, 2), "€"), icon = icon("credit-card", lib = "glyphicon"),
      color = "yellow"
    )
    
  })
  
  
  
  
  filtered_backtest <- reactive({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP,
             cote <= input$cote_max)
    
    mise_p = input$SP
    mise_g = input$SG
    
    stat = filtered %>%
      mutate(
        gain_place = ifelse(P > 0,
                            (P*mise_p)-mise_p,
                            -mise_p),
        gain_gagnant = ifelse(G > 0,
                              (G*mise_g)-mise_g,
                              -mise_g),
      )
    
    stat %>% 
      group_by(raceDate) %>%
      summarise(gain_place = sum(gain_place),
                gain_gagnant = sum(gain_gagnant),
                dep = n()*(mise_p+mise_g)) %>%
      ungroup() %>%
      mutate(cum_place = cumsum(gain_place),
             cum_gagnant = cumsum(gain_gagnant),
             total = cumsum(gain_place + gain_gagnant))
    
  })
  
  output$roi <- renderInfoBox({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP,
             cote <= input$cote_max)
    
    mise_p = input$SP
    mise_g = input$SG
    
    stat = filtered %>%
      mutate(
        gain_place = ifelse(P > 0,
                            (P*mise_p)-mise_p,
                            -mise_p),
        gain_gagnant = ifelse(G > 0,
                              (G*mise_g)-mise_g,
                              -mise_g),
      )
    
    text =  (sum(stat$gain_place) + sum(stat$gain_gagnant)) / ((nrow(filtered)*input$SP) + (nrow(filtered)*input$SG))
    
    
    infoBox(
      "ROI total: ", paste0(round(text, 2)*100,"%"), icon = icon("credit-card", lib = "glyphicon"),
      color = "yellow"
    )
    
  })
  
  output$roi_G <- renderInfoBox({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP,
             cote <= input$cote_max)
    
    mise_g = input$SG
    
    stat = filtered %>%
      mutate(
        gain_gagnant = ifelse(G > 0,
                              (G*mise_g)-mise_g,
                              -mise_g)
      )
    
    text =  sum(stat$gain_gagnant) / (nrow(filtered)*input$SG)
    
    
    infoBox(
      "ROI SG: ", paste0(round(text, 2)*100,"%"), icon = icon("credit-card", lib = "glyphicon"),
      color = "yellow"
    )
    
  })
  
  output$roi_P <- renderInfoBox({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP,
             cote <= input$cote_max)
    
    mise_p = input$SP
    
    stat = filtered %>%
      mutate(
        gain_place = ifelse(P > 0,
                            (P*mise_p)-mise_p,
                            -mise_p)
      )
    
    
    text =  sum(stat$gain_place) / (nrow(filtered)*input$SP)
    
    
    infoBox(
      "ROI SP: ", paste0(round(text, 2)*100,"%"), icon = icon("credit-card", lib = "glyphicon"),
      color = "yellow"
    )
    
  })
  
  output$roi_moyen <- renderInfoBox({
    
    filtered <- result %>% 
      filter(P1 >= input$tresh2/100,
             PP >= input$treshPP/100,
             rank_P1 <= input$rank_tresh,
             rank_PP <= input$rank_treshPP,
             cote <= input$cote_max)
    
    mise_p = input$SP
    mise_g = input$SG
    
    stat = filtered %>%
      mutate(
        gain_place = ifelse(P > 0,
                            (P*mise_p)-mise_p,
                            -mise_p),
        gain_gagnant = ifelse(G > 0,
                              (G*mise_g)-mise_g,
                              -mise_g),
      ) %>% 
      group_by(raceDate) %>% 
      summarise(paris = n(),
                benef = sum(gain_place, gain_gagnant)) %>% 
      ungroup() %>% 
      mutate(ROI = benef / (paris*mise_p + paris*mise_g))
    
    
    text =  mean(stat$ROI)
    
    
    infoBox(
      "ROI moyen par jour: ", paste0(round(text, 2)*100,"%"), icon = icon("credit-card", lib = "glyphicon"),
      color = "yellow"
    )
    
  })
  
  output$plot_backtest <- renderPlot({
    
    filtered <- filtered_backtest()
    
    if (nrow(filtered) == 0 | !values$authenticated) {
      return(NULL)
    }
    
    filtered$raceDate = as.Date(filtered$raceDate)
    
    ggplot(filtered, aes(x = raceDate, y = total))+ 
      geom_line(aes(color='Total')) +
      geom_point(size = 0.5)+
      geom_line( aes(x = raceDate, y = cum_gagnant, color='SG')) +
      geom_line( aes(x = raceDate, y = cum_place, color='SP')) +
      scale_x_date(date_labels = '%d-%m-%y', date_breaks = '1 day') +
      scale_y_continuous(labels = dollar_format(suffix = "€", prefix = "")) +
      geom_hline(yintercept=0, linetype="dashed", color = "red")+
      theme(
        axis.text.x = element_text(
          angle = 40,
          hjust = 1,
          size = 8)) +
      ylab("Portefeuille") + xlab("Date") +
      scale_color_manual(name='',
                         breaks=c('Total', 'SG', 'SP'),
                         values=c('Total'='black', 'SG'='blue', 'SP'='orange'))
    
    
  })
  
}

# Lancer l'application Shiny
shinyApp(ui, server)