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
# webr::install('plotly')

# library(shinymanager)
# library(highcharter)

# Charger les données
data <- read.csv2('preds.csv', stringsAsFactors = FALSE)
result <- read.csv2('result.csv', stringsAsFactors = FALSE)
result <- result %>% 
  group_by(C_uuid) %>%
  arrange(-Proba, .by_group = T) %>%
  mutate(rank_P1 = row_number()) %>%
  ungroup() %>% 
  mutate(flag = ifelse(PP == 1 & P == 0,
                       1,
                       0)) %>%
  filter(flag == 0) 

result$raceDate <- as.Date(result$raceDate)

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
    tabPanel("Chevaux à jouer",
             
             fluidRow(
               div(id = "Sidebar", sidebarPanel(width = 12,
                                                fluidRow( sliderInput("tresh",
                                                                      label="Proba mini.",
                                                                      min = 0, max = 100, post  = " %",
                                                                      value = 80))))),

             mainPanel(width = '100%',
                       fluidRow(gt_output("mytable_today"))

)),
tabPanel("Backtest",
         
         fluidRow(
           div(id = "Sidebar_back", sidebarPanel(width = 12,
                                            fluidRow( sliderInput("tresh2",
                                                                  label="Proba mini.",
                                                                  min = 0, max = 100, post  = " %",
                                                                  value = 85)),
                                            fluidRow(
                                              column(2, numericInput("SG", "Mise G:", 2, min = 1, max = 1000)),
                                              column(2, numericInput("SP", "Mise P:", 8, min = 1, max = 1000)),
                                              column(2, numericInput("rank_tresh", "Rank de proba mini", 1, min = 1, max = 6)),)))),
         
         mainPanel(width = '100%',
                   dashboardBody(fluidRow(
                     infoBoxOutput('sp_stat'),
                     infoBoxOutput('sg_stat'))),
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
                   arrange(desc(.pred_win)),
                 mycolors)
    
    ggplot(filtered, aes(x = .pred_win, y = reorder(horse_label,  .pred_win), 
                     label = paste0(round(.pred_win * 100, 2), "%"))) +
      geom_bar(stat = "identity", fill = mycolors) +
      labs(x = "Probabilité de finir SP", y = "Cheval") +
      geom_text(position = position_dodge(width = .9),
                hjust = -0.5,
                size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_vline(xintercept = 0.8,
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
    if (nrow(filtered) == 0 | !values$authenticated) {
      return(NULL)
    }
    
    filtered %>% 
      select(saddle, horseName, trainerName, jockeyName, 
             #totalPrize,
             driver_ratio_topp, trainer_ratio_topp, horse_ratio_topp,
             mean_redkill_last12_month, mean_redkill_last24_month_hipp,
             driver_ratio_topp_evol, jour_last_course,
             # fav_ko_last, outsider_last,
             .pred_win) %>%
      arrange(desc(.pred_win)) %>%  
      mutate(#.pred_win = formattable::percent(.pred_win),
        .pred_win = .pred_win*100,
        mean_redkill_last12_month = digits(mean_redkill_last12_month, 2),
        mean_redkill_last24_month_hipp = digits(mean_redkill_last24_month_hipp, 2),
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
        mean_redkill_last12_month = 'Red.kill<br>1 an',
        mean_redkill_last24_month_hipp = 'Red.kil<br>piste',
        jour_last_course = 'Repos',
        # fav_ko_last = 'Fav<br>Dernière course',
        # outsider_last = 'Outsider<br>Dernière course',
        .fn = md) %>% 
      # gt_color_rows(.pred_win, palette = "ggsci::blue_material", domain = c(0,1)) %>% 
      gt_color_rows(mean_redkill_last12_month, palette = "ggsci::green_material", direction = -1) %>% 
      gt_color_rows(mean_redkill_last24_month_hipp, palette = "ggsci::teal_material", direction = -1) %>% 
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
        # decimals = 3,
        label_cutoff = 0.1,
        fill = "#2CA25F", background = "lightblue",
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
      tab_style(
        style = list(
          # cell_fill(color = "#F9E3D6"),
          cell_text(size = px(12))
        ),
        locations = cells_body(
          columns = c(mean_redkill_last12_month, mean_redkill_last24_month_hipp)
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
      cols_width(
        saddle ~ px(60),
        .pred_win ~ px(120),
        trainerName ~ px(60),
        jockeyName ~ px(60),
        driver_ratio_topp ~ px(80),
        trainer_ratio_topp ~ px(80),
        horse_ratio_topp ~ px(80),
        mean_redkill_last12_month ~ px(60),
        mean_redkill_last24_month_hipp~ px(60),
        jour_last_course~ px(50),
        everything() ~ px(90)) %>% 
      cols_hide(driver_ratio_topp_evol)
    

    
    
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
      filter(Proba >= input$tresh2/100,
             rank_P1 <= input$rank_tresh)
    
    text = filtered %>%
      group_by(PP) %>%
      tally() %>%
      mutate(perc = n/sum(n)) %>% 
      filter(PP == 1)
    
    infoBox(
      "% Pred. SP", paste0(round(text$perc, 2)*100,"%"), icon = icon("list"),
      color = "purple"
    )
  })
    
    
  output$sg_stat <- renderInfoBox({
    
    filtered <- result %>% 
      filter(Proba >= input$tresh2/100,
             rank_P1 <= input$rank_tresh)
    
    text = filtered %>%
      group_by(P1) %>%
      tally() %>%
      mutate(perc = n/sum(n)) %>% 
      filter(P1 == 1)
    
    infoBox(
      "% Pred. SG", paste0(round(text$perc, 2)*100,"%"), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
    
    
  })
  
  
  filtered_backtest <- reactive({
    filtered <- result %>% 
      filter(Proba >= input$tresh2/100,
             rank_P1 <= input$rank_tresh)
    
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
  
  
  output$plot_backtest <- renderPlot({
    
    filtered <- filtered_backtest()
    
    if (nrow(filtered) == 0 | !values$authenticated) {
      return(NULL)
    }
    
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