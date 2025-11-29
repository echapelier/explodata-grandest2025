server <- function(input, output, session) {

  # Première sélection de données en fonction du groupe taxonomique choisi (oiseaux, papillons, plantes)
  selection_donnees <- reactive({
    req(input$choix_groupetaxo)
    df <- liste_jeuxdonnees[[input$choix_groupetaxo]]
    # Applique le type Date aux données du champ date au cas où elles n’auraient pas été reconnues comme telles
    df$date <- as.Date(df$dateobservation)
    df
  })

  # Chargement conditionnel de la liste des communes pour le sélecteur de commune
  observeEvent(
    list(selection_donnees(), input$selecteur_perimetregeo),
    {
      req(input$selecteur_perimetregeo == "Une commune")
      
      communes <- sort(unique(selection_donnees()$commune))
      updateSelectizeInput(
        session,
        inputId = "commune",
        choices = communes,
        server  = TRUE
      )
    },
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # Texte d’explication visible dans la barre de gauche lorsque l’onglet "Galerie des espèces" est sélectionné (par défaut)
  output$explications_galerie <- renderUI({
    req(input$tabs == "Galerie des espèces")
    markdown("À droite, la galerie des espèces liste toutes les espèces observées du groupe taxonomique sélectionné (oiseaux, papillons ou plantes), avec pour chaque espèce son nom, le nombre d’observations dans l’échantillon et la date de l’observation la plus récente.
    
          Chez les oiseaux, on remarque la rareté de la huppe faciée : l’observation la plus récente pour cette espèce remonte à 2014, contre 2022 pour tous les autres oiseaux de l’échantillon.

          ---
    
          Conçu en réponse au concours de visualisation de données 2025 organisé par DataGrandEst, cet explorateur de données poursuit deux objectifs :
          - faciliter l’exploitation des données mises à disposition, c'est-à-dire un échantillon de données d’observations de la biodiversité en région Grand Est ;
          - faire émerger à partir de ces données quelques questions de recherche.

          Compte tenu du caractère échantillonnaire des jeux de données mis à disposition ainsi que des limitations propres à la méthode des observations, il convient de garder à l’esprit que cet outil n’a pas de prétention scientifique et a essentiellement pour but d’illustrer l’intérêt des visualisations pour l’analyse de données.
          
          Il a par ailleurs été fait le choix de ne pas chercher à croiser les données relatives aux oiseaux, aux papillons et aux plantes en raison de leurs grandes différences en termes de nombre d’observations et de périodes couvertes.")
  })

  # Texte d’explication visible dans la barre de gauche lorsque l’onglet "Cartographie" est sélectionné
  output$explications_cartographie <- renderUI({
    req(input$tabs == "Cartographie")
    markdown("L’histogramme ci-dessus vise à faciliter la sélection d’une période de filtrage des observations en montrant le nombre d’observations par mois dans le groupe taxonomique choisi (oiseaux, papillons ou plantes).
    
    À droite, la cartographie permet de voir pour chaque espèce où précisément les apparitions ont été observées. La couleur des points d’observation reflète leur ancienneté : plus la couleur est intense, plus l’observation est récente. Cliquer sur le point d’observation affiche sa date exacte.    
    
    Chez les oiseaux, on peut s’étonner de la concentration des observations de la perruche à collier à Nancy et Metz, tandis que pour certaines plantes comme la pensée et le vulpin des champs, les lieux d’observation sont étrangement alignés.")
  })

  # Texte d’explication visible dans la barre de gauche lorsque l’onglet "Observations par période et périmètre géographique" est sélectionné
  output$explications_comptage <- renderUI({
    req(input$tabs == "Observations par période et périmètre géographique")
    markdown("L’histogramme ci-dessus vise à faciliter la sélection d’une période de filtrage des observations en montrant le nombre d’observations par mois dans le groupe taxonomique (oiseaux, papillons ou plantes) et le périmètre géographique choisis.
    
    Le graphique à droite affiche le nombre d’observations de chaque espèce sur la période et dans le périmètre géographique définis par l’utilisateur.")
  })

  # Affichage conditionnel du sélecteur de périmètre géographique
  output$choix_perimetregeo <- renderUI({
    req(input$tabs == "Observations par période et périmètre géographique")
    radioButtons(
      inputId = "selecteur_perimetregeo", 
      label = "Dans quel périmètre géographique ?", 
      choices = c("Toute la région Grand Est", "Un département", "Une commune"), 
      selected = "Toute la région Grand Est"
    )
  })
  
  # Affichage conditionnel des listes des départements ou communes pour le sélecteur de périmètre géographique
  output$filtre_perimetregeo <- renderUI({
    req(input$tabs == "Observations par période et périmètre géographique")
    req(input$selecteur_perimetregeo)

    df <- selection_donnees()

    switch(
      input$selecteur_perimetregeo,
      "Un département" = selectInput("departement", "Département :", choices = sort(unique(df$departement))),
      "Une commune" = selectizeInput(
        inputId  = "commune",
        label    = "Commune :",
        choices  = NULL,
        multiple = FALSE,
        options  = list(
          placeholder = "Entrez le nom d’une commune",
          maxOptions  = 10
        ),
        width = "100%"
      ),
      NULL
    )
  })

  # Affichage conditionnel du sélecteur de période
  output$choix_periode <- renderUI({
    req(input$tabs == "Cartographie" ||
        input$tabs == "Observations par période et périmètre géographique"
        )
    df <- selection_donnees()
    sliderInput("selecteur_periode", "Sur quelle période ?",
                  min = min(df$date, na.rm = TRUE),
                  max = max(df$date, na.rm = TRUE),
                  value = c(min(df$date, na.rm = TRUE), max(df$date, na.rm = TRUE)),
                  timeFormat = "%d/%m/%Y")
  })
  
  # Filtrage des données en fonction de la période et du périmètre géographique définis par l’utilisateur
  df_selection <- reactive({
    req(input$selecteur_periode)
    df <- selection_donnees()
    # Filtre périmètre géographique
    if (input$selecteur_perimetregeo == "Un département" && !is.null(input$departement)) {
      df <- df %>% filter(departement == input$departement)
    } else if (input$selecteur_perimetregeo == "Une commune" && !is.null(input$commune)) {
      df <- df %>% filter(commune == input$commune)
    }
    # Filtre période
    df <- df %>% filter(date >= input$selecteur_periode[1], date <= input$selecteur_periode[2])
    df
  })

  # Calcule le nombre d’espèces dans la sélection pour fixer ensuite les dimensions du graphique
  # de façon à conserver la même hauteur des barres quelle que soit la taille de la sélection
  nb_max_especes <- reactive({
    length(unique(df_selection()$nomvernaculaire))
  })

  nb_max_observations <- reactive({
    df <- selection_donnees()
    df %>% 
      count(nomvernaculaire) %>% 
      pull(n) %>%
      max(0, .)
  })
  
  # Histogramme des dates des observations
  output$histogramme_dates <- renderPlot({
    req(input$tabs == "Cartographie" ||
        input$tabs == "Observations par période et périmètre géographique"
        )
    df <- selection_donnees()
    # Filtre périmètre géographique
    if (input$selecteur_perimetregeo == "Un département" && !is.null(input$departement)) {
      df <- df %>% filter(departement == input$departement)
    } else if (input$selecteur_perimetregeo == "Une commune" && !is.null(input$commune)) {
      df <- df %>% filter(commune == input$commune)
    }

    # Détermine les dates de début et fin de mois
    etendue_dates <- selection_donnees()$date
    breaks <- seq(
      floor_date(min(etendue_dates, na.rm=TRUE), "month"),
      ceiling_date(max(etendue_dates, na.rm=TRUE), "month"),
      by = "month"
    )

    ggplot(df, aes(date)) +
      geom_histogram(breaks = breaks, fill = "#696523") +
      scale_x_date(
        date_breaks = "1 month", 
        date_labels = "%b %Y",
        expand      = c(0,0)
      ) +
      scale_y_continuous(expansion(mult = c(0, 0.05)), minor_breaks = NULL) +
      theme(
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      labs(x = NULL, y = NULL)
  })
  
  # Graphique du décompte d’observations par espèce
  output$graphique_comptage_especes <- renderPlot({
    req(input$choix_groupetaxo)

    df <- df_selection()
    req(nrow(df) > 0, "Aucune donnée disponible pour cette sélection.")

    decompte_observations <- df %>%
      group_by(nomvernaculaire) %>%
      summarise(n_obs = n(), .groups = "drop") %>%
      arrange(n_obs)

    # Réduit la longueur des noms d’espèces lorsqu’ils sont trop longs (dépassent 20 caractères)
    max_longueur_nom_especes <- 20
    decompte_observations <- decompte_observations %>%
      mutate(
        nom_court = ifelse(
          nchar(nomvernaculaire, type = "chars") > max_longueur_nom_especes,
          paste0(substr(nomvernaculaire, 1, max_longueur_nom_especes), "…"),
          nomvernaculaire
        )
      )

    ggplot(decompte_observations, aes(x = n_obs, y = reorder(nom_court, n_obs))) +
      geom_vline(xintercept = 0,
                 colour     = "#2a2a2a",
                 linewidth       = 1.2) +
      geom_col(fill = couleurs_groupetaxo[[input$choix_groupetaxo]]) +
      geom_text(aes(label = nomvernaculaire),
                x        = 0.005 * nb_max_observations(), # x ne reflète pas un nombre de pixels mais est exprimé dans l’unité de l’abscisse
                hjust    = 0,
                colour   = "#292622",
                size     = 4) +
      labs(title = "Nombre d’observations par espèce", x = NULL, y = NULL) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, nb_max_observations()),
        sec.axis = dup_axis()
      ) +
      theme(
        title = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x.top   = element_text(),
        axis.ticks.x  = element_line(colour = "#2a2a2a"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x  = element_line(colour = "#2a2a2a"),
        panel.ontop = TRUE,
        panel.background = element_rect(fill='transparent')
      )
  }, 
  height = function() {
    # Hauteur dynamique du graphique en fonction du nombre maximum d’espèces à afficher, avec hauteur minimale
     20 * max(7, nb_max_especes())
  })

  # Galerie des espèces
  source("server_galerie.R", local = TRUE)

  # Cartographie
  source("server_carte.R", local = TRUE)
  
  }