# Fixe les limites de la carte
limites_carte <- list(
    # latitude, longitude
    c(47, 3),    # coin sud-ouest
    c(50, 9)     # coin nord-est
  )

# Crée le fonds de carte
output$carte_observations <- leaflet::renderLeaflet({
    leaflet(options = leafletOptions(
                         maxBounds = limites_carte,
                         minZoom   = 7
                       )) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      # Vue initiale
      setView(lng = 6.15, lat = 48.65, zoom = 7)
  })

# Sélectionne les données à ajouter au fonds de carte
df_pour_carte <- reactive({
  df <- selection_donnees()
  df <- df %>% 
    filter(!is.na(latitude), !is.na(longitude))  # Ignore les observations dépourvues de coordonnées géographiques
    req(nrow(df) > 0)
  df
})

# Rend le sélecteur d’espèces visible uniquement lorsque l’onglet "Cartographie" est sélectionné / actif
output$choix_espece <- renderUI({
  req(input$tabs == "Cartographie")
  df <- df_pour_carte()
  especes <- sort(unique(df$nomvernaculaire))
  selectInput(
      inputId  = "selecteur_espece",
      label    = "en particulier à l’espèce",
      choices  = especes,
      selected = especes[1]
    )
})

# Filtre les données pour ne retenir que les observations de l’espèce choisie lors de la période sélectionnée
df_selection_carte <- reactive({
  df <- df_pour_carte()
  req(input$selecteur_espece, input$selecteur_periode)
  df <- df %>%
    filter(nomvernaculaire == input$selecteur_espece,
          date >= input$selecteur_periode[1],
          date <= input$selecteur_periode[2])
  df
})

# Actualise les couleurs des marqueurs en fonction de l’ancienneté des observations dans la sélection active d’observations
# Plus l’observation est récente, plus la couleur est intense
observe({
  df <- df_selection_carte()
  req(input$choix_groupetaxo)
  req(nrow(df) > 0)

  pal <- colorNumeric(
    palette = colorRampPalette(c("#fef3e7", couleurs_groupetaxo[[input$choix_groupetaxo]]))(100),
    domain  = df$date,
    na.color = "#ffffff"
  )

  leafletProxy("carte_observations", data = df) %>%
    clearMarkers() %>%
    addCircleMarkers(
      ~longitude, ~latitude,
      fillColor   = ~pal(date),
      color       = "grey",
      weight      = 1,
      radius      = 5,
      fillOpacity = 0.9,
      popup       = ~paste0("<strong>Date :</strong> ",
                            format(date, "%d/%m/%Y"))
    )
})