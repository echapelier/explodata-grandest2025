# Objet galerie
output$galerie_especes <- renderUI({
    df <- selection_donnees()
    
    # Pour chaque espèce, compte le nombre d’observations et retient la date de l’observation la plus récente
    nb_observations_especes <- df %>%
      group_by(espece) %>%
      summarise(
        n_obs = n(),
        derniere_obs = max(date, na.rm = TRUE),
        nomvernaculaire = first(nomvernaculaire)
      ) %>%
      arrange(desc(n_obs))  # Classe par nombre d’observations et par ordre décroissant
    
    # Crée la galerie
    items_galerie <- lapply(1:nrow(nb_observations_especes), function(i) {
      especes <- nb_observations_especes[i, ]
      
      div(
        class = "item-galerie",
        
        h4(especes$nomvernaculaire),

        p(
          span(especes$n_obs, style = "font-size: 1.4rem; color: #807662"),
          br(),
          span(format(especes$derniere_obs, "%d/%m/%Y"), style = "font-size: 0.8rem;")
        )
      )
    })
    
    # Place tous les items de la galerie dans un div
    div(
      style = "display: flex; flex-wrap: wrap;",
      items_galerie
    )
  })