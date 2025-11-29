# Thème par défaut pour les graphiques ggplot2
ggplot2::theme_set(ggplot2::theme_minimal())

# Applique le thème personnalisé aux graphiques ggplot2
thematic_shiny()

ui <- fluidPage(
  theme = bs_theme(
    bg = "#D9E5D2", 
    fg = "#000",
    base_font = font_google("Bricolage Grotesque"),
    font_scale = 0.8,
    `h1-font-size` = "1.2rem",
    `h4-font-size` = "0.8rem",
    `form-check-input-bg` = "#696523",
    `form-check-input-checked-bg-color` = "#696523",
    `nav-link-color` = "#D9E5D2",
    `nav-link-hover-color` = "#D9E5D2",
    `nav-pills-link-active-bg` = "#292622"
  ),

  # On utilise des doubles {{ }} pour les propriétés CSS en raison de glue qui lit les { } simples comme des conteneurs de variables R
  tags$style(HTML(glue("
    div.row {{
      margin-top: 0.5rem;
    }}

    div.col-sm-9 {{
      padding-left: 0;
    }}

    form.well {{ 
      border:none;
      background-color: inherit;
      padding-top: 0;
    }}

    h1 {{
      height: 60px;
      margin: 0;
      display: flex;
      align-items: flex-end; 
      border-bottom: 1px solid black;
      padding-bottom: 5px;
    }}

    @media (min-width: 576px) and (max-width: 1400px) {{
      h1 {{
        font-size: 1.3vw;
      }}
    }}

    label.control-label {{
      font-weight: bold;
      font-size: 0.9rem;
    }}

    .selectize-control .selectize-input {{
      height: 30px;
    }}

    .selectize-control .selectize-input > div {{
      max-width: 235px; 
      white-space: nowrap; 
      overflow: hidden; 
      text-overflow: ellipsis; 
    }}

    #choix_groupetaxo input[type='radio'] {{ display: none; }}
    
    #choix_groupetaxo > .shiny-options-group label {{
      display: inline-block;
      padding: 6px 8px;
      border-radius: 8px;
      border: 5px solid transparent;
    }}

    #choix_groupetaxo > .shiny-options-group label:has(input[value='oiseaux']) {{
      background: {couleurs_groupetaxo[['oiseaux']]};
    }}

    #choix_groupetaxo > .shiny-options-group label:has(input[value='papillons']) {{
      background: {couleurs_groupetaxo[['papillons']]};
    }}

    #choix_groupetaxo > .shiny-options-group label:has(input[value='plantes']) {{
      background: {couleurs_groupetaxo[['plantes']]};
    }}

    #choix_groupetaxo > .shiny-options-group label:has(input[type='radio']:checked) {{
      border: 5px solid #000;
    }}

    #selecteur_periode-label + span {{
      margin-left: 25px;
    }}

    .irs-from, .irs-to, .irs-bar, .irs-handle, .irs-single {{
      background-color: #696523 !important;
      color: #fff !important;
    }}

    .irs-min, .irs-max {{
      visibility: hidden !important;
    }}

    #histogramme_dates {{
      margin-bottom: 20px;
    }}

    #tabs {{
      display: flex;
      flex-wrap: nowrap;
      align-items: stretch;
      height: 60px;
      border-bottom: 1px solid black;
    }}

    #tabs .nav-item {{
      flex: 1 1 0;
      min-width: 0;
      text-align: center;
      margin: 0 2px 0 2px;
      border-radius: 20px 20px 0 0;
      background-color: #837561;
    }}
  
    #tabs .nav-link {{
      display: block;
      width: 100%;
      height: 60px;
      line-height: 45px;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      border-radius: 20px 20px 0 0;
    }}

    .item-galerie {{
      border: 2px solid #ddd;
      border-radius: 8px;
      padding: 7px;
      margin: 10px; 
      background: #f9f9f9;
      display: inline-block;
      width: 150px;
      vertical-align: top;
    }}
  "))),

  sidebarLayout(

    # Menu barre de gauche
    sidebarPanel(
      h1("Explorateur de données biodiversité en région Grand Est"),
      radioButtons("choix_groupetaxo", "Je m’intéresse aux", choices = names(liste_jeuxdonnees), selected = "oiseaux", inline = TRUE),
      uiOutput("choix_espece"),
      uiOutput("choix_perimetregeo"),
      uiOutput("filtre_perimetregeo"),
      uiOutput("choix_periode"),
      plotOutput("histogramme_dates", height = 100),
      uiOutput("explications_galerie"),
      uiOutput("explications_cartographie"),
      uiOutput("explications_comptage"),
      width = 3
    ),

    # Espace contenu
    mainPanel(
      tabsetPanel(
        id = "tabs",
        type = "pills",
        tabPanel("Galerie des espèces",
          class = "ms-2 mt-3",
          uiOutput("galerie_especes")
        ),
        tabPanel("Cartographie", 
          class = "ms-2 mt-3",
          leaflet::leafletOutput("carte_observations", height = 600)
        ),
        tabPanel("Observations par période et périmètre géographique", 
          class = "ms-2 mt-3",
          plotOutput("graphique_comptage_especes")
        )
      ),
      width = 9  
    )
  ),

  tags$script(HTML("
    $(document).on('shiny:bound', function() {
      $('#choix_groupetaxo .shiny-options-group label')
        .addClass('text-center flex-fill');
      $('#choix_groupetaxo .shiny-options-group')
        .addClass('column-gap-2');
    });
  "))

)