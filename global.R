library(shiny)
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(ggplot2)
library(leaflet)
library(bslib)
library(thematic)

# Dans les 3 jeux de données oiseaux, papillons et plantes, les champs longitude et latitude suivent l’écriture "48,057"
# Nous devons les transformer pour leur appliquer le modèle "48.057"
transformer_virgule_point <- function(x) as.numeric(gsub(",", ".", x, fixed = TRUE))

# Les noms vernaculaires de papillons contiennent les articles ainsi que de nombreux autres noms qui rendent l’affichage peu esthétique
# Nous allons les réécrire suivant le modèle "nom vernaculaire princpal (aussi appelé...)"
reecrire_nom_papillon_vec <- function(x) {
  # Supprime les parenthèses
  x <- gsub("\\s*\\([^)]*\\)", "", x)
  # Supprime les espaces avant les traits d’union
  x <- gsub("\\s+-", "-", x)
  # Supprime les espaces après les traits d’union
  x <- gsub("-\\s+", "-", x)
  
  liste_noms <- strsplit(x, ",")
  
  vapply(liste_noms, function(noms) {
    noms <- trimws(noms)
    premier_nom <- noms[1]
    autres_noms <- noms[-1]
  
    if (length(autres_noms) > 0 && !all(autres_noms == "")) {
      paste(premier_nom, "(aussi appelé", paste0(paste(autres_noms, collapse = ", "), ")"))
    } else {
      premier_nom
    }
  }, character(1))
}

# Assurons-nous que tous les noms d’espèces commencent par une majuscule
maj_premiercaractere <- function(x){
  stopifnot(is.character(x))
  
  out <- x
  idx <- !is.na(x) & nzchar(x)
  
  out[idx] <- paste0(
    toupper(substring(x[idx], 1, 1)),
    substring(x[idx], 2)
  )
  
  out
}

# Charge le jeux de données oiseaux
donnees_oiseaux   <- read_csv("Data/01_oiseaux.csv",
                      col_types = cols(
                        latitude = col_character(),
                        longitude = col_character()
                      )
                      ) %>%
             mutate(
               across(c(latitude, longitude), transformer_virgule_point)
             )

# Charge le jeux de données papillons
donnees_papillons <- read_csv("Data/02_papillons.csv",
                      col_types = cols(
                        latitude = col_character(),
                        longitude = col_character()
                      )
                      ) %>%
             mutate(
               across(c(latitude, longitude), transformer_virgule_point),
               nomvernaculaire = reecrire_nom_papillon_vec(nomvernaculaire)
             )

# Charge le jeux de données plantes
donnees_plantes   <- read_csv("Data/03_plantes.csv",
                      col_types = cols(
                        latitude = col_character(),
                        longitude = col_character()
                      )
                      ) %>%
             mutate(
               across(c(latitude, longitude), transformer_virgule_point),
               nomvernaculaire = maj_premiercaractere(nomvernaculaire)
             )

# Liste des jeux de données pour le menu déroulant "Je m'intéresse aux"
liste_jeuxdonnees <- list(
  'oiseaux'   = donnees_oiseaux,
  'papillons' = donnees_papillons,
  'plantes'   = donnees_plantes
)

# Associe chaque groupe taxonomique à une couleur
couleurs_groupetaxo <- c(
  oiseaux   = "#D24826",
  papillons = "#777AAB",
  plantes   = "#48AA68"
)

# shiny::runApp()