# ==============================================================================
# MAIN.R — Pipeline principal Afrobarometer Senegal
# Usage :
#   En ligne de commande : Rscript main.R
#   En interactif        : source("main.R")
#
# Prerequis : placer la base brute (base.dta) dans input/
#
# Etapes :
#   1. Import et nettoyage
#   2. Table individus consolidee
#   3. Table menages consolidee
#   4. Controle qualite (QAQC)
#   5. Ponderation et estimations pondereees
#   6. Analyses thematiques avancees
#   7. Export (CSV + Excel + HTML)
#   8. Cartographie thematique (cartes choroplethes par region)
# ==============================================================================

# -- Verification & installation des packages ---------------------------------
packages_requis <- c("haven", "labelled", "dplyr", "tidyr", "purrr",
                     "stringr", "here", "tibble", "tools", "readxl")

packages_manquants <- packages_requis[
  !sapply(packages_requis, requireNamespace, quietly = TRUE)
]

if (length(packages_manquants) > 0) {
  stop(
    "Packages manquants - installez avec :\n",
    'install.packages(c(',
    paste0('"', packages_manquants, '"', collapse = ", "), "))"
  )
}

library(here)

# -- Chargement des modules ---------------------------------------------------
source(here("R", "config.R"))
source(here("R", "utils.R"))
source(here("R", "01_import.R"))
source(here("R", "02_individus.R"))
source(here("R", "03_menages.R"))
source(here("R", "04_qaqc.R"))
source(here("R", "05_ponderation.R"))
source(here("R", "06_analyse.R"))
source(here("R", "07_export.R"))
if (requireNamespace("sf", quietly = TRUE)) {
  source(here("R", "08_cartographie.R"))
}

# ==============================================================================
# PIPELINE
# ==============================================================================

message("\n", paste(rep("=", 60), collapse = ""))
message(sprintf(" PIPELINE AFROBAROMETER SENEGAL - Round %d (%d) - 8 etapes",
                ROUND$numero, ROUND$annee))
message(paste(rep("=", 60), collapse = ""), "\n")

ts_debut <- Sys.time()

# -- ETAPE 1 : Import ---------------------------------------------------------
message("ETAPE 1/7 : Import et nettoyage de la base")
message(paste(rep("-", 40), collapse = ""))
res_import <- lancer_import(verbose = TRUE)
base       <- res_import$base

# -- ETAPE 2 : Table individus ------------------------------------------------
message("\nETAPE 2/7 : Construction de la table individus")
message(paste(rep("-", 40), collapse = ""))
table_individus <- construire_table_individus(base, verbose = TRUE)

# -- ETAPE 3 : Table menages --------------------------------------------------
message("\nETAPE 3/7 : Construction de la table menages")
message(paste(rep("-", 40), collapse = ""))
table_menages <- construire_table_menages(base, verbose = TRUE)

# -- ETAPE 4 : QAQC -----------------------------------------------------------
message("\nETAPE 4/7 : Controle qualite (QAQC)")
message(paste(rep("-", 40), collapse = ""))
rapport_qaqc <- produire_qaqc(
  df_individus     = table_individus,
  df_menages       = table_menages,
  rapport_outliers = res_import$outliers,
  verbose          = TRUE
)

# -- ETAPE 5 : Ponderation ----------------------------------------------------
message("\nETAPE 5/7 : Ponderation et estimations ponderees")
message(paste(rep("-", 40), collapse = ""))
res_pond        <- lancer_ponderation(table_individus, table_menages, base, verbose = TRUE)
table_individus <- res_pond$individus
table_menages   <- res_pond$menages

# -- ETAPE 6 : Analyses thematiques ------------------------------------------
message("\nETAPE 6/7 : Analyses thematiques avancees")
message(paste(rep("-", 40), collapse = ""))
res_analyse     <- lancer_analyse(table_individus, table_menages, verbose = TRUE)
table_individus <- res_analyse$individus

# -- ETAPE 7 : Export ---------------------------------------------------------
message("\nETAPE 7/7 : Export des sorties")
message(paste(rep("-", 40), collapse = ""))
chemins     <- lancer_export(table_individus, table_menages, rapport_qaqc, verbose = TRUE)
chemin_html <- generer_rapport_html(rapport_qaqc, base = base, meta = res_import$meta, verbose = TRUE)

# -- ETAPE 8 : Cartographie (optionnelle) -------------------------------------
message("\nETAPE 8/8 : Cartographie thematique par region")
message(paste(rep("-", 40), collapse = ""))
res_carto <- NULL
if (exists("lancer_cartographie")) {
  res_carto <- tryCatch(
    lancer_cartographie(table_individus, table_menages, verbose = TRUE),
    error = function(e) {
      message("[08_carto] Cartographie ignoree : ", conditionMessage(e))
      NULL
    }
  )
} else {
  message("[08_carto] Package 'sf' absent - etape ignoree (installer sf, geodata, patchwork)")
}

# -- Resume final -------------------------------------------------------------
duree <- round(as.numeric(difftime(Sys.time(), ts_debut, units = "secs")), 1)

message("\n", paste(rep("=", 60), collapse = ""))
message(" PIPELINE TERMINE")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("  Duree totale         : %s secondes", duree))
message(sprintf("  Individus traites    : %d", nrow(table_individus)))
message(sprintf("  Menages traites      : %d", nrow(table_menages)))
message(sprintf("  Variable de poids    : %s", res_pond$var_poids))
message(sprintf("  Indice bien-etre moy : %.1f / 100",
                mean(table_individus$indice_bien_etre, na.rm = TRUE)))
message("  Sorties :")
message(sprintf("    Individus CSV      : %s", chemins$tables$individus))
message(sprintf("    Menages CSV        : %s", chemins$tables$menages))
message(sprintf("    QAQC Excel         : %s", chemins$qaqc$excel %||% "(non genere)"))
message(sprintf("    QAQC HTML          : %s", chemin_html %||% "(non genere - installer rmarkdown)"))
if (!is.null(res_carto)) {
  message(sprintf("    Cartes PNG         : %d carte(s) dans output/cartes/",
                  length(res_carto$chemins)))
}
message(paste(rep("=", 60), collapse = ""), "\n")

res_pipeline <- list(
  base            = base,
  table_individus = table_individus,
  table_menages   = table_menages,
  rapport_qaqc    = rapport_qaqc,
  estimations_pond = res_pond$estimations,
  analyse         = res_analyse,
  chemins         = chemins,
  chemin_html     = chemin_html,
  cartographie    = res_carto
)

# -- OUVERTURE AUTOMATIQUE DES SORTIES ----------------------------------------
if (interactive()) {

  # 1. Rapport QAQC HTML (ouvre dans le navigateur)
  if (!is.null(chemin_html) && file.exists(chemin_html)) {
    message("[main] Ouverture du rapport QAQC dans le navigateur...")
    utils::browseURL(chemin_html)
  }

  # 2. Plateforme Shiny (lance et ouvre dans le navigateur)
  chemin_shiny <- here("shiny", "app.R")
  if (file.exists(chemin_shiny) &&
      requireNamespace("shiny", quietly = TRUE) &&
      requireNamespace("bslib", quietly = TRUE)) {
    message("[main] Lancement de la plateforme Shiny...")
    message("[main] Fermer la fenetre Shiny (Ctrl+C dans la console) pour reprendre la session R.")
    shiny::runApp(here("shiny"), launch.browser = TRUE)
  } else if (!file.exists(chemin_shiny)) {
    message("[main] Plateforme Shiny introuvable (shiny/app.R manquant).")
  } else {
    message("[main] Packages 'shiny'/'bslib' manquants - plateforme non lancee.")
    message("       Installez avec : install.packages(c('shiny','bslib','DT'))")
  }

}

invisible(res_pipeline)
