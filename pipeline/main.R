# ==============================================================================
# MAIN.R — Pipeline principal Afrobarometer Sénégal
# Usage :
#   En ligne de commande : Rscript pipeline/main.R
#   En interactif        : source("pipeline/main.R")
#
# Prérequis : placer la base brute (base.dta) dans pipeline/input/
#
# Étapes :
#   1. Import & nettoyage
#   2. Table individus consolidée
#   3. Table ménages consolidée
#   4. Contrôle qualité (QAQC)
#   5. Export (CSV + Excel + HTML)
# ==============================================================================

# ── Vérification & installation des packages ──────────────────────────────────
packages_requis <- c("haven", "labelled", "dplyr", "tidyr", "purrr",
                     "stringr", "here", "tibble", "tools")

packages_manquants <- packages_requis[
  !sapply(packages_requis, requireNamespace, quietly = TRUE)
]

if (length(packages_manquants) > 0) {
  stop(
    "Packages manquants — installez avec :\n",
    'install.packages(c(',
    paste0('"', packages_manquants, '"', collapse = ", "), "))"
  )
}

library(here)

# ── Chargement des modules ────────────────────────────────────────────────────
source(here("pipeline", "R", "config.R"))
source(here("pipeline", "R", "utils.R"))
source(here("pipeline", "R", "01_import.R"))
source(here("pipeline", "R", "02_individus.R"))
source(here("pipeline", "R", "03_menages.R"))
source(here("pipeline", "R", "04_qaqc.R"))
source(here("pipeline", "R", "05_export.R"))

# ==============================================================================
# PIPELINE
# ==============================================================================

message("\n", paste(rep("=", 60), collapse = ""))
message(sprintf(" PIPELINE AFROBAROMETER SÉNÉGAL — Round %d (%d)",
                ROUND$numero, ROUND$annee))
message(paste(rep("=", 60), collapse = ""), "\n")

ts_debut <- Sys.time()

# ── ÉTAPE 1 : Import ──────────────────────────────────────────────────────────
message("ÉTAPE 1/5 : Import et nettoyage de la base")
message(paste(rep("-", 40), collapse = ""))
res_import   <- lancer_import(verbose = TRUE)
base         <- res_import$base

# ── ÉTAPE 2 : Table individus ─────────────────────────────────────────────────
message("\nÉTAPE 2/5 : Construction de la table individus")
message(paste(rep("-", 40), collapse = ""))
table_individus <- construire_table_individus(base, verbose = TRUE)

# ── ÉTAPE 3 : Table ménages ───────────────────────────────────────────────────
message("\nÉTAPE 3/5 : Construction de la table ménages")
message(paste(rep("-", 40), collapse = ""))
table_menages <- construire_table_menages(base, verbose = TRUE)

# ── ÉTAPE 4 : QAQC ────────────────────────────────────────────────────────────
message("\nÉTAPE 4/5 : Contrôle qualité (QAQC)")
message(paste(rep("-", 40), collapse = ""))
rapport_qaqc <- produire_qaqc(
  df_individus     = table_individus,
  df_menages       = table_menages,
  rapport_outliers = res_import$outliers,
  verbose          = TRUE
)

# ── ÉTAPE 5 : Export ──────────────────────────────────────────────────────────
message("\nÉTAPE 5/5 : Export des sorties")
message(paste(rep("-", 40), collapse = ""))
chemins <- lancer_export(table_individus, table_menages, rapport_qaqc, verbose = TRUE)

# Rapport QAQC HTML
chemin_html <- generer_rapport_html(rapport_qaqc, base = base, meta = res_import$meta, verbose = TRUE)

# ── Résumé final ──────────────────────────────────────────────────────────────
duree <- round(as.numeric(difftime(Sys.time(), ts_debut, units = "secs")), 1)

message("\n", paste(rep("=", 60), collapse = ""))
message(" PIPELINE TERMINÉ")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("  Durée totale      : %s secondes", duree))
message(sprintf("  Individus traités : %d", nrow(table_individus)))
message(sprintf("  Ménages traités   : %d", nrow(table_menages)))
message(sprintf("  Sorties :"))
message(sprintf("    Individus CSV   : %s", chemins$tables$individus))
message(sprintf("    Ménages CSV     : %s", chemins$tables$menages))
message(sprintf("    QAQC Excel      : %s", chemins$qaqc$excel %||% "(non généré)"))
message(sprintf("    QAQC HTML       : %s", chemin_html %||% "(non généré — installer rmarkdown)"))
message(paste(rep("=", 60), collapse = ""), "\n")

invisible(list(
  base            = base,
  table_individus = table_individus,
  table_menages   = table_menages,
  rapport_qaqc    = rapport_qaqc,
  chemins         = chemins,
  chemin_html     = chemin_html
))
