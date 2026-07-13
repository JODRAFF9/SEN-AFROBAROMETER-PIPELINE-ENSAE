# ==============================================================================
# INSTALL_PACKAGES.R - Installation de toutes les dependances du pipeline
# Lancer ce script une seule fois avant la premiere execution de main.R
#
#   source("install_packages.R")
#   Rscript install_packages.R
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("  INSTALLATION DES PACKAGES - Pipeline Afrobarometer Senegal\n")
cat("================================================================\n\n")

# ==============================================================================
# 1. DEPENDANCES SYSTEME (Linux / macOS)
#    Le package 'sf' requiert GDAL, GEOS et PROJ.
#    Ces bibliotheques doivent etre installees AVANT d'installer sf.
# ==============================================================================
if (.Platform$OS.type == "unix") {
  cat("-- Verification des dependances systeme pour 'sf'...\n\n")

  os <- tryCatch(
    readLines("/etc/os-release", warn = FALSE),
    error = function(e) character(0)
  )
  is_debian <- any(grepl("debian|ubuntu", os, ignore.case = TRUE))
  is_fedora <- any(grepl("fedora|rhel|centos", os, ignore.case = TRUE))
  is_mac    <- Sys.info()[["sysname"]] == "Darwin"

  gdal_ok <- nchar(Sys.which("gdal-config")) > 0 ||
             nchar(Sys.which("gdal_translate")) > 0

  if (!gdal_ok) {
    cat("  [!] GDAL/GEOS/PROJ non detectes.\n")
    cat("  Installez-les avec la commande correspondant a votre systeme :\n\n")

    if (is_debian) {
      cat("  Ubuntu / Debian :\n")
      cat("    sudo apt-get update\n")
      cat("    sudo apt-get install -y libgdal-dev libgeos-dev libproj-dev\n\n")
    }
    if (is_fedora) {
      cat("  Fedora / RHEL / CentOS :\n")
      cat("    sudo dnf install -y gdal-devel geos-devel proj-devel\n\n")
    }
    if (is_mac) {
      cat("  macOS (Homebrew) :\n")
      cat("    brew install gdal geos proj\n\n")
    }
    cat("  Apres l'installation systeme, relancez ce script.\n\n")
  } else {
    cat("  [OK] GDAL detecte sur le systeme.\n\n")
  }
}

# ==============================================================================
# 2. PACKAGES R
# ==============================================================================

# ── Groupes de packages ────────────────────────────────────────────────────────
groupes <- list(

  "Donnees et manipulation" = c(
    "haven",      # Lecture fichiers Stata (.dta) et SPSS (.sav)
    "labelled",   # Gestion des labels Stata
    "dplyr",      # Manipulation de donnees (tidyverse)
    "tidyr",      # Mise en forme (pivot, nest...)
    "purrr",      # Programmation fonctionnelle
    "stringr",    # Traitement de chaines de caracteres
    "tibble",     # Tableaux modernes
    "here",       # Chemins relatifs robustes
    "readxl",     # Lecture du fichier de mapping Excel
    "tools"       # Utilitaires de base R
  ),

  "Rapport QAQC HTML" = c(
    "rmarkdown",  # Generation de rapports HTML / PDF
    "knitr",      # Moteur de rendu R Markdown
    "kableExtra", # Tableaux HTML enrichis
    "ggplot2",    # Visualisations
    "openxlsx"    # Export Excel (.xlsx)
  ),

  "Cartographie (etape 08)" = c(
    "sf",         # Donnees spatiales (Simple Features) - necessite GDAL
    "geodata",    # Telechargement shapefiles GADM
    "patchwork"   # Assemblage multi-cartes
  )
)

# ── Installation ──────────────────────────────────────────────────────────────
resultats <- list()

for (groupe in names(groupes)) {
  pkgs <- groupes[[groupe]]
  cat(sprintf("-- %s\n", groupe))

  for (pkg in pkgs) {
    deja_installe <- requireNamespace(pkg, quietly = TRUE)

    if (deja_installe) {
      cat(sprintf("   [OK] %-18s (deja installe)\n", pkg))
      resultats[[pkg]] <- "ok"
    } else {
      cat(sprintf("   [..] %-18s installation...\n", pkg))

      succes <- tryCatch({
        # Pour sf : tenter d'abord via r-universe (binaires pre-compiles)
        if (pkg == "sf" && grepl("linux", R.version$os)) {
          install.packages(pkg,
            repos = c("https://r-spatial.r-universe.dev",
                      "https://cloud.r-project.org"),
            quiet = TRUE
          )
        } else {
          install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
        }
        requireNamespace(pkg, quietly = TRUE)
      }, error   = function(e) FALSE,
         warning = function(w) requireNamespace(pkg, quietly = TRUE))

      if (succes) {
        cat(sprintf("   [OK] %-18s installe avec succes\n", pkg))
        resultats[[pkg]] <- "ok"
      } else {
        cat(sprintf("   [!!] %-18s ECHEC d'installation\n", pkg))
        resultats[[pkg]] <- "echec"
      }
    }
  }
  cat("\n")
}

# ==============================================================================
# 3. RAPPORT FINAL
# ==============================================================================
ok     <- names(resultats)[resultats == "ok"]
echecs <- names(resultats)[resultats == "echec"]

cat("================================================================\n")
cat("  BILAN\n")
cat("================================================================\n")
cat(sprintf("  Packages OK     : %d / %d\n", length(ok), length(resultats)))

if (length(echecs) == 0) {
  cat("  Tous les packages sont installes.\n")
  cat("  Vous pouvez lancer le pipeline : source('main.R')\n")
} else {
  cat(sprintf("  Packages en echec (%d) :\n", length(echecs)))
  for (pkg in echecs) cat(sprintf("    - %s\n", pkg))
  cat("\n")
  cat("  Si 'sf' est en echec, installez d'abord les libs systeme\n")
  cat("  (voir les commandes affichees plus haut), puis relancez.\n")
  cat("\n")
  cat("  Les packages en echec sont optionnels : le pipeline\n")
  cat("  fonctionnera sans eux (cartographie et/ou export HTML\n")
  cat("  seront ignores automatiquement).\n")
}

cat("================================================================\n\n")

invisible(resultats)
