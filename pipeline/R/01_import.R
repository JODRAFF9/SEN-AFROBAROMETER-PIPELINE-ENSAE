# ==============================================================================
# 01_IMPORT.R — Import et nettoyage initial de la base Afrobarometer
# ==============================================================================

# ── Chargement des packages ───────────────────────────────────────────────────
library(haven)      # lecture .dta / .sav
library(labelled)   # gestion labels Stata
library(dplyr)
library(tidyr)
library(stringr)
library(here)

source(here("pipeline", "R", "config.R"))

# ==============================================================================
# FONCTION: importer_base
# Lit la base brute (Stata, SPSS ou CSV) et retourne un data.frame propre.
# ==============================================================================
importer_base <- function(dossier_input = PATHS$input,
                          fichier       = FICHIER_BRUT,
                          verbose       = TRUE) {

  chemin <- file.path(dossier_input, fichier)

  if (!file.exists(chemin)) {
    stop("Fichier introuvable : ", chemin,
         "\nPlacez la base dans le dossier : ", dossier_input)
  }

  ext <- tolower(tools::file_ext(fichier))

  if (verbose) message("[01_import] Lecture de : ", chemin)

  base <- switch(ext,
    dta = haven::read_dta(chemin),
    sav = haven::read_sav(chemin),
    csv = readr::read_csv(chemin, show_col_types = FALSE),
    stop("Extension non supportée : ", ext,
         "\nFormats acceptés : .dta, .sav, .csv")
  )

  n_obs  <- nrow(base)
  n_vars <- ncol(base)

  if (verbose) {
    message(sprintf("[01_import] Base chargée : %d observations, %d variables", n_obs, n_vars))
  }

  base
}

# ==============================================================================
# FONCTION: nettoyer_base
# Applique les transformations communes à toute la base :
#   1. Harmonise les noms de colonnes (snake_case)
#   2. Recode les non-réponses en NA
#   3. Convertit les variables labellisées en facteurs
# ==============================================================================
nettoyer_base <- function(base, verbose = TRUE) {

  if (verbose) message("[01_import] Nettoyage initial...")

  # ── 1. Harmonisation des noms de colonnes ────────────────────────────────
  noms_originaux <- names(base)
  names(base)    <- toupper(trimws(names(base)))

  # ── 2. Recode numérique → NA ─────────────────────────────────────────────
  base <- base |>
    dplyr::mutate(dplyr::across(
      where(is.numeric),
      ~ dplyr::if_else(. %in% CODES_MANQUANTS_NUM, NA_real_, as.double(.))
    ))

  # ── 3. Recode labels → NA puis conversion en facteur ─────────────────────
  base <- base |>
    dplyr::mutate(dplyr::across(
      where(labelled::is.labelled),
      ~ {
        val <- as.character(labelled::to_factor(.))
        val[val %in% CODES_MANQUANTS_LABEL] <- NA
        factor(val)
      }
    ))

  if (verbose) {
    n_na_total <- sum(is.na(base))
    message(sprintf("[01_import] Total de NA après recodage : %d", n_na_total))
  }

  base
}

# ==============================================================================
# FONCTION: verifier_variables_requises
# Vérifie que toutes les variables du mapping config.R sont présentes.
# Retourne un data.frame de diagnostic.
# ==============================================================================
verifier_variables_requises <- function(base, verbose = TRUE) {

  vars_requises <- unique(c(
    ID_INDIVIDU,
    unlist(VARS_DEMO),
    unlist(VARS_GEO),
    unlist(VARS_PROFIL),
    unlist(VARS_VIE_MENAGE)
  ))

  # Garder seulement les noms (pas les vecteurs de plusieurs variables)
  vars_requises <- vars_requises[nchar(vars_requises) > 0]

  presentes  <- vars_requises[vars_requises %in% names(base)]
  absentes   <- vars_requises[!vars_requises %in% names(base)]

  diagnostic <- data.frame(
    variable  = vars_requises,
    presente  = vars_requises %in% names(base),
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message(sprintf("[01_import] Variables requises : %d présentes / %d absentes",
                    length(presentes), length(absentes)))
    if (length(absentes) > 0) {
      message("  Variables absentes : ", paste(absentes, collapse = ", "))
      message("  => Vérifiez le mapping dans config.R pour ce round.")
    }
  }

  diagnostic
}

# ==============================================================================
# FONCTION: detecter_outliers_univaries
# Détecte les valeurs aberrantes dans les variables numériques continues
# via la méthode IQR.
# Retourne un data.frame listant chaque valeur suspecte.
# ==============================================================================
detecter_outliers_univaries <- function(base,
                                        facteur_iqr = SEUILS_QAQC$seuil_outlier_iqr,
                                        verbose      = TRUE) {

  vars_num <- base |>
    dplyr::select(where(is.numeric)) |>
    names()

  resultats <- lapply(vars_num, function(v) {
    x   <- base[[v]]
    x   <- x[!is.na(x)]
    if (length(x) < 10) return(NULL)

    q1  <- quantile(x, 0.25, na.rm = TRUE)
    q3  <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1

    borne_inf <- q1 - facteur_iqr * iqr
    borne_sup <- q3 + facteur_iqr * iqr

    n_outliers <- sum(x < borne_inf | x > borne_sup)
    if (n_outliers == 0) return(NULL)

    data.frame(
      variable   = v,
      n_outliers = n_outliers,
      pct        = round(n_outliers / length(x) * 100, 2),
      min_val    = min(x),
      max_val    = max(x),
      borne_inf  = round(borne_inf, 2),
      borne_sup  = round(borne_sup, 2),
      stringsAsFactors = FALSE
    )
  })

  rapport_outliers <- dplyr::bind_rows(resultats)

  if (verbose && nrow(rapport_outliers) > 0) {
    message(sprintf("[01_import] %d variable(s) avec des outliers IQR détectées.",
                    nrow(rapport_outliers)))
  }

  rapport_outliers
}

# ==============================================================================
# WRAPPER: lancer_import
# Exécute le pipeline d'import complet et retourne une liste structurée.
# ==============================================================================
lancer_import <- function(dossier_input = PATHS$input,
                          fichier       = FICHIER_BRUT,
                          verbose       = TRUE) {

  base_brute    <- importer_base(dossier_input, fichier, verbose)
  base_propre   <- nettoyer_base(base_brute, verbose)
  diagnostic    <- verifier_variables_requises(base_propre, verbose)
  outliers      <- detecter_outliers_univaries(base_propre, verbose = verbose)

  list(
    base        = base_propre,
    diagnostic  = diagnostic,
    outliers    = outliers,
    meta = list(
      n_obs      = nrow(base_propre),
      n_vars     = ncol(base_propre),
      round      = ROUND$numero,
      annee      = ROUND$annee,
      horodatage = Sys.time()
    )
  )
}
