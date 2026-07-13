# ==============================================================================
# 05_PONDERATION.R — Application des poids d'enquête et estimations pondérées
# Afrobarometer fournit une variable de pondération (WITHINWT ou équivalent)
# qui corrige les biais de sélection et permet des inférences nationales.
# ==============================================================================

library(dplyr)
library(tidyr)
library(here)

source(here("R", "config.R"))
source(here("R", "utils.R"))

# ==============================================================================
# FONCTION: detecter_variable_poids
# Cherche la variable de pondération dans la base brute.
# Afrobarometer utilise typiquement : WITHINWT, COMBINWT, WT, WEIGHT
# ==============================================================================
detecter_variable_poids <- function(base, verbose = TRUE) {

  candidats <- c("WITHINWT", "COMBINWT", "WT", "WEIGHT", "POIDS",
                 "withinwt", "combinwt", "wt", "weight")
  trouvee   <- candidats[candidats %in% names(base)]

  if (length(trouvee) == 0) {
    if (verbose) message("[05_ponderation] Aucune variable de pondération trouvée. Poids unitaires utilisés.")
    return(NULL)
  }

  choix <- trouvee[1]
  if (verbose) message("[05_ponderation] Variable de pondération détectée : ", choix)
  choix
}

# ==============================================================================
# FONCTION: appliquer_poids
# Ajoute la colonne `poids` aux tables individus et ménages.
# Si aucune variable de pondération n'existe, poids = 1.
# ==============================================================================
appliquer_poids <- function(df_individus, df_menages, base_brute, verbose = TRUE) {

  var_poids <- detecter_variable_poids(base_brute, verbose)

  if (is.null(var_poids)) {
    df_individus$poids <- 1
    df_menages$poids   <- 1
    return(list(individus = df_individus, menages = df_menages,
                var_poids = "unitaire"))
  }

  # Joindre les poids via l'identifiant individu
  poids_df <- base_brute |>
    dplyr::select(dplyr::all_of(c(ID_INDIVIDU, var_poids))) |>
    dplyr::rename(poids = dplyr::all_of(var_poids))

  col_id <- "id_individu"

  df_individus <- df_individus |>
    dplyr::left_join(poids_df, by = stats::setNames(ID_INDIVIDU, col_id)) |>
    dplyr::mutate(poids = dplyr::if_else(is.na(poids) | poids <= 0, 1, poids))

  df_menages <- df_menages |>
    dplyr::left_join(poids_df, by = stats::setNames(ID_INDIVIDU, col_id)) |>
    dplyr::mutate(poids = dplyr::if_else(is.na(poids) | poids <= 0, 1, poids))

  if (verbose) {
    message(sprintf("[05_ponderation] Poids appliqués : min=%.3f  max=%.3f  somme=%.1f",
                    min(df_individus$poids, na.rm = TRUE),
                    max(df_individus$poids, na.rm = TRUE),
                    sum(df_individus$poids, na.rm = TRUE)))
  }

  list(individus = df_individus, menages = df_menages, var_poids = var_poids)
}

# ==============================================================================
# FONCTION: estimer_proportion_ponderee
# Calcule une proportion pondérée avec intervalle de confiance à 95 %.
# Méthode : approximation linéaire (delta method).
# ==============================================================================
estimer_proportion_ponderee <- function(df, var_cat, poids_col = "poids",
                                         niveau_confiance = 0.95) {

  alpha <- 1 - niveau_confiance
  z     <- qnorm(1 - alpha / 2)

  df |>
    dplyr::filter(!is.na(.data[[var_cat]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(var_cat))) |>
    dplyr::summarise(
      effectif_pondere = sum(.data[[poids_col]], na.rm = TRUE),
      effectif_brut    = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      total            = sum(effectif_pondere),
      proportion       = effectif_pondere / total,
      erreur_std       = sqrt(proportion * (1 - proportion) / sum(effectif_brut)),
      ic_inf           = pmax(0, proportion - z * erreur_std),
      ic_sup           = pmin(1, proportion + z * erreur_std),
      proportion_pct   = round(proportion * 100, 1),
      ic_inf_pct       = round(ic_inf    * 100, 1),
      ic_sup_pct       = round(ic_sup    * 100, 1)
    ) |>
    dplyr::select(-total, -erreur_std, -proportion, -ic_inf, -ic_sup)
}

# ==============================================================================
# FONCTION: estimations_ponderees
# Produit les estimations pondérées des principales variables catégorielles.
# ==============================================================================
estimations_ponderees <- function(df_individus, df_menages, verbose = TRUE) {

  if (verbose) message("[05_ponderation] Calcul des estimations pondérées...")

  resultats <- list()

  vars_ind <- c("genre", "milieu", "region", "niveau_etudes",
                "statut_emploi_principal", "isic_section_principal")

  for (v in vars_ind) {
    if (v %in% names(df_individus)) {
      resultats[[paste0("pond_", v)]] <- estimer_proportion_ponderee(df_individus, v)
    }
  }

  # Privation pondérée par ménage
  if ("indice_privation" %in% names(df_menages) && "poids" %in% names(df_menages)) {
    resultats$pond_privation_stats <- df_menages |>
      dplyr::summarise(
        moyenne_pond = stats::weighted.mean(indice_privation, poids, na.rm = TRUE),
        pct_zero_privation_pond = sum(poids[indice_privation == 0], na.rm = TRUE) /
                                  sum(poids, na.rm = TRUE) * 100
      ) |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
  }

  if (verbose) message(sprintf("[05_ponderation] %d estimation(s) pondérée(s) produites.", length(resultats)))
  resultats
}

# ==============================================================================
# WRAPPER: lancer_ponderation
# ==============================================================================
lancer_ponderation <- function(df_individus, df_menages, base_brute, verbose = TRUE) {

  res_poids   <- appliquer_poids(df_individus, df_menages, base_brute, verbose)
  estimations <- estimations_ponderees(res_poids$individus, res_poids$menages, verbose)

  list(
    individus   = res_poids$individus,
    menages     = res_poids$menages,
    var_poids   = res_poids$var_poids,
    estimations = estimations
  )
}
