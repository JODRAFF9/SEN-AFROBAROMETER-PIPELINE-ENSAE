# ==============================================================================
# 03_MENAGES.R — Construction de la table ménages consolidée
# ==============================================================================

library(dplyr)
library(here)

source(here("pipeline", "R", "config.R"))
source(here("pipeline", "R", "utils.R"))

# ==============================================================================
# FONCTION: extraire_profil_repondant_menage
# Variables démographiques du répondant (représentant du ménage), préfixées rep_
# ==============================================================================
extraire_profil_repondant_menage <- function(base) {

  dispo  <- VARS_DEMO[unlist(VARS_DEMO) %in% names(base)]
  cols   <- unname(unlist(dispo))
  cibles <- paste0("rep_", names(dispo))

  df <- base |>
    dplyr::select(all_of(c(ID_INDIVIDU, cols))) |>
    dplyr::rename(
      id_individu = all_of(ID_INDIVIDU),
      !!!setNames(cols, cibles)
    )

  if ("rep_genre" %in% names(df))
    df <- dplyr::mutate(df, rep_genre = libeller_genre(rep_genre))

  if ("rep_niveau_etudes" %in% names(df))
    df <- dplyr::mutate(df, rep_niveau_etudes = libeller_niveau_etudes(rep_niveau_etudes))

  df
}

# ==============================================================================
# FONCTION: extraire_geo_menage
# ==============================================================================
extraire_geo_menage <- function(base) {

  df <- selectionner_renommer(base, ID_INDIVIDU, VARS_GEO)

  if ("region" %in% names(df))
    df <- dplyr::mutate(df, region = libeller_region(region))

  if ("milieu" %in% names(df))
    df <- dplyr::mutate(df, milieu = libeller_milieu(milieu))

  df
}

# ==============================================================================
# FONCTION: extraire_services_zone_menage
# ==============================================================================
extraire_services_zone_menage <- function(base) {

  df <- selectionner_renommer(base, ID_INDIVIDU, VARS_SERVICES)

  if ("source_eau" %in% names(df))
    df <- dplyr::mutate(df, source_eau = libeller_source_eau(source_eau))

  if ("electricite_acces" %in% names(df))
    df <- dplyr::mutate(df, electricite_acces = libeller_electricite(electricite_acces))

  df
}

# ==============================================================================
# FONCTION: extraire_conditions_vie_menage
# ==============================================================================
extraire_conditions_vie_menage <- function(base) {

  df <- selectionner_renommer(base, ID_INDIVIDU, VARS_VIE_MENAGE)

  cols_priv <- intersect(names(VARS_VIE_MENAGE), names(df))

  if (length(cols_priv) == 0) return(df)

  df <- df |>
    dplyr::mutate(
      dplyr::across(all_of(cols_priv), ~ suppressWarnings(as.numeric(.)))
    )

  # Indicateurs de privation fréquente (val >= 2 = quelques fois ou plus)
  df <- df |>
    dplyr::mutate(
      dplyr::across(
        all_of(cols_priv),
        ~ dplyr::if_else(. >= 2, 1L, 0L, missing = NA_integer_),
        .names = "prive_{.col}"
      ),
      indice_privation = rowSums(
        dplyr::pick(starts_with("prive_")),
        na.rm = FALSE
      ),
      groupe_privation = dplyr::case_when(
        indice_privation == 0 ~ "Aucune privation",
        indice_privation == 1 ~ "Privation légère (1 type)",
        indice_privation == 2 ~ "Privation modérée (2 types)",
        indice_privation >= 3 ~ "Privation sévère (3+ types)",
        TRUE                  ~ NA_character_
      )
    )

  df
}

# ==============================================================================
# WRAPPER: construire_table_menages
# ==============================================================================
construire_table_menages <- function(base, verbose = TRUE) {

  if (verbose) message("[03_menages] Extraction profil répondant/ménage...")
  df_rep <- extraire_profil_repondant_menage(base)

  if (verbose) message("[03_menages] Extraction géographie ménage...")
  df_geo <- extraire_geo_menage(base)

  if (verbose) message("[03_menages] Extraction services sociaux zone...")
  df_svc <- extraire_services_zone_menage(base)

  if (verbose) message("[03_menages] Extraction conditions de vie...")
  df_vie <- extraire_conditions_vie_menage(base)

  if (verbose) message("[03_menages] Fusion...")
  table_men <- df_rep |>
    dplyr::left_join(df_geo, by = "id_individu") |>
    dplyr::left_join(df_svc, by = "id_individu") |>
    dplyr::left_join(df_vie, by = "id_individu") |>
    dplyr::mutate(
      round_afrobarometer = ROUND$numero,
      annee_enquete       = ROUND$annee,
      pays                = ROUND$pays
    )

  if (verbose) {
    message(sprintf("[03_menages] Table ménages : %d x %d",
                    nrow(table_men), ncol(table_men)))
  }

  table_men
}
