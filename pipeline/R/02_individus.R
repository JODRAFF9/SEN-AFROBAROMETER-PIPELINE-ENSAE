# ==============================================================================
# 02_INDIVIDUS.R — Construction de la table individus consolidée
# ==============================================================================

library(dplyr)
library(here)

source(here("pipeline", "R", "config.R"))
source(here("pipeline", "R", "utils.R"))

# ==============================================================================
# FONCTION: extraire_demo_individu
# ==============================================================================
extraire_demo_individu <- function(base) {

  df <- selectionner_renommer(base, ID_INDIVIDU, VARS_DEMO)

  if ("genre" %in% names(df))
    df <- dplyr::mutate(df, genre = libeller_genre(genre))

  if ("niveau_etudes" %in% names(df))
    df <- dplyr::mutate(df, niveau_etudes = libeller_niveau_etudes(niveau_etudes))

  df
}

# ==============================================================================
# FONCTION: extraire_geo_individu
# ==============================================================================
extraire_geo_individu <- function(base) {

  df <- selectionner_renommer(base, ID_INDIVIDU, VARS_GEO)

  if ("region" %in% names(df))
    df <- dplyr::mutate(df, region = libeller_region(region))

  if ("milieu" %in% names(df))
    df <- dplyr::mutate(df, milieu = libeller_milieu(milieu))

  df
}

# ==============================================================================
# FONCTION: extraire_profil_individu
# ==============================================================================
extraire_profil_individu <- function(base) {

  # ── Emploi ─────────────────────────────────────────────────────────────────
  df_emploi <- selectionner_renommer(base, ID_INDIVIDU, VARS_EMPLOI)

  if ("statut_emploi_principal" %in% names(df_emploi))
    df_emploi <- dplyr::mutate(df_emploi,
      statut_emploi_principal = libeller_statut_emploi(statut_emploi_principal))

  # Classification ISIC Rev 4
  for (col_act in c("activite_principale", "activite_secondaire")) {
    if (col_act %in% names(df_emploi)) {
      isic <- appliquer_isic(df_emploi[[col_act]], ISIC_MAPPING)
      df_emploi[[paste0(col_act, "_isic_section")]] <- isic$section
      df_emploi[[paste0(col_act, "_isic_libelle")]] <- isic$libelle
    }
  }

  # ── Biens possédés ──────────────────────────────────────────────────────────
  df_biens <- selectionner_renommer(base, ID_INDIVIDU, VARS_BIENS)

  cols_biens <- intersect(names(unlist(VARS_BIENS)), names(df_biens))
  if (length(cols_biens) > 0) {
    df_biens <- df_biens |>
      dplyr::mutate(
        dplyr::across(
          all_of(cols_biens),
          ~ dplyr::if_else(suppressWarnings(as.integer(.)) >= 1, 1L, 0L,
                           missing = NA_integer_)
        ),
        score_actifs = rowSums(dplyr::pick(all_of(cols_biens)), na.rm = FALSE)
      )
  }

  # ── Accès aux services sociaux ──────────────────────────────────────────────
  df_services <- selectionner_renommer(base, ID_INDIVIDU, VARS_SERVICES)

  if ("source_eau" %in% names(df_services))
    df_services <- dplyr::mutate(df_services, source_eau = libeller_source_eau(source_eau))

  if ("electricite_acces" %in% names(df_services))
    df_services <- dplyr::mutate(df_services,
      electricite_acces = libeller_electricite(electricite_acces))

  df_emploi |>
    dplyr::left_join(df_biens,    by = "id_individu") |>
    dplyr::left_join(df_services, by = "id_individu")
}

# ==============================================================================
# WRAPPER: construire_table_individus
# ==============================================================================
construire_table_individus <- function(base, verbose = TRUE) {

  if (verbose) message("[02_individus] Extraction démographique...")
  df_demo   <- extraire_demo_individu(base)

  if (verbose) message("[02_individus] Extraction géographique...")
  df_geo    <- extraire_geo_individu(base)

  if (verbose) message("[02_individus] Extraction profil emploi/biens/services...")
  df_profil <- extraire_profil_individu(base)

  if (verbose) message("[02_individus] Fusion...")
  table_ind <- df_demo |>
    dplyr::left_join(df_geo,    by = "id_individu") |>
    dplyr::left_join(df_profil, by = "id_individu") |>
    dplyr::mutate(
      round_afrobarometer = ROUND$numero,
      annee_enquete       = ROUND$annee,
      pays                = ROUND$pays
    )

  if (verbose) {
    message(sprintf("[02_individus] Table individus : %d x %d",
                    nrow(table_ind), ncol(table_ind)))
  }

  table_ind
}
