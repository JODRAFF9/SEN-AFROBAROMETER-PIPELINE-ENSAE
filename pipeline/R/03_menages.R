# ==============================================================================
# 03_MENAGES.R — Construction de la table ménages consolidée
# ==============================================================================

library(dplyr)
library(here)

source(here("pipeline", "R", "config.R"))

# Helper partagé (dupliqué ici pour éviter la dépendance à 02_individus.R)
selectionner_renommer <- function(base, id_col, mapping) {
  dispo <- mapping[unlist(mapping) %in% names(base)]
  if (length(dispo) == 0) {
    return(base |>
             dplyr::select(all_of(id_col)) |>
             dplyr::rename(id_individu = all_of(id_col)))
  }
  cols   <- unname(unlist(dispo))
  cibles <- names(dispo)

  base |>
    dplyr::select(all_of(c(id_col, cols))) |>
    dplyr::rename(
      id_individu = all_of(id_col),
      !!!setNames(cols, cibles)
    )
}

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

  if ("rep_genre" %in% names(df)) {
    df <- df |>
      dplyr::mutate(rep_genre = dplyr::case_when(
        rep_genre == 1 ~ "Homme",
        rep_genre == 2 ~ "Femme",
        TRUE           ~ NA_character_
      ))
  }

  if ("rep_niveau_etudes" %in% names(df)) {
    df <- df |>
      dplyr::mutate(rep_niveau_etudes = dplyr::case_when(
        rep_niveau_etudes == 0 ~ "Aucun enseignement formel",
        rep_niveau_etudes == 1 ~ "Enseignement informel/coranique",
        rep_niveau_etudes == 2 ~ "Primaire incomplet",
        rep_niveau_etudes == 3 ~ "Primaire complet",
        rep_niveau_etudes == 4 ~ "Secondaire incomplet",
        rep_niveau_etudes == 5 ~ "Secondaire complet",
        rep_niveau_etudes == 6 ~ "Post-secondaire incomplet",
        rep_niveau_etudes == 7 ~ "Université complet",
        TRUE                   ~ NA_character_
      ))
  }

  df
}

# ==============================================================================
# FONCTION: extraire_geo_menage
# ==============================================================================
extraire_geo_menage <- function(base) {

  df <- selectionner_renommer(base, ID_INDIVIDU, VARS_GEO)

  if ("region" %in% names(df)) {
    df <- df |>
      dplyr::mutate(region = dplyr::case_when(
        region == 660 ~ "Dakar",        region == 661 ~ "Diourbel",
        region == 662 ~ "Fatick",       region == 663 ~ "Kaffrine",
        region == 664 ~ "Kaolack",      region == 665 ~ "Kédougou",
        region == 666 ~ "Kolda",        region == 667 ~ "Louga",
        region == 668 ~ "Matam",        region == 669 ~ "Saint-Louis",
        region == 670 ~ "Sédhiou",      region == 671 ~ "Tambacounda",
        region == 672 ~ "Thiès",        region == 673 ~ "Ziguinchor",
        TRUE          ~ as.character(region)
      ))
  }

  if ("milieu" %in% names(df)) {
    df <- df |>
      dplyr::mutate(milieu = dplyr::case_when(
        milieu == 1 ~ "Rural",
        milieu == 2 ~ "Urbain",
        TRUE        ~ NA_character_
      ))
  }

  df
}

# ==============================================================================
# FONCTION: extraire_services_zone_menage
# ==============================================================================
extraire_services_zone_menage <- function(base) {

  df <- selectionner_renommer(base, ID_INDIVIDU, VARS_SERVICES)

  if ("source_eau" %in% names(df)) {
    df <- df |>
      dplyr::mutate(source_eau = dplyr::case_when(
        source_eau == 1 ~ "Robinet dans la maison",
        source_eau == 2 ~ "Robinet dans la cour/concession",
        source_eau == 3 ~ "Fontaine publique",
        source_eau == 4 ~ "Puits protégé",
        source_eau == 5 ~ "Puits non protégé",
        source_eau == 6 ~ "Source/rivière",
        source_eau == 7 ~ "Eau de pluie",
        source_eau == 8 ~ "Eau en bouteille",
        TRUE            ~ NA_character_
      ))
  }

  if ("electricite_acces" %in% names(df)) {
    df <- df |>
      dplyr::mutate(electricite_acces = dplyr::case_when(
        electricite_acces == 1 ~ "Oui",
        electricite_acces == 0 ~ "Non",
        TRUE                   ~ NA_character_
      ))
  }

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
