# ==============================================================================
# 02_INDIVIDUS.R — Construction de la table individus consolidée
# ==============================================================================

library(dplyr)
library(here)

source(here("pipeline", "R", "config.R"))

# Helper : sélectionne et renomme les variables d'un mapping
# mapping = liste nommée (nom_cible = "NOM_COL_BASE")
selectionner_renommer <- function(base, id_col, mapping) {
  dispo <- mapping[unlist(mapping) %in% names(base)]
  if (length(dispo) == 0) {
    return(base |>
             dplyr::select(all_of(id_col)) |>
             dplyr::rename(id_individu = all_of(id_col)))
  }
  cols   <- unname(unlist(dispo))   # valeurs = noms dans la base
  cibles <- names(dispo)            # noms = noms cibles

  base |>
    dplyr::select(all_of(c(id_col, cols))) |>
    dplyr::rename(
      id_individu = all_of(id_col),
      !!!setNames(cols, cibles)
    )
}

# ==============================================================================
# FONCTION: extraire_demo_individu
# ==============================================================================
extraire_demo_individu <- function(base) {

  df <- selectionner_renommer(base, ID_INDIVIDU, VARS_DEMO)

  if ("genre" %in% names(df)) {
    df <- df |>
      dplyr::mutate(genre = dplyr::case_when(
        genre == 1 ~ "Homme",
        genre == 2 ~ "Femme",
        TRUE       ~ NA_character_
      ))
  }

  if ("niveau_etudes" %in% names(df)) {
    df <- df |>
      dplyr::mutate(niveau_etudes = dplyr::case_when(
        niveau_etudes == 0 ~ "Aucun enseignement formel",
        niveau_etudes == 1 ~ "Enseignement informel/coranique",
        niveau_etudes == 2 ~ "Primaire incomplet",
        niveau_etudes == 3 ~ "Primaire complet",
        niveau_etudes == 4 ~ "Secondaire incomplet",
        niveau_etudes == 5 ~ "Secondaire complet",
        niveau_etudes == 6 ~ "Post-secondaire incomplet",
        niveau_etudes == 7 ~ "Université complet",
        TRUE               ~ NA_character_
      ))
  }

  df
}

# ==============================================================================
# FONCTION: extraire_geo_individu
# ==============================================================================
extraire_geo_individu <- function(base) {

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
# FONCTION: extraire_profil_individu
# ==============================================================================
extraire_profil_individu <- function(base) {

  # ── Emploi ─────────────────────────────────────────────────────────────────
  df_emploi <- selectionner_renommer(base, ID_INDIVIDU, VARS_EMPLOI)

  if ("statut_emploi_principal" %in% names(df_emploi)) {
    df_emploi <- df_emploi |>
      dplyr::mutate(statut_emploi_principal = dplyr::case_when(
        statut_emploi_principal == 0 ~ "Inactif (ne cherche pas)",
        statut_emploi_principal == 1 ~ "Chômeur (cherche un emploi)",
        statut_emploi_principal == 2 ~ "Employé à temps partiel",
        statut_emploi_principal == 3 ~ "Employé à temps plein",
        TRUE                         ~ NA_character_
      ))
  }

  # Classification ISIC Rev 4
  for (col_act in c("activite_principale", "activite_secondaire")) {
    if (col_act %in% names(df_emploi)) {
      code_num  <- suppressWarnings(as.integer(as.character(df_emploi[[col_act]])))
      match_idx <- match(code_num, ISIC_MAPPING$code_afrobarometer)
      df_emploi[[paste0(col_act, "_isic_section")]] <- ISIC_MAPPING$isic_section[match_idx]
      df_emploi[[paste0(col_act, "_isic_libelle")]] <- ISIC_MAPPING$isic_libelle[match_idx]
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

  if ("source_eau" %in% names(df_services)) {
    df_services <- df_services |>
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

  if ("electricite_acces" %in% names(df_services)) {
    df_services <- df_services |>
      dplyr::mutate(electricite_acces = dplyr::case_when(
        electricite_acces == 1 ~ "Oui",
        electricite_acces == 0 ~ "Non",
        TRUE                   ~ NA_character_
      ))
  }

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
