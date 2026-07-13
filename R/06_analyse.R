# ==============================================================================
# 06_ANALYSE.R — Analyses thematiques avancees
# Produit des indicateurs composites, tableaux croises ponderes et
# segmentations multi-dimensionnelles a partir des tables traitees.
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(here)

source(here("R", "config.R"))
source(here("R", "utils.R"))

# ==============================================================================
# FONCTION: indice_bien_etre
# Construit un indice composite de bien-etre (0-100) en combinant :
#   - Score d'actifs (biens possedes)
#   - Acces aux services (eau, electricite, assainissement)
#   - Absence de privation (conditions de vie)
# Normalisation min-max par composante puis agregation ponderee.
# ==============================================================================
indice_bien_etre <- function(df_individus, poids = c(actifs = 0.4, services = 0.3, privation = 0.3)) {

  normaliser <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(rep(0, length(x)))
    (x - rng[1]) / diff(rng)
  }

  df <- df_individus

  # Composante 1 : actifs
  if ("score_actifs" %in% names(df)) {
    df$comp_actifs <- normaliser(df$score_actifs) * 100
  } else {
    df$comp_actifs <- NA_real_
  }

  # Composante 2 : acces aux services
  cols_serv <- intersect(c("electricite_acces", "source_eau", "assainissement"), names(df))
  if (length(cols_serv) > 0) {
    serv_num <- df |>
      dplyr::select(dplyr::all_of(cols_serv)) |>
      dplyr::mutate(dplyr::across(everything(), ~ as.numeric(as.character(.))))
    df$comp_services <- normaliser(rowMeans(serv_num, na.rm = TRUE)) * 100
  } else {
    df$comp_services <- NA_real_
  }

  # Composante 3 : absence de privation (score inverse de l'indice de privation)
  # L'indice de privation vient des menages : il est joint si disponible
  if ("indice_privation" %in% names(df)) {
    max_priv <- max(df$indice_privation, na.rm = TRUE)
    df$comp_privation <- (1 - df$indice_privation / max_priv) * 100
  } else {
    df$comp_privation <- NA_real_
  }

  # Agregation ponderee (ignore les composantes manquantes)
  df <- df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      indice_bien_etre = {
        vals <- c(comp_actifs, comp_services, comp_privation)
        w    <- unname(poids)
        ok   <- !is.na(vals)
        if (!any(ok)) NA_real_
        else round(sum(vals[ok] * w[ok]) / sum(w[ok]), 1)
      }
    ) |>
    dplyr::ungroup()

  df
}

# ==============================================================================
# FONCTION: tableau_croise_pondere
# Produit un tableau croise ponderede entre deux variables categorielle.
# Retourne les proportions en ligne avec effectifs bruts.
# ==============================================================================
tableau_croise_pondere <- function(df, var_ligne, var_col, poids_col = "poids") {

  df |>
    dplyr::filter(!is.na(.data[[var_ligne]]), !is.na(.data[[var_col]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(var_ligne, var_col)))) |>
    dplyr::summarise(
      poids_sum = sum(.data[[poids_col]], na.rm = TRUE),
      n         = dplyr::n(),
      .groups   = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(var_ligne))) |>
    dplyr::mutate(
      pct_ligne = round(poids_sum / sum(poids_sum) * 100, 1)
    ) |>
    dplyr::ungroup()
}

# ==============================================================================
# FONCTION: segmentation_vulnerabilite
# Classe chaque individu dans un segment de vulnerabilite selon 3 criteres :
#   - Privation elevee (indice_privation >= 3)
#   - Sans emploi stable (statut_emploi_principal in 0/1)
#   - Acces limite aux services (score services < median)
# ==============================================================================
segmentation_vulnerabilite <- function(df_individus) {

  df <- df_individus

  # Critere 1 : privation
  if ("indice_privation" %in% names(df)) {
    df$vuln_privation <- as.integer(!is.na(df$indice_privation) & df$indice_privation >= 3)
  } else {
    df$vuln_privation <- NA_integer_
  }

  # Critere 2 : emploi precaire
  if ("statut_emploi_principal" %in% names(df)) {
    niv_precaire <- c("0", "1", "Sans emploi (ne cherche pas)", "Sans emploi (cherche)")
    df$vuln_emploi <- as.integer(as.character(df$statut_emploi_principal) %in% niv_precaire)
  } else {
    df$vuln_emploi <- NA_integer_
  }

  # Critere 3 : acces limite aux services
  if ("score_actifs" %in% names(df)) {
    seuil <- median(df$score_actifs, na.rm = TRUE)
    df$vuln_actifs <- as.integer(!is.na(df$score_actifs) & df$score_actifs < seuil)
  } else {
    df$vuln_actifs <- NA_integer_
  }

  # Score de vulnerabilite composite (0-3)
  df <- df |>
    dplyr::mutate(
      score_vulnerabilite = rowSums(
        dplyr::pick(dplyr::starts_with("vuln_")), na.rm = FALSE
      ),
      segment_vulnerabilite = dplyr::case_when(
        is.na(score_vulnerabilite)   ~ NA_character_,
        score_vulnerabilite == 0     ~ "Resilient",
        score_vulnerabilite == 1     ~ "Vulnerable modere",
        score_vulnerabilite == 2     ~ "Vulnerable",
        score_vulnerabilite >= 3     ~ "Tres vulnerable"
      )
    )

  df
}

# ==============================================================================
# FONCTION: profil_regional
# Synthetise les indicateurs cles par region pour cartographie ou comparaison.
# ==============================================================================
profil_regional <- function(df_individus, df_menages) {

  ind_region <- df_individus |>
    dplyr::filter(!is.na(region)) |>
    dplyr::group_by(region) |>
    dplyr::summarise(
      n_individus          = dplyr::n(),
      pct_urbain           = if ("milieu" %in% names(dplyr::cur_data()))
                               round(mean(as.numeric(as.character(milieu)) == 2, na.rm = TRUE) * 100, 1)
                             else NA_real_,
      score_actifs_moyen   = if ("score_actifs" %in% names(dplyr::cur_data()))
                               round(mean(score_actifs, na.rm = TRUE), 2)
                             else NA_real_,
      pct_sans_emploi      = if ("statut_emploi_principal" %in% names(dplyr::cur_data()))
                               round(mean(as.character(statut_emploi_principal) %in%
                                 c("0","1","Sans emploi (ne cherche pas)","Sans emploi (cherche)"),
                                 na.rm = TRUE) * 100, 1)
                             else NA_real_,
      .groups = "drop"
    )

  men_region <- df_menages |>
    dplyr::filter(!is.na(region)) |>
    dplyr::group_by(region) |>
    dplyr::summarise(
      privation_moyenne    = if ("indice_privation" %in% names(dplyr::cur_data()))
                               round(mean(indice_privation, na.rm = TRUE), 2)
                             else NA_real_,
      pct_severe_privation = if ("indice_privation" %in% names(dplyr::cur_data()))
                               round(mean(indice_privation >= 3, na.rm = TRUE) * 100, 1)
                             else NA_real_,
      .groups = "drop"
    )

  dplyr::left_join(ind_region, men_region, by = "region") |>
    dplyr::arrange(dplyr::desc(privation_moyenne))
}

# ==============================================================================
# WRAPPER: lancer_analyse
# ==============================================================================
lancer_analyse <- function(df_individus, df_menages, verbose = TRUE) {

  if (verbose) message("[06_analyse] Calcul de l'indice de bien-etre...")
  df_individus <- indice_bien_etre(df_individus)

  if (verbose) message("[06_analyse] Segmentation par vulnerabilite...")
  df_individus <- segmentation_vulnerabilite(df_individus)

  if (verbose) message("[06_analyse] Tableaux croises ponderes...")
  tableaux <- list()

  if (all(c("genre", "niveau_etudes", "poids") %in% names(df_individus)))
    tableaux$genre_x_education <- tableau_croise_pondere(df_individus, "genre", "niveau_etudes")

  if (all(c("milieu", "statut_emploi_principal", "poids") %in% names(df_individus)))
    tableaux$milieu_x_emploi <- tableau_croise_pondere(df_individus, "milieu", "statut_emploi_principal")

  if (all(c("region", "segment_vulnerabilite", "poids") %in% names(df_individus)))
    tableaux$region_x_vulnerabilite <- tableau_croise_pondere(df_individus, "region", "segment_vulnerabilite")

  if (verbose) message("[06_analyse] Profil regional...")
  profil_reg <- profil_regional(df_individus, df_menages)

  # Distribution du segment de vulnerabilite
  distrib_vuln <- if ("segment_vulnerabilite" %in% names(df_individus)) {
    df_individus |>
      dplyr::count(segment_vulnerabilite, name = "effectif") |>
      dplyr::mutate(pct = round(effectif / sum(effectif) * 100, 1)) |>
      dplyr::arrange(dplyr::desc(effectif))
  } else NULL

  if (verbose) {
    message(sprintf("[06_analyse] Indice de bien-etre moyen : %.1f / 100",
                    mean(df_individus$indice_bien_etre, na.rm = TRUE)))
    if (!is.null(distrib_vuln))
      message(sprintf("[06_analyse] Tres vulnerables : %.1f %%",
                      distrib_vuln$pct[distrib_vuln$segment_vulnerabilite == "Tres vulnerable"][1]))
  }

  list(
    individus        = df_individus,
    tableaux_croises = tableaux,
    profil_regional  = profil_reg,
    distrib_vulnerabilite = distrib_vuln
  )
}
