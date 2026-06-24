# ==============================================================================
# 04_QAQC.R — Contrôle qualité et production du fichier QAQC
# Contenu : taux de NA, outliers, cohérence, estimations primaires
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(here)

source(here("pipeline", "R", "config.R"))
source(here("pipeline", "R", "utils.R"))

# ==============================================================================
# FONCTION: generer_rapport_html
# Génère le rapport QAQC en HTML via R Markdown.
# Nécessite les packages : rmarkdown, knitr, kableExtra, ggplot2
# ==============================================================================
generer_rapport_html <- function(qaqc,
                                 base        = NULL,
                                 meta        = NULL,
                                 chemin_rmd = here("pipeline", "R", "qaqc_report.Rmd"),
                                 dossier_qaqc = PATHS$qaqc,
                                 verbose = TRUE) {

  pkgs_requis <- c("rmarkdown", "knitr", "kableExtra", "ggplot2")
  pkgs_manq   <- pkgs_requis[!sapply(pkgs_requis, requireNamespace, quietly = TRUE)]

  if (length(pkgs_manq) > 0) {
    warning(
      "Rapport HTML non généré. Packages manquants : ",
      paste(pkgs_manq, collapse = ", "),
      "\nInstallez-les avec : install.packages(c(",
      paste0('"', pkgs_manq, '"', collapse=", "), "))"
    )
    return(NULL)
  }

  if (!file.exists(chemin_rmd)) {
    warning("Template Rmd introuvable : ", chemin_rmd)
    return(NULL)
  }

  if (!dir.exists(dossier_qaqc)) dir.create(dossier_qaqc, recursive = TRUE)

  suffixe  <- sprintf("R%d_%s", ROUND$numero, ROUND$annee)
  chemin_h <- file.path(dossier_qaqc, paste0("QAQC_Afrobarometer_", suffixe, ".html"))

  if (verbose) message("[04_qaqc] Génération du rapport HTML...")

  rmarkdown::render(
    input       = chemin_rmd,
    output_file = chemin_h,
    params      = list(qaqc = qaqc, base = base, meta = meta, round = ROUND$numero, annee = ROUND$annee),
    quiet       = !verbose,
    envir       = new.env(parent = globalenv())
  )

  if (verbose) message("[04_qaqc] Rapport HTML exporté : ", chemin_h)

  chemin_h
}

# ==============================================================================
# FONCTION: rapport_na_par_variable
# Calcule le taux de NA pour chaque colonne d'une table.
# ==============================================================================
rapport_na_par_variable <- function(df, nom_table = "") {

  purrr::map_dfr(names(df), function(v) {
    n_total <- nrow(df)
    n_na    <- sum(is.na(df[[v]]))
    pct_na  <- round(n_na / n_total * 100, 2)

    data.frame(
      table    = nom_table,
      variable = v,
      n_total  = n_total,
      n_na     = n_na,
      pct_na   = pct_na,
      statut   = statut_na(pct_na),
      stringsAsFactors = FALSE
    )
  })
}

# ==============================================================================
# FONCTION: verifier_coherence_ages
# Contrôle de cohérence : âges plausibles (15–120 ans pour Afrobarometer).
# ==============================================================================
verifier_coherence_ages <- function(df_individus) {

  if (!"age" %in% names(df_individus)) {
    return(data.frame(
      controle = "Age",
      statut   = "Non applicable",
      detail   = "Variable 'age' absente"
    ))
  }

  age_num <- suppressWarnings(as.numeric(as.character(df_individus$age)))
  n_invalides <- sum(age_num < 15 | age_num > 120, na.rm = TRUE)

  data.frame(
    controle = "Plage d'âge (15-120 ans)",
    statut   = ifelse(n_invalides == 0, "OK", "Anomalie"),
    detail   = sprintf("%d valeurs hors plage détectées", n_invalides),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# FONCTION: verifier_unicite_id
# Vérifie qu'il n'y a pas de doublons sur l'identifiant.
# ==============================================================================
verifier_unicite_id <- function(df, col_id = "id_individu") {

  if (!col_id %in% names(df)) {
    return(data.frame(
      controle = "Unicité ID",
      statut   = "Non applicable",
      detail   = paste("Colonne", col_id, "absente")
    ))
  }

  n_doublons <- sum(duplicated(df[[col_id]], incomparables = NA))

  data.frame(
    controle = paste("Unicité de", col_id),
    statut   = ifelse(n_doublons == 0, "OK", "Anomalie"),
    detail   = sprintf("%d doublon(s) détecté(s)", n_doublons),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# FONCTION: estimations_primaires
# Calcule les statistiques descriptives primaires après traitement :
#   - Distributions des variables clés (individus)
#   - Indicateurs de pauvreté/privation (ménages)
# ==============================================================================
estimations_primaires <- function(df_individus, df_menages) {

  resultats <- list()

  # ── Distribution par genre ────────────────────────────────────────────────
  if ("genre" %in% names(df_individus)) {
    resultats$distribution_genre <- df_individus |>
      dplyr::count(genre, name = "effectif") |>
      dplyr::mutate(pourcentage = round(effectif / sum(effectif) * 100, 1)) |>
      dplyr::arrange(dplyr::desc(effectif))
  }

  # ── Distribution par niveau d'études ────────────────────────────────────
  if ("niveau_etudes" %in% names(df_individus)) {
    resultats$distribution_education <- df_individus |>
      dplyr::count(niveau_etudes, name = "effectif") |>
      dplyr::mutate(pourcentage = round(effectif / sum(effectif) * 100, 1)) |>
      dplyr::arrange(dplyr::desc(effectif))
  }

  # ── Distribution par milieu ───────────────────────────────────────────────
  if ("milieu" %in% names(df_individus)) {
    resultats$distribution_milieu <- df_individus |>
      dplyr::count(milieu, name = "effectif") |>
      dplyr::mutate(pourcentage = round(effectif / sum(effectif) * 100, 1))
  }

  # ── Distribution par région ───────────────────────────────────────────────
  if ("region" %in% names(df_individus)) {
    resultats$distribution_region <- df_individus |>
      dplyr::count(region, name = "effectif") |>
      dplyr::mutate(pourcentage = round(effectif / sum(effectif) * 100, 1)) |>
      dplyr::arrange(dplyr::desc(effectif))
  }

  # ── Distribution par statut d'emploi ─────────────────────────────────────
  if ("statut_emploi_principal" %in% names(df_individus)) {
    resultats$distribution_emploi <- df_individus |>
      dplyr::count(statut_emploi_principal, name = "effectif") |>
      dplyr::mutate(pourcentage = round(effectif / sum(effectif) * 100, 1)) |>
      dplyr::arrange(dplyr::desc(effectif))
  }

  # ── Secteur d'emploi principal (ISIC Rev 4) ───────────────────────────────
  if ("isic_libelle_principal" %in% names(df_individus)) {
    resultats$distribution_isic <- df_individus |>
      dplyr::filter(!is.na(isic_libelle_principal)) |>
      dplyr::count(isic_section_principal, isic_libelle_principal, name = "effectif") |>
      dplyr::mutate(pourcentage = round(effectif / sum(effectif) * 100, 1)) |>
      dplyr::arrange(dplyr::desc(effectif))
  }

  # ── Score moyen d'actifs ──────────────────────────────────────────────────
  if ("score_actifs" %in% names(df_individus)) {
    resultats$score_actifs_stats <- df_individus |>
      dplyr::summarise(
        moyenne = round(mean(score_actifs, na.rm = TRUE), 2),
        mediane = median(score_actifs, na.rm = TRUE),
        ecart_type = round(sd(score_actifs, na.rm = TRUE), 2),
        min = min(score_actifs, na.rm = TRUE),
        max = max(score_actifs, na.rm = TRUE)
      )
  }

  # ── Indice de privation ménages ───────────────────────────────────────────
  if ("indice_privation" %in% names(df_menages)) {
    resultats$indice_privation_menages <- df_menages |>
      dplyr::summarise(
        moyenne    = round(mean(indice_privation, na.rm = TRUE), 2),
        mediane    = median(indice_privation, na.rm = TRUE),
        pct_zero_privation = round(mean(indice_privation == 0, na.rm = TRUE) * 100, 1)
      )

    resultats$distribution_privation <- df_menages |>
      dplyr::count(indice_privation, name = "effectif") |>
      dplyr::mutate(pourcentage = round(effectif / sum(effectif) * 100, 1)) |>
      dplyr::arrange(indice_privation)
  }

  resultats
}

# ==============================================================================
# WRAPPER: produire_qaqc
# Génère le rapport QAQC complet sous forme de liste.
# ==============================================================================
produire_qaqc <- function(df_individus, df_menages,
                          rapport_outliers = NULL,
                          verbose = TRUE) {

  if (verbose) message("[04_qaqc] Calcul des taux de NA...")
  na_individus <- rapport_na_par_variable(df_individus, "individus")
  na_menages   <- rapport_na_par_variable(df_menages,   "menages")
  rapport_na   <- dplyr::bind_rows(na_individus, na_menages)

  if (verbose) message("[04_qaqc] Contrôles de cohérence...")
  controles <- dplyr::bind_rows(
    verifier_unicite_id(df_individus, "id_individu"),
    verifier_unicite_id(df_menages,   "id_individu"),
    verifier_coherence_ages(df_individus)
  )

  if (verbose) message("[04_qaqc] Calcul des estimations primaires...")
  estimations <- estimations_primaires(df_individus, df_menages)

  # Résumé global
  n_alertes   <- sum(rapport_na$statut == "Alerte")
  n_critiques <- sum(rapport_na$statut == "Critique")

  resume_global <- data.frame(
    indicateur = c(
      "Observations individus",
      "Variables individus",
      "Observations ménages",
      "Variables ménages",
      "Variables NA Alerte (>20%)",
      "Variables NA Critique (>50%)",
      "Contrôles OK",
      "Contrôles avec anomalies",
      "Round Afrobarometer",
      "Année enquête"
    ),
    valeur = c(
      nrow(df_individus),
      ncol(df_individus),
      nrow(df_menages),
      ncol(df_menages),
      n_alertes,
      n_critiques,
      sum(controles$statut == "OK"),
      sum(controles$statut == "Anomalie"),
      ROUND$numero,
      ROUND$annee
    ),
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message(sprintf("[04_qaqc] %d alerte(s) NA, %d critique(s) NA, %d anomalie(s) de cohérence.",
                    n_alertes, n_critiques,
                    sum(controles$statut == "Anomalie")))
  }

  list(
    resume_global    = resume_global,
    rapport_na       = rapport_na,
    controles        = controles,
    outliers         = rapport_outliers,
    estimations      = estimations,
    horodatage_qaqc  = Sys.time()
  )
}
