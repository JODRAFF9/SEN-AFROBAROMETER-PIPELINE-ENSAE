# ==============================================================================
# 05_EXPORT.R — Export des tables consolidées et du rapport QAQC
# Formats : CSV (portable) + Excel (rapport QAQC)
# ==============================================================================

library(dplyr)
library(here)

source(here("pipeline", "R", "config.R"))

# Chargement conditionnel d'openxlsx pour le rapport QAQC
charger_openxlsx <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("Package 'openxlsx' non disponible. Le rapport QAQC sera exporté en CSV uniquement.")
    return(FALSE)
  }
  TRUE
}

# ==============================================================================
# FONCTION: creer_dossiers_output
# Crée les dossiers de sortie s'ils n'existent pas.
# ==============================================================================
creer_dossiers_output <- function() {
  dirs <- c(PATHS$output, PATHS$qaqc)
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE)
      message("[05_export] Dossier créé : ", d)
    }
  }
}

# ==============================================================================
# FONCTION: exporter_tables_consolidees
# Exporte les deux tables (individus, ménages) en CSV.
# ==============================================================================
exporter_tables_consolidees <- function(df_individus, df_menages, verbose = TRUE) {

  creer_dossiers_output()

  suffixe <- sprintf("R%d_%s", ROUND$numero, ROUND$annee)

  chemin_ind <- file.path(PATHS$output, paste0("table_individus_", suffixe, ".csv"))
  chemin_men <- file.path(PATHS$output, paste0("table_menages_",   suffixe, ".csv"))

  write.csv(df_individus, chemin_ind, row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(df_menages,   chemin_men, row.names = FALSE, fileEncoding = "UTF-8")

  if (verbose) {
    message("[05_export] Table individus exportée : ", chemin_ind)
    message("[05_export] Table ménages exportée   : ", chemin_men)
  }

  list(individus = chemin_ind, menages = chemin_men)
}

# ==============================================================================
# FONCTION: exporter_qaqc_excel
# Exporte le rapport QAQC dans un classeur Excel multi-onglets.
# Onglets : Résumé | NA_rapport | Contrôles | Outliers | Estimations_*
# ==============================================================================
exporter_qaqc_excel <- function(qaqc, verbose = TRUE) {

  creer_dossiers_output()

  suffixe   <- sprintf("R%d_%s", ROUND$numero, ROUND$annee)
  chemin_xl <- file.path(PATHS$qaqc, paste0("QAQC_Afrobarometer_", suffixe, ".xlsx"))
  chemin_cs <- file.path(PATHS$qaqc, paste0("QAQC_resume_",        suffixe, ".csv"))

  # Export CSV de secours (toujours)
  write.csv(qaqc$rapport_na, chemin_cs, row.names = FALSE, fileEncoding = "UTF-8")

  # Export Excel enrichi (si openxlsx disponible)
  if (charger_openxlsx()) {
    library(openxlsx)

    styles <- list(
      ok       = createStyle(fontColour = "#1A6E1A", bgFill = "#C6EFCE"),
      alerte   = createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C"),
      critique = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"),
      entete   = createStyle(textDecoration = "bold", border = "Bottom")
    )

    wb <- createWorkbook()
    addWorksheet(wb, "Résumé_global")
    addWorksheet(wb, "NA_rapport")
    addWorksheet(wb, "Contrôles")
    addWorksheet(wb, "Outliers")

    # Onglet Résumé
    writeData(wb, "Résumé_global", qaqc$resume_global)
    addStyle(wb, "Résumé_global", styles$entete, rows = 1, cols = 1:2)

    # Onglet NA
    writeData(wb, "NA_rapport", qaqc$rapport_na)
    addStyle(wb, "NA_rapport", styles$entete, rows = 1, cols = 1:ncol(qaqc$rapport_na))

    # Mise en couleur conditionnelle des statuts NA
    if (nrow(qaqc$rapport_na) > 0) {
      for (i in seq_len(nrow(qaqc$rapport_na))) {
        st <- qaqc$rapport_na$statut[i]
        style_row <- switch(st,
          OK       = styles$ok,
          Faible   = styles$ok,
          Alerte   = styles$alerte,
          Critique = styles$critique,
          NULL
        )
        if (!is.null(style_row)) {
          addStyle(wb, "NA_rapport", style_row, rows = i + 1,
                   cols = 1:ncol(qaqc$rapport_na), stack = TRUE)
        }
      }
    }

    # Onglet Contrôles
    writeData(wb, "Contrôles", qaqc$controles)
    addStyle(wb, "Contrôles", styles$entete, rows = 1, cols = 1:ncol(qaqc$controles))

    # Onglet Outliers
    if (!is.null(qaqc$outliers) && nrow(qaqc$outliers) > 0) {
      writeData(wb, "Outliers", qaqc$outliers)
      addStyle(wb, "Outliers", styles$entete, rows = 1, cols = 1:ncol(qaqc$outliers))
    } else {
      writeData(wb, "Outliers", data.frame(message = "Aucun outlier détecté"))
    }

    # Onglets estimations
    for (nom_est in names(qaqc$estimations)) {
      est_df <- qaqc$estimations[[nom_est]]
      if (is.data.frame(est_df) && nrow(est_df) > 0) {
        onglet <- substr(nom_est, 1, 31)   # Excel limite les noms à 31 caractères
        addWorksheet(wb, onglet)
        writeData(wb, onglet, est_df)
        addStyle(wb, onglet, styles$entete, rows = 1, cols = 1:ncol(est_df))
      }
    }

    saveWorkbook(wb, chemin_xl, overwrite = TRUE)

    if (verbose) {
      message("[05_export] Rapport QAQC Excel exporté : ", chemin_xl)
    }
  }

  if (verbose) message("[05_export] Rapport QAQC CSV exporté : ", chemin_cs)

  list(excel = chemin_xl, csv = chemin_cs)
}

# ==============================================================================
# WRAPPER: lancer_export
# Exporte toutes les sorties du pipeline.
# ==============================================================================
lancer_export <- function(df_individus, df_menages, qaqc, verbose = TRUE) {

  if (verbose) message("[05_export] Export des tables consolidées...")
  chemins_tables <- exporter_tables_consolidees(df_individus, df_menages, verbose)

  if (verbose) message("[05_export] Export du rapport QAQC (Excel + HTML)...")
  chemins_qaqc   <- exporter_qaqc_excel(qaqc, verbose)

  # Rapport HTML (nécessite rmarkdown)
  chemin_html <- generer_rapport_html(qaqc, verbose = verbose)
  chemins_qaqc$html <- chemin_html

  if (verbose) {
    message("\n[05_export] === EXPORT TERMINÉ ===")
    message("  Individus : ", chemins_tables$individus)
    message("  Ménages   : ", chemins_tables$menages)
    message("  QAQC      : ", chemins_qaqc$excel %||% chemins_qaqc$csv)
  }

  list(tables = chemins_tables, qaqc = chemins_qaqc)
}


`%||%` <- function(a, b) if (!is.null(a)) a else b
