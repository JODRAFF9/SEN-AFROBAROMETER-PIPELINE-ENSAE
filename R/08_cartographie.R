# ==============================================================================
# 08_CARTOGRAPHIE.R - Cartographie thematique des indicateurs par region
# Produit des cartes choropleths du Senegal a partir des donnees traitees.
# Packages requis : sf, ggplot2, geodata (ou rnaturalearth), dplyr, patchwork
# ==============================================================================

library(dplyr)
library(ggplot2)
library(here)

source(here("R", "config.R"))

# ==============================================================================
# CORRESPONDANCE codes Afrobarometer -> noms regions officiels
# Les noms doivent correspondre exactement au shapefile GADM Senegal niveau 1
# ==============================================================================
REGIONS_SEN <- data.frame(
  code_region  = as.character(660:673),
  nom_region   = c(
    "Dakar", "Diourbel", "Fatick", "Kaffrine", "Kaolack",
    "Kedougou", "Kolda", "Louga", "Matam", "Saint-Louis",
    "Sedhiou", "Tambacounda", "Thies", "Ziguinchor"
  ),
  # Noms tels qu'ils apparaissent dans GADM (variantes orthographiques)
  nom_gadm     = c(
    "Dakar", "Diourbel", "Fatick", "Kaffrine", "Kaolack",
    "Kedougou", "Kolda", "Louga", "Matam", "Saint-Louis",
    "Sedhiou", "Tambacounda", "Thies", "Ziguinchor"
  ),
  stringsAsFactors = FALSE
)

# ==============================================================================
# FONCTION: charger_shapefile_senegal
# Telecharge et met en cache le shapefile GADM niveau 1 du Senegal.
# Necessite le package geodata (ou rnaturalearth en fallback).
# ==============================================================================
charger_shapefile_senegal <- function(dossier_cache = here("input", "geo"),
                                       verbose = TRUE) {

  if (!dir.exists(dossier_cache)) dir.create(dossier_cache, recursive = TRUE)

  # Tentative 1 : geodata (GADM)
  if (requireNamespace("geodata", quietly = TRUE) &&
      requireNamespace("sf",      quietly = TRUE)) {

    if (verbose) message("[08_carto] Chargement du shapefile GADM via geodata...")
    shp <- tryCatch({
      geodata::gadm(country = "SEN", level = 1, path = dossier_cache) |>
        sf::st_as_sf()
    }, error = function(e) NULL)

    if (!is.null(shp)) return(shp)
  }

  # Tentative 2 : rnaturalearth
  if (requireNamespace("rnaturalearth",  quietly = TRUE) &&
      requireNamespace("rnaturalearthdata", quietly = TRUE) &&
      requireNamespace("sf", quietly = TRUE)) {

    if (verbose) message("[08_carto] Chargement via rnaturalearth...")
    shp <- tryCatch({
      rnaturalearth::ne_states(country = "Senegal", returnclass = "sf")
    }, error = function(e) NULL)

    if (!is.null(shp)) return(shp)
  }

  stop(
    "Shapefile indisponible. Installez l'un des packages suivants :\n",
    "  install.packages(c('geodata', 'sf'))  # recommande\n",
    "  install.packages(c('rnaturalearth', 'rnaturalearthdata', 'sf'))"
  )
}

# ==============================================================================
# FONCTION: preparer_donnees_carto
# Agregge les indicateurs par region et joint avec le shapefile.
# ==============================================================================
preparer_donnees_carto <- function(df_individus, df_menages, shp, verbose = TRUE) {

  if (verbose) message("[08_carto] Agregation des indicateurs par region...")

  # Agregation individus par region
  ind_reg <- df_individus |>
    dplyr::mutate(code_region = as.character(region)) |>
    dplyr::left_join(REGIONS_SEN, by = "code_region") |>
    dplyr::filter(!is.na(nom_region)) |>
    dplyr::group_by(nom_region) |>
    dplyr::summarise(
      n_individus        = dplyr::n(),
      pct_urbain         = round(mean(
        as.numeric(as.character(milieu)) == 2, na.rm = TRUE) * 100, 1),
      score_actifs_moy   = round(mean(score_actifs, na.rm = TRUE), 2),
      indice_bien_etre_moy = if ("indice_bien_etre" %in% names(dplyr::cur_data()))
                               round(mean(indice_bien_etre, na.rm = TRUE), 1)
                             else NA_real_,
      pct_tres_vuln      = if ("segment_vulnerabilite" %in% names(dplyr::cur_data()))
                             round(mean(segment_vulnerabilite == "Tres vulnerable",
                                        na.rm = TRUE) * 100, 1)
                           else NA_real_,
      pct_sans_emploi    = round(mean(
        as.character(statut_emploi_principal) %in%
          c("0", "1", "Sans emploi (ne cherche pas)", "Sans emploi (cherche)"),
        na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )

  # Agregation menages par region
  men_reg <- df_menages |>
    dplyr::mutate(code_region = as.character(region)) |>
    dplyr::left_join(REGIONS_SEN, by = "code_region") |>
    dplyr::filter(!is.na(nom_region)) |>
    dplyr::group_by(nom_region) |>
    dplyr::summarise(
      privation_moy      = round(mean(indice_privation, na.rm = TRUE), 2),
      pct_privation_sev  = round(mean(indice_privation >= 3, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )

  data_reg <- dplyr::left_join(ind_reg, men_reg, by = "nom_region")

  # Colonne de jointure dans le shapefile
  col_nom_shp <- dplyr::case_when(
    "NAME_1"  %in% names(shp) ~ "NAME_1",
    "name"    %in% names(shp) ~ "name",
    "name_en" %in% names(shp) ~ "name_en",
    TRUE ~ names(shp)[1]
  )

  shp_join <- shp |>
    dplyr::rename(nom_gadm = dplyr::all_of(col_nom_shp)) |>
    dplyr::left_join(
      REGIONS_SEN |> dplyr::select(nom_region, nom_gadm),
      by = "nom_gadm"
    ) |>
    dplyr::left_join(data_reg, by = "nom_region")

  if (verbose) {
    n_matchees <- sum(!is.na(shp_join$n_individus))
    message(sprintf("[08_carto] %d / %d regions matchees avec les donnees.", n_matchees, nrow(shp)))
  }

  shp_join
}

# ==============================================================================
# THEME cartographique
# ==============================================================================
theme_carte <- function() {
  ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 13,
                                             colour = "#1A3A5C", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "#546E7A",
                                             hjust = 0.5, margin = ggplot2::margin(b = 8)),
      plot.caption  = ggplot2::element_text(size = 7.5, colour = "#90A4AE",
                                             hjust = 1, margin = ggplot2::margin(t = 6)),
      legend.position   = "right",
      legend.title      = ggplot2::element_text(face = "bold", size = 8.5),
      legend.text       = ggplot2::element_text(size = 8),
      legend.key.height = ggplot2::unit(0.9, "cm"),
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin       = ggplot2::margin(10, 10, 10, 10)
    )
}

SOURCE_CAPTION <- "Source : Afrobarometer Round 9 Senegal (2022) - Pipeline ENSAE Dakar"

# ==============================================================================
# FONCTION: carte_choroplethe
# Generateur generique de carte choroplethe.
# ==============================================================================
carte_choroplethe <- function(shp_data, variable, titre, sous_titre = "",
                               palette = "YlOrRd", direction = 1,
                               fmt_legende = function(x) round(x, 1),
                               label_legende = "") {

  ggplot2::ggplot(shp_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variable]]),
                     colour = "white", linewidth = 0.4) +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = nom_region),
      size = 2.2, colour = "#1A3A5C", fontface = "bold",
      check_overlap = TRUE
    ) +
    ggplot2::scale_fill_distiller(
      palette   = palette,
      direction = direction,
      name      = label_legende,
      labels    = fmt_legende,
      na.value  = "#EEEEEE"
    ) +
    ggplot2::labs(
      title    = titre,
      subtitle = sous_titre,
      caption  = SOURCE_CAPTION
    ) +
    theme_carte()
}

# ==============================================================================
# FONCTION: produire_cartes
# Produit l'ensemble des cartes et les sauvegarde en PNG.
# ==============================================================================
produire_cartes <- function(shp_data, dossier_sortie, verbose = TRUE) {

  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis pour la cartographie.")

  dir.create(dossier_sortie, recursive = TRUE, showWarnings = FALSE)

  cartes <- list()

  # ── Carte 1 : Indice de bien-etre ─────────────────────────────────────────
  if ("indice_bien_etre_moy" %in% names(shp_data) &&
      !all(is.na(shp_data$indice_bien_etre_moy))) {

    cartes$bien_etre <- carte_choroplethe(
      shp_data,
      variable      = "indice_bien_etre_moy",
      titre         = "Indice de bien-etre moyen par region",
      sous_titre    = "Score composite 0-100 (actifs + services + privation)",
      palette       = "YlGn",
      direction     = 1,
      label_legende = "Score /100"
    )
  }

  # ── Carte 2 : Indice de privation ─────────────────────────────────────────
  if ("privation_moy" %in% names(shp_data)) {

    cartes$privation <- carte_choroplethe(
      shp_data,
      variable      = "privation_moy",
      titre         = "Indice de privation moyen des menages",
      sous_titre    = "Score composite 0-5 (alimentation, eau, soins, combustible, revenus)",
      palette       = "YlOrRd",
      direction     = 1,
      label_legende = "Score /5"
    )
  }

  # ── Carte 3 : Taux de vulnerabilite severe ────────────────────────────────
  if ("pct_tres_vuln" %in% names(shp_data) &&
      !all(is.na(shp_data$pct_tres_vuln))) {

    cartes$vulnerabilite <- carte_choroplethe(
      shp_data,
      variable      = "pct_tres_vuln",
      titre         = "Population tres vulnerable par region (%)",
      sous_titre    = "Cumul : privation elevee + emploi precaire + faibles actifs",
      palette       = "Reds",
      direction     = 1,
      fmt_legende   = function(x) paste0(x, "%"),
      label_legende = "% pop."
    )
  }

  # ── Carte 4 : Score d'actifs ──────────────────────────────────────────────
  if ("score_actifs_moy" %in% names(shp_data)) {

    cartes$actifs <- carte_choroplethe(
      shp_data,
      variable      = "score_actifs_moy",
      titre         = "Score d'actifs moyen par region",
      sous_titre    = "Biens possedes : radio, TV, vehicule, ordinateur, telephone, internet",
      palette       = "Blues",
      direction     = 1,
      label_legende = "Score"
    )
  }

  # ── Carte 5 : Taux d'urbanisation ─────────────────────────────────────────
  if ("pct_urbain" %in% names(shp_data)) {

    cartes$urbanisation <- carte_choroplethe(
      shp_data,
      variable      = "pct_urbain",
      titre         = "Taux d'urbanisation par region (%)",
      sous_titre    = "Part des repondants en milieu urbain",
      palette       = "PuBu",
      direction     = 1,
      fmt_legende   = function(x) paste0(x, "%"),
      label_legende = "% urbain"
    )
  }

  # ── Carte 6 : Taux de chomage / precarite emploi ─────────────────────────
  if ("pct_sans_emploi" %in% names(shp_data)) {

    cartes$emploi <- carte_choroplethe(
      shp_data,
      variable      = "pct_sans_emploi",
      titre         = "Precarite de l'emploi par region (%)",
      sous_titre    = "Part des repondants sans emploi stable (cherchant ou non)",
      palette       = "OrRd",
      direction     = 1,
      fmt_legende   = function(x) paste0(x, "%"),
      label_legende = "% sans emploi\nstable"
    )
  }

  # ── Panneau multi-cartes (si patchwork disponible) ────────────────────────
  if (requireNamespace("patchwork", quietly = TRUE) && length(cartes) >= 2) {
    cartes_list <- unname(cartes)
    n <- length(cartes_list)
    ncol_panel <- if (n <= 2) 2 else if (n <= 4) 2 else 3

    panneau <- patchwork::wrap_plots(cartes_list, ncol = ncol_panel) +
      patchwork::plot_annotation(
        title   = "Tableau de bord cartographique - Afrobarometer Senegal R9 (2022)",
        caption = SOURCE_CAPTION,
        theme   = ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 15,
                                              colour = "#1A3A5C", hjust = 0.5)
        )
      )
    cartes$panneau <- panneau
  }

  # ── Sauvegarde ────────────────────────────────────────────────────────────
  chemins <- list()
  noms_fichiers <- list(
    bien_etre    = "carte_bien_etre.png",
    privation    = "carte_privation.png",
    vulnerabilite = "carte_vulnerabilite.png",
    actifs       = "carte_actifs.png",
    urbanisation = "carte_urbanisation.png",
    emploi       = "carte_emploi.png",
    panneau      = "carte_panneau_complet.png"
  )

  for (nom in names(cartes)) {
    chemin <- file.path(dossier_sortie, noms_fichiers[[nom]])
    largeur <- if (nom == "panneau") 18 else 9
    hauteur <- if (nom == "panneau") 14 else 8

    ggplot2::ggsave(chemin, plot = cartes[[nom]],
                    width = largeur, height = hauteur,
                    dpi = 180, bg = "white")

    chemins[[nom]] <- chemin
    if (verbose) message(sprintf("[08_carto] Carte exportee : %s", chemin))
  }

  list(cartes = cartes, chemins = chemins)
}

# ==============================================================================
# WRAPPER: lancer_cartographie
# ==============================================================================
lancer_cartographie <- function(df_individus, df_menages,
                                 dossier_sortie = file.path(PATHS$output, "cartes"),
                                 verbose = TRUE) {

  pkgs <- c("sf", "geodata")
  pkgs_ok <- sapply(pkgs, requireNamespace, quietly = TRUE)

  if (!pkgs_ok["sf"]) {
    warning(
      "[08_carto] Package 'sf' manquant - cartographie ignoree.\n",
      "  Installez avec : install.packages(c('sf', 'geodata'))"
    )
    return(invisible(NULL))
  }

  if (verbose) message("[08_carto] Chargement du fond de carte Senegal...")
  shp <- charger_shapefile_senegal(verbose = verbose)

  if (verbose) message("[08_carto] Preparation des donnees spatiales...")
  shp_data <- preparer_donnees_carto(df_individus, df_menages, shp, verbose)

  if (verbose) message("[08_carto] Generation des cartes...")
  res <- produire_cartes(shp_data, dossier_sortie, verbose)

  if (verbose) message(sprintf("[08_carto] %d carte(s) generee(s) dans : %s",
                                length(res$chemins), dossier_sortie))

  invisible(list(
    shp_data = shp_data,
    cartes   = res$cartes,
    chemins  = res$chemins
  ))
}
