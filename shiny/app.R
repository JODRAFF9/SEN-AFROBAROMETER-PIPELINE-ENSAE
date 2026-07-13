# ==============================================================================
# SHINY APP - Plateforme de visualisation Afrobarometer Senegal Round 9 (2022)
# Lancer : shiny::runApp("shiny/")
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(haven)
library(here)
library(purrr)

# ------------------------------------------------------------------------------
# Palette professionnelle
# ------------------------------------------------------------------------------
PAL <- list(
  vert    = "#009A44",
  rouge   = "#CE1126",
  jaune   = "#FCD116",
  bleu    = "#1B3A6B",
  gris    = "#4A5568",
  clair   = "#F7F8FA",
  bordure = "#D1D5DB"
)

PAL_BARS <- c("#1B3A6B", "#009A44", "#CE1126", "#FCD116", "#4A5568",
              "#2E7D32", "#7B1FA2", "#E65100", "#0277BD", "#558B2F")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ------------------------------------------------------------------------------
# Chargement et preparation des donnees (une seule fois au demarrage)
# ------------------------------------------------------------------------------
charger_donnees <- function() {
  path <- here("input", "base.dta")
  if (!file.exists(path)) stop("Fichier introuvable : input/base.dta")

  df_raw <- haven::read_dta(path)

  col_lbl <- setNames(
    sapply(df_raw, function(x) { l <- attr(x, "label"); if (is.null(l)) "" else l }),
    names(df_raw)
  )

  df <- df_raw |>
    haven::as_factor() |>
    dplyr::mutate(dplyr::across(where(is.character), as.factor))

  df$region_lbl <- as.character(df$REGION)
  df$milieu_lbl <- as.character(df$URBRUR)
  df$sexe       <- as.character(df$Q100)

  df$age_num <- suppressWarnings(as.numeric(as.character(df_raw$Q1)))
  df$tranche_age <- cut(df$age_num,
    breaks = c(17, 24, 34, 44, 54, 120),
    labels = c("18-24", "25-34", "35-44", "45-54", "55+"),
    right  = TRUE)

  # Score de privation (Q6A-Q6E)
  priv_num <- df_raw |>
    dplyr::select(Q6A, Q6B, Q6C, Q6D, Q6E) |>
    dplyr::mutate(dplyr::across(everything(), ~ {
      v <- suppressWarnings(as.numeric(as.character(.)))
      ifelse(v >= 8, NA_real_, v)
    }))
  df$score_privation <- rowMeans(priv_num, na.rm = TRUE)

  # Score d'actifs (Q90A-Q90F, binaire)
  act_num <- df_raw |>
    dplyr::select(Q90A, Q90B, Q90C, Q90D, Q90E, Q90F) |>
    dplyr::mutate(dplyr::across(everything(), ~ {
      v <- suppressWarnings(as.numeric(as.character(.)))
      ifelse(v >= 8, NA_real_, pmin(v, 1))
    }))
  df$score_actifs <- rowSums(act_num, na.rm = TRUE)

  # Statut d'emploi
  emp_raw <- suppressWarnings(as.numeric(as.character(df_raw$Q93A)))
  df$statut_emploi <- factor(emp_raw, levels = 0:3,
    labels = c("Sans emploi (ne cherche pas)", "Sans emploi (cherche)",
               "Temps partiel", "Temps plein"))

  df$direction_pays      <- as.character(df$Q3)
  df$econ_pays           <- as.character(df$Q4A)
  df$econ_perso          <- as.character(df$Q4B)
  df$insecurite_quartier <- as.character(df$Q7A)
  df$insecurite_maison   <- as.character(df$Q7B)
  df$corrup_police       <- as.character(df$Q43C)
  df$corrup_fonct        <- as.character(df$Q43E)

  list(df = df, col_lbl = col_lbl)
}

loaded  <- charger_donnees()
DF      <- loaded$df
N_TOTAL <- nrow(DF)
REGIONS <- sort(unique(as.character(DF$region_lbl)))
MILIEUX <- sort(unique(as.character(DF$milieu_lbl)))
SEXES   <- sort(unique(as.character(DF$sexe)))
TRANCHES <- levels(DF$tranche_age)

# Charger la base brute une seule fois pour le QAQC
DF_RAW  <- haven::read_dta(here("input", "base.dta"))

# ------------------------------------------------------------------------------
# Constantes UI
# ------------------------------------------------------------------------------
CAPTION_STD <- "Source : Afrobarometer Round 9 - Senegal (2022) | ENSAE Dakar"

EXCL_VALS <- c(
  "Refuse de repondre [Ne pas lire]", "Je ne sais pas [Ne pas lire]",
  "Refuse de repondre", "Je ne sais pas",
  "Refuse de répondre [Ne pas lire]", "Je ne sais pas [Ne pas lire]",
  "Refuse de répondre", "Je ne sais pas"
)

# ------------------------------------------------------------------------------
# Theme ggplot
# ------------------------------------------------------------------------------
theme_afro <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title       = element_text(colour = PAL$bleu, face = "bold", size = base_size + 1,
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(colour = PAL$gris, size = base_size - 1),
      plot.caption     = element_text(colour = "#9CA3AF", size = 7.5, hjust = 1,
                                      margin = margin(t = 4)),
      axis.text        = element_text(colour = PAL$gris, size = base_size - 1),
      axis.title       = element_text(colour = PAL$gris, size = base_size - 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x      = element_line(colour = "#D1D5DB", linewidth = 0.4),
      legend.position  = "bottom",
      legend.title     = element_text(colour = PAL$gris, face = "bold", size = base_size - 2),
      legend.text      = element_text(colour = PAL$gris, size = base_size - 2),
      legend.margin    = margin(t = 2),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      strip.text       = element_text(colour = PAL$bleu, face = "bold"),
      plot.margin      = margin(8, 16, 6, 8)
    )
}

# ------------------------------------------------------------------------------
# Helpers graphiques
# ------------------------------------------------------------------------------
bar_prop <- function(df, var, titre, subtitle = "", fill_col = PAL$bleu) {
  d <- df |>
    dplyr::filter(!is.na(.data[[var]]),
                  !as.character(.data[[var]]) %in% EXCL_VALS) |>
    dplyr::count(.data[[var]], name = "n") |>
    dplyr::mutate(
      pct = n / sum(n) * 100,
      val = stringr::str_wrap(as.character(.data[[var]]), width = 30)
    )

  if (nrow(d) == 0) return(ggplot() + theme_void() +
    labs(title = "Aucune donnee disponible pour ce filtre"))

  ggplot(d, aes(x = reorder(val, pct), y = pct)) +
    geom_col(fill = fill_col, width = 0.55, alpha = 0.9) +
    geom_text(aes(label = paste0(round(pct, 1), "%")),
              hjust = -0.1, size = 3.4, colour = "#374151", fontface = "bold") +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22)),
                       labels = function(x) paste0(x, "%")) +
    labs(title = titre, subtitle = subtitle, caption = CAPTION_STD,
         x = NULL, y = NULL) +
    theme_afro() +
    theme(axis.line.x = element_blank())
}

bar_groupe <- function(df, var, groupvar, titre, subtitle = "") {
  d <- df |>
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[groupvar]]),
                  !as.character(.data[[var]]) %in% EXCL_VALS) |>
    dplyr::count(.data[[groupvar]], .data[[var]], name = "n") |>
    dplyr::group_by(.data[[groupvar]]) |>
    dplyr::mutate(pct = n / sum(n) * 100) |>
    dplyr::ungroup()

  if (nrow(d) == 0) return(ggplot() + theme_void() +
    labs(title = "Aucune donnee disponible pour ce filtre"))

  nom_grp <- switch(groupvar,
    milieu_lbl  = "Milieu",
    sexe        = "Sexe",
    tranche_age = "Tranche d'age",
    groupvar)

  d <- d |> dplyr::mutate(
    val_wrap = stringr::str_wrap(as.character(.data[[var]]), width = 14)
  )

  ggplot(d, aes(x = val_wrap, y = pct,
                fill = as.character(.data[[groupvar]]))) +
    geom_col(position = position_dodge(0.72), width = 0.62, alpha = 0.9) +
    geom_text(aes(label = paste0(round(pct, 0), "%")),
              position = position_dodge(0.72), vjust = -0.35,
              size = 2.8, colour = PAL$gris) +
    scale_fill_manual(values = PAL_BARS, name = nom_grp) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       labels = function(x) paste0(x, "%")) +
    labs(title = titre, subtitle = subtitle, caption = CAPTION_STD,
         x = NULL, y = NULL) +
    theme_afro() +
    theme(axis.text.x = element_text(size = 8, lineheight = 0.85),
          legend.position = "bottom")
}

# Sidebar communes
sidebar_filtres <- function(prefix = "f") {
  sidebar(
    title = tags$span(icon("sliders"), " Filtres"),
    width = 270,
    bg    = "white",
    open  = "open",
    accordion(
      open = TRUE,
      accordion_panel(
        "Geographie",
        icon = icon("map-marker-alt"),
        selectInput(paste0(prefix, "_region"), "Region",
          choices  = c("Toutes les regions" = "Toutes", REGIONS),
          selected = "Toutes"),
        selectInput(paste0(prefix, "_milieu"), "Milieu",
          choices  = c("Tous les milieux" = "Tous", MILIEUX),
          selected = "Tous")
      ),
      accordion_panel(
        "Profil",
        icon = icon("user"),
        checkboxGroupInput(paste0(prefix, "_sexe"), "Sexe",
          choices = SEXES, selected = SEXES, inline = TRUE),
        checkboxGroupInput(paste0(prefix, "_age"), "Tranche d'age",
          choices = TRANCHES, selected = TRANCHES)
      )
    ),
    hr(style = "margin:10px 0;"),
    uiOutput(paste0(prefix, "_n_obs")),
    if (prefix == "f") {
      tagList(
        downloadButton("dl_data", "Exporter (CSV)",
          class = "btn-sm btn-outline-primary w-100 mt-2"),
        actionButton("reset_filtres", "Reinitialiser les filtres",
          class = "btn-sm btn-outline-secondary w-100 mt-1",
          icon  = icon("rotate-left"))
      )
    }
  )
}

sidebar_options <- function(prefix, group_choices = c(
    "Sans ventilation"  = "none",
    "Milieu"            = "milieu_lbl",
    "Sexe"              = "sexe",
    "Tranche d'age"     = "tranche_age")) {
  sidebar(
    title = tags$span(icon("sliders"), " Options"),
    width = 250,
    bg    = "white",
    selectInput(paste0(prefix, "_region"), "Region",
      choices  = c("Toutes les regions" = "Toutes", REGIONS),
      selected = "Toutes"),
    selectInput(paste0(prefix, "_milieu"), "Milieu",
      choices  = c("Tous les milieux" = "Tous", MILIEUX),
      selected = "Tous"),
    hr(style = "margin:8px 0;"),
    radioButtons(paste0(prefix, "_group"), "Ventiler par :",
      choices  = group_choices,
      selected = "none")
  )
}

# ==============================================================================
# UI
# ==============================================================================
ui <- page_navbar(
  title = tags$span(
    style = "font-weight:600; font-size:1rem; letter-spacing:0.01em;",
    tags$span(style = "color:#FCD116; margin-right:6px;", "|"),
    "Afrobarometer Senegal",
    tags$small(style = "font-weight:400; opacity:0.75; margin-left:6px;",
               "Round 9 - 2022")
  ),
  theme = bs_theme(
    version      = 5,
    primary      = "#1B3A6B",
    secondary    = "#009A44",
    success      = "#009A44",
    info         = "#0277BD",
    warning      = "#E6A817",
    danger       = "#CE1126",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    bg           = "#F4F6F9",
    fg           = "#1F2937",
    "navbar-bg"             = "#1B3A6B",
    "navbar-light-color"    = "#FFFFFF",
    "card-border-color"     = "#E2E8F0",
    "card-cap-bg"           = "#F8FAFC",
    "accordion-border-color"= "#E2E8F0"
  ),
  navbar_options = navbar_options(bg = "#1B3A6B", fg = "#FFFFFF"),
  fillable = FALSE,
  lang     = "fr",

  # CSS personnalise
  header = tags$head(tags$style(HTML("
    .card-header { font-weight: 600; font-size: 0.88rem;
                   color: #1B3A6B; border-bottom: 2px solid #E2E8F0; }
    .sidebar-title { font-weight: 700; color: #1B3A6B; font-size: 0.9rem; }
    .value-box .value-box-value { font-size: 1.5rem; font-weight: 700; }
    .value-box .value-box-title { font-size: 0.78rem; text-transform: uppercase;
                                   letter-spacing: 0.05em; opacity: 0.9; }
    .bslib-card { box-shadow: 0 1px 4px rgba(0,0,0,.07); border-radius: 8px; }
    .accordion-button { font-size: 0.85rem; font-weight: 600; color: #1B3A6B; }
    .n-obs-badge { background:#EFF6FF; color:#1B3A6B; border-radius:6px;
                   padding:6px 10px; font-size:0.82rem; margin-top:6px;
                   display:flex; align-items:center; gap:6px; }
    .nav-link { font-size: 0.88rem; }
  "))),

  # ============================================================
  # ONGLET 1 : Vue d'ensemble
  # ============================================================
  nav_panel(
    "Vue d'ensemble",
    icon = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar_filtres("f"),

      layout_column_wrap(
        width = 1/4,
        value_box(
          title    = "Individus enquetes",
          value    = textOutput("kpi_n"),
          showcase = icon("users"),
          theme    = "primary",
          p(style = "font-size:0.75rem; opacity:0.85;",
            "sur 14 regions du Senegal")
        ),
        value_box(
          title    = "Part urbaine",
          value    = textOutput("kpi_urbain"),
          showcase = icon("city"),
          theme    = "success",
          p(style = "font-size:0.75rem; opacity:0.85;",
            "des repondants en milieu urbain")
        ),
        value_box(
          title    = "Privation moyenne",
          value    = textOutput("kpi_priv"),
          showcase = icon("house"),
          theme    = "warning",
          p(style = "font-size:0.75rem; opacity:0.85;",
            "score composite / 4 (5 besoins)")
        ),
        value_box(
          title    = "Score d'actifs moyen",
          value    = textOutput("kpi_actifs"),
          showcase = icon("tv"),
          theme    = "info",
          p(style = "font-size:0.75rem; opacity:0.85;",
            "biens possedes / 6 (radio a internet)")
        )
      ),

      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(icon("compass"), " Direction generale du pays (Q3)"),
          plotOutput("plot_direction", height = 210)
        ),
        card(
          full_screen = TRUE,
          card_header(icon("chart-line"), " Perception economique - National vs Personnel (Q4A/Q4B)"),
          plotOutput("plot_econ", height = 210)
        )
      ),

      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(icon("map"), " Repartition des enquetes par region"),
          plotOutput("plot_region", height = 290)
        ),
        card(
          full_screen = TRUE,
          card_header(icon("users"), " Pyramide des ages par sexe"),
          plotOutput("plot_pyramide", height = 290)
        )
      )
    )
  ),

  # ============================================================
  # ONGLET 2 : Conditions de vie
  # ============================================================
  nav_panel(
    "Conditions de vie",
    icon = icon("house"),
    layout_sidebar(
      sidebar = sidebar_options("cv", group_choices = c(
        "Sans ventilation" = "none",
        "Milieu"           = "milieu_lbl",
        "Sexe"             = "sexe",
        "Tranche d'age"    = "tranche_age"
      )),

      layout_column_wrap(
        width = 1/3,
        card(full_screen = TRUE,
             card_header(icon("utensils"), " Manque de nourriture (Q6A)"),
             plotOutput("cv_q6a", height = 250)),
        card(full_screen = TRUE,
             card_header(icon("droplet"), " Manque d'eau potable (Q6B)"),
             plotOutput("cv_q6b", height = 250)),
        card(full_screen = TRUE,
             card_header(icon("pills"), " Manque de medicaments (Q6C)"),
             plotOutput("cv_q6c", height = 250))
      ),
      layout_column_wrap(
        width = 1/3,
        card(full_screen = TRUE,
             card_header(icon("fire"), " Manque de combustible (Q6D)"),
             plotOutput("cv_q6d", height = 250)),
        card(full_screen = TRUE,
             card_header(icon("coins"), " Manque de revenus (Q6E)"),
             plotOutput("cv_q6e", height = 250)),
        card(full_screen = TRUE,
             card_header(icon("chart-bar"), " Score de privation par region"),
             plotOutput("cv_priv_region", height = 250))
      )
    )
  ),

  # ============================================================
  # ONGLET 3 : Actifs et Emploi
  # ============================================================
  nav_panel(
    "Actifs et Emploi",
    icon = icon("briefcase"),
    layout_sidebar(
      sidebar = sidebar_options("ae", group_choices = c(
        "Sans ventilation" = "none",
        "Milieu"           = "milieu_lbl",
        "Sexe"             = "sexe"
      )),
      layout_column_wrap(
        width = 1/2,
        card(full_screen = TRUE,
             card_header(icon("person-digging"), " Statut d'emploi salarie (Q93A)"),
             plotOutput("ae_emploi", height = 300)),
        card(full_screen = TRUE,
             card_header(icon("star"), " Score d'actifs moyen par region"),
             plotOutput("ae_actifs_region", height = 300))
      ),
      layout_column_wrap(
        width = 1,
        card(full_screen = TRUE,
             card_header(icon("boxes-stacked"), " Possession de biens du menage (Q90A-F)"),
             plotOutput("ae_biens", height = 300))
      )
    )
  ),

  # ============================================================
  # ONGLET 4 : Gouvernance et Securite
  # ============================================================
  nav_panel(
    "Gouvernance et Securite",
    icon = icon("landmark"),
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(icon("sliders"), " Options"),
        width = 250, bg = "white",
        selectInput("gs_region", "Region",
          choices  = c("Toutes les regions" = "Toutes", REGIONS),
          selected = "Toutes"),
        selectInput("gs_milieu", "Milieu",
          choices  = c("Tous les milieux" = "Tous", MILIEUX),
          selected = "Tous")
      ),

      navset_card_tab(
        full_screen = TRUE,
        nav_panel("Securite",
          layout_column_wrap(
            width = 1/2,
            card(
              card_header(icon("street-view"), " Insecurite dans le quartier (Q7A)"),
              plotOutput("gs_insec_qrt", height = 270)
            ),
            card(
              card_header(icon("door-closed"), " Insecurite au domicile (Q7B)"),
              plotOutput("gs_insec_dom", height = 270)
            )
          )
        ),
        nav_panel("Corruption",
          layout_column_wrap(
            width = 1/2,
            card(
              card_header(icon("user-shield"), " Corruption policiere percue (Q43C)"),
              plotOutput("gs_corr_pol", height = 270)
            ),
            card(
              card_header(icon("building-columns"), " Corruption des fonctionnaires (Q43E)"),
              plotOutput("gs_corr_fonct", height = 270)
            )
          )
        )
      )
    )
  ),

  # ============================================================
  # ONGLET 5 : Controle qualite
  # ============================================================
  nav_panel(
    "Controle qualite",
    icon = icon("check-circle"),

    layout_column_wrap(
      width = 1/3,
      value_box("Observations totales",  textOutput("qaqc_n"),
                showcase = icon("database"),  theme = "primary"),
      value_box("Variables dans la base", textOutput("qaqc_ncols"),
                showcase = icon("table"),     theme = "info"),
      value_box("Taux de NA moyen",       textOutput("qaqc_na_moy"),
                showcase = icon("exclamation-circle"), theme = "warning")
    ),

    layout_column_wrap(
      width = 1/2,
      card(
        full_screen = TRUE,
        card_header(icon("list-check"), " Controles de coherence"),
        tableOutput("qaqc_controles")
      ),
      card(
        full_screen = TRUE,
        card_header(icon("triangle-exclamation"), " Variables avec taux de NA > 10 %"),
        DT::dataTableOutput("qaqc_na_table")
      )
    ),

    layout_column_wrap(
      width = 1/2,
      card(
        full_screen = TRUE,
        card_header(icon("chart-area"), " Repartition des taux de valeurs manquantes"),
        plotOutput("qaqc_na_hist", height = 250)
      ),
      card(
        full_screen = TRUE,
        card_header(icon("map-marker-alt"), " Couverture des 14 regions"),
        plotOutput("qaqc_region_cov", height = 250)
      )
    )
  ),

  # ============================================================
  # ONGLET 6 : Donnees brutes
  # ============================================================
  nav_panel(
    "Donnees",
    icon = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(icon("columns"), " Colonnes"),
        width = 250, bg = "white",
        p(style = "font-size:0.82rem; color:#4A5568;",
          "Selectionnez les colonnes a afficher dans le tableau."),
        checkboxGroupInput("tbl_cols", NULL,
          choices = c(
            "Region"        = "region_lbl",
            "Milieu"        = "milieu_lbl",
            "Sexe"          = "sexe",
            "Tranche d'age" = "tranche_age",
            "Direction pays" = "direction_pays",
            "Econ. nationale" = "econ_pays",
            "Econ. perso."  = "econ_perso",
            "Score privation" = "score_privation",
            "Score actifs"  = "score_actifs",
            "Statut emploi" = "statut_emploi"
          ),
          selected = c("region_lbl", "milieu_lbl", "sexe",
                       "tranche_age", "score_privation", "score_actifs")
        ),
        hr(style = "margin:8px 0;"),
        downloadButton("dl_data2", "Exporter (CSV)",
          class = "btn-sm btn-outline-primary w-100")
      ),
      card(DT::dataTableOutput("tbl_raw"))
    )
  ),

  # ============================================================
  # ONGLET 7 : A propos
  # ============================================================
  nav_panel(
    "A propos",
    icon = icon("info-circle"),
    layout_column_wrap(
      width = 1/3,
      value_box("Enquete",     "Round 9 - 2022",  showcase = icon("calendar"), theme = "primary"),
      value_box("Pays",        "Senegal",          showcase = icon("globe"),    theme = "success"),
      value_box("Echantillon", "1 200 individus",  showcase = icon("users"),    theme = "info")
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header(icon("list"), " Contenu de l'application"),
        tags$table(class = "table table-sm table-hover",
          tags$thead(tags$tr(
            tags$th("Onglet"), tags$th("Description")
          )),
          tags$tbody(
            tags$tr(tags$td(tags$strong("Vue d'ensemble")),
                    tags$td("KPIs, perception economique, pyramide des ages")),
            tags$tr(tags$td(tags$strong("Conditions de vie")),
                    tags$td("Privations : nourriture, eau, soins, combustible, revenus")),
            tags$tr(tags$td(tags$strong("Actifs et Emploi")),
                    tags$td("Biens du menage, statut d'emploi, score d'actifs")),
            tags$tr(tags$td(tags$strong("Gouvernance et Securite")),
                    tags$td("Insecurite percue, corruption policiere et administrative")),
            tags$tr(tags$td(tags$strong("Controle qualite")),
                    tags$td("Coherence, valeurs manquantes, couverture regionale")),
            tags$tr(tags$td(tags$strong("Donnees")),
                    tags$td("Table interactive filtreable et exportable"))
          )
        )
      ),
      card(
        card_header(icon("code"), " Variables cles"),
        tags$table(class = "table table-sm table-hover",
          tags$thead(tags$tr(
            tags$th("Code"), tags$th("Description")
          )),
          tags$tbody(
            tags$tr(tags$td(tags$code("Q6A-Q6E")),  tags$td("Frequence des privations (0 = Jamais / 4 = Toujours)")),
            tags$tr(tags$td(tags$code("Q90A-F")),   tags$td("Possession de biens (radio, TV, voiture, ordinateur, banque, telephone)")),
            tags$tr(tags$td(tags$code("Q93A")),     tags$td("Statut d'emploi salarie")),
            tags$tr(tags$td(tags$code("Q4A/Q4B")), tags$td("Evaluation economique (pays / personnelle)")),
            tags$tr(tags$td(tags$code("Q43C/Q43E")), tags$td("Corruption percue (police / fonctionnaires)")),
            tags$tr(tags$td(tags$code("WITHINWT")), tags$td("Variable de ponderation"))
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ---- Reactive : donnees filtrees (Vue d'ensemble) -------------------------
  df_filt <- reactive({
    d <- DF
    req_region <- isTruthy(input$f_region) && input$f_region != "Toutes"
    req_milieu <- isTruthy(input$f_milieu) && input$f_milieu != "Tous"
    if (req_region) d <- d |> dplyr::filter(region_lbl == input$f_region)
    if (req_milieu) d <- d |> dplyr::filter(milieu_lbl == input$f_milieu)
    if (isTruthy(input$f_sexe) && length(input$f_sexe) > 0)
      d <- d |> dplyr::filter(sexe %in% input$f_sexe)
    if (isTruthy(input$f_age) && length(input$f_age) > 0)
      d <- d |> dplyr::filter(as.character(tranche_age) %in% input$f_age)
    d
  })

  # ---- Reactive : conditions de vie ----------------------------------------
  df_cv <- reactive({
    d <- DF
    if (isTruthy(input$cv_region) && input$cv_region != "Toutes")
      d <- d |> dplyr::filter(region_lbl == input$cv_region)
    if (isTruthy(input$cv_milieu) && input$cv_milieu != "Tous")
      d <- d |> dplyr::filter(milieu_lbl == input$cv_milieu)
    d
  })

  # ---- Reactive : actifs/emploi --------------------------------------------
  df_ae <- reactive({
    d <- DF
    if (isTruthy(input$ae_region) && input$ae_region != "Toutes")
      d <- d |> dplyr::filter(region_lbl == input$ae_region)
    if (isTruthy(input$ae_milieu) && input$ae_milieu != "Tous")
      d <- d |> dplyr::filter(milieu_lbl == input$ae_milieu)
    d
  })

  # ---- Reactive : gouvernance/securite -------------------------------------
  df_gs <- reactive({
    d <- DF
    if (isTruthy(input$gs_region) && input$gs_region != "Toutes")
      d <- d |> dplyr::filter(region_lbl == input$gs_region)
    if (isTruthy(input$gs_milieu) && input$gs_milieu != "Tous")
      d <- d |> dplyr::filter(milieu_lbl == input$gs_milieu)
    d
  })

  # ---- Reset filtres --------------------------------------------------------
  observeEvent(input$reset_filtres, {
    updateSelectInput(session, "f_region", selected = "Toutes")
    updateSelectInput(session, "f_milieu", selected = "Tous")
    updateCheckboxGroupInput(session, "f_sexe", selected = SEXES)
    updateCheckboxGroupInput(session, "f_age",  selected = TRANCHES)
  })

  # ---- Badge observations selectionnees ------------------------------------
  badge_obs <- function(n, id) {
    output[[id]] <- renderUI({
      tags$div(class = "n-obs-badge",
        icon("filter"),
        tags$strong(format(n, big.mark = " ")),
        " observations selectionnees"
      )
    })
  }
  observe(badge_obs(nrow(df_filt()), "f_n_obs"))

  # ---- KPIs ----------------------------------------------------------------
  output$kpi_n <- renderText(format(nrow(df_filt()), big.mark = " "))

  output$kpi_urbain <- renderText({
    paste0(round(mean(df_filt()$milieu_lbl == "Urbain", na.rm = TRUE) * 100, 1), " %")
  })
  output$kpi_priv <- renderText({
    paste0(round(mean(df_filt()$score_privation, na.rm = TRUE), 2), " / 4")
  })
  output$kpi_actifs <- renderText({
    paste0(round(mean(df_filt()$score_actifs, na.rm = TRUE), 2), " / 6")
  })

  # ---- Vue d'ensemble - plots ----------------------------------------------
  output$plot_direction <- renderPlot({
    bar_prop(df_filt(), "direction_pays", "Direction generale du pays",
             fill_col = PAL$bleu)
  }, res = 100)

  output$plot_econ <- renderPlot({
    d <- df_filt() |>
      dplyr::select(econ_pays, econ_perso) |>
      tidyr::pivot_longer(everything(), names_to = "indicateur", values_to = "val") |>
      dplyr::filter(!is.na(val), !as.character(val) %in% EXCL_VALS) |>
      dplyr::mutate(
        indicateur = dplyr::recode(indicateur,
          econ_pays  = "Economie\nnationale",
          econ_perso = "Conditions\npersonnelles"),
        val_wrap = stringr::str_wrap(as.character(val), width = 12)
      ) |>
      dplyr::count(indicateur, val_wrap) |>
      dplyr::group_by(indicateur) |>
      dplyr::mutate(pct = n / sum(n) * 100) |>
      dplyr::ungroup()

    ggplot(d, aes(x = reorder(val_wrap, pct), y = pct, fill = indicateur)) +
      geom_col(position = position_dodge(0.72), width = 0.62, alpha = 0.9) +
      geom_text(aes(label = paste0(round(pct, 0), "%")),
                position = position_dodge(0.72), hjust = -0.1,
                size = 3.0, colour = PAL$gris) +
      scale_fill_manual(
        values = c("Economie\nnationale" = PAL$bleu, "Conditions\npersonnelles" = PAL$vert),
        name   = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.22)),
                         labels = function(x) paste0(x, "%")) +
      coord_flip() +
      labs(title = "Perception economique (Q4A / Q4B)",
           caption = CAPTION_STD, x = NULL, y = NULL) +
      theme_afro() +
      theme(legend.position = "bottom")
  }, res = 100)

  output$plot_region <- renderPlot({
    d <- df_filt() |>
      dplyr::filter(!is.na(region_lbl)) |>
      dplyr::count(region_lbl, name = "n") |>
      dplyr::mutate(pct = n / sum(n) * 100)
    ggplot(d, aes(x = reorder(region_lbl, pct), y = pct)) +
      geom_col(fill = PAL$bleu, width = 0.68, alpha = 0.88) +
      geom_text(aes(label = paste0(round(pct, 0), "%")),
                hjust = -0.1, size = 3.2, colour = PAL$gris, fontface = "bold") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                         labels = function(x) paste0(x, "%")) +
      labs(title = "Repartition par region", caption = CAPTION_STD,
           x = NULL, y = NULL) +
      theme_afro() +
      theme(axis.line.x = element_blank())
  }, res = 100)

  output$plot_pyramide <- renderPlot({
    d <- df_filt() |>
      dplyr::filter(!is.na(tranche_age), sexe %in% c("Homme", "Femme")) |>
      dplyr::count(tranche_age, sexe) |>
      dplyr::group_by(sexe) |>
      dplyr::mutate(pct = n / sum(n) * 100,
                    pct_dir = ifelse(sexe == "Homme", -pct, pct)) |>
      dplyr::ungroup()
    ggplot(d, aes(x = tranche_age, y = pct_dir, fill = sexe)) +
      geom_col(width = 0.68, alpha = 0.9) +
      scale_fill_manual(values = c("Homme" = PAL$bleu, "Femme" = PAL$rouge),
                        name = NULL) +
      scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
      coord_flip() +
      labs(title = "Pyramide des ages",
           subtitle = "Hommes (gauche) / Femmes (droite)",
           caption = CAPTION_STD, x = NULL, y = "%") +
      theme_afro() +
      theme(legend.position = "bottom")
  }, res = 100)

  # ---- Conditions de vie ---------------------------------------------------
  cv_plot <- function(col, titre) {
    renderPlot({
      grp <- input$cv_group
      if (grp == "none") bar_prop(df_cv(), col, titre)
      else               bar_groupe(df_cv(), col, grp, titre)
    }, res = 100)
  }
  output$cv_q6a <- cv_plot("Q6A", "Manque de nourriture")
  output$cv_q6b <- cv_plot("Q6B", "Manque d'eau potable")
  output$cv_q6c <- cv_plot("Q6C", "Manque de medicaments")
  output$cv_q6d <- cv_plot("Q6D", "Manque de combustible")
  output$cv_q6e <- cv_plot("Q6E", "Manque de revenus")

  output$cv_priv_region <- renderPlot({
    d <- df_cv() |>
      dplyr::filter(!is.na(region_lbl)) |>
      dplyr::group_by(region_lbl) |>
      dplyr::summarise(priv_moy = mean(score_privation, na.rm = TRUE), .groups = "drop")
    ggplot(d, aes(x = reorder(region_lbl, priv_moy), y = priv_moy)) +
      geom_col(fill = PAL$rouge, alpha = 0.85, width = 0.68) +
      geom_text(aes(label = round(priv_moy, 2)), hjust = -0.1,
                size = 3.2, colour = PAL$gris, fontface = "bold") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.22)),
                         limits = c(0, 4)) +
      labs(title = "Score de privation moyen par region (/ 4)",
           caption = CAPTION_STD, x = NULL, y = NULL) +
      theme_afro() +
      theme(axis.line.x = element_blank())
  }, res = 100)

  # ---- Actifs et Emploi ----------------------------------------------------
  output$ae_emploi <- renderPlot({
    grp <- input$ae_group
    if (grp == "none")
      bar_prop(df_ae(), "statut_emploi", "Statut d'emploi salarie", fill_col = PAL$vert)
    else
      bar_groupe(df_ae(), "statut_emploi", grp, "Statut d'emploi salarie")
  }, res = 100)

  output$ae_actifs_region <- renderPlot({
    d <- df_ae() |>
      dplyr::filter(!is.na(region_lbl)) |>
      dplyr::group_by(region_lbl) |>
      dplyr::summarise(actifs_moy = mean(score_actifs, na.rm = TRUE), .groups = "drop")
    ggplot(d, aes(x = reorder(region_lbl, actifs_moy), y = actifs_moy)) +
      geom_col(fill = PAL$bleu, alpha = 0.87, width = 0.68) +
      geom_text(aes(label = round(actifs_moy, 1)), hjust = -0.12,
                size = 3.2, colour = PAL$gris, fontface = "bold") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)), limits = c(0, 6)) +
      labs(title = "Score d'actifs moyen par region (/ 6)",
           caption = CAPTION_STD, x = NULL, y = NULL) +
      theme_afro() +
      theme(axis.line.x = element_blank())
  }, res = 100)

  output$ae_biens <- renderPlot({
    biens_map <- c(Q90A = "Radio", Q90B = "Television", Q90C = "Voiture / Moto",
                   Q90D = "Ordinateur", Q90E = "Compte bancaire", Q90F = "Telephone")
    grp <- input$ae_group

    d <- df_ae() |>
      dplyr::select(dplyr::any_of(c(names(biens_map), grp))) |>
      tidyr::pivot_longer(dplyr::any_of(names(biens_map)),
                          names_to = "bien", values_to = "val") |>
      dplyr::mutate(
        bien_lbl = biens_map[bien],
        possede  = as.character(val) %in%
                   c("Oui (en possede personnellement)",
                     "Quelqu'un d'autre dans le menage en possede",
                     "Oui (en possède personnellement)",
                     "Quelqu'un d'autre dans le ménage en possède")
      ) |>
      dplyr::filter(!is.na(possede))

    if (grp != "none" && grp %in% names(d)) {
      d2 <- d |>
        dplyr::group_by(bien_lbl, .data[[grp]]) |>
        dplyr::summarise(pct = mean(possede) * 100, .groups = "drop")
      ggplot(d2, aes(x = bien_lbl, y = pct, fill = as.character(.data[[grp]]))) +
        geom_col(position = position_dodge(0.72), width = 0.62, alpha = 0.9) +
        scale_fill_manual(values = PAL_BARS, name = NULL) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                           labels = function(x) paste0(x, "%")) +
        labs(title = "Possession de biens (%)", caption = CAPTION_STD,
             x = NULL, y = "%") +
        theme_afro() +
        theme(legend.position = "bottom")
    } else {
      d2 <- d |>
        dplyr::group_by(bien_lbl) |>
        dplyr::summarise(pct = mean(possede) * 100, .groups = "drop")
      ggplot(d2, aes(x = reorder(bien_lbl, pct), y = pct)) +
        geom_col(fill = PAL$vert, alpha = 0.88, width = 0.65) +
        geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.12,
                  size = 3.6, colour = PAL$gris) +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                           labels = function(x) paste0(x, "%")) +
        labs(title = "Possession de biens dans le menage (%)",
             caption = CAPTION_STD, x = NULL, y = "% des menages") +
        theme_afro()
    }
  }, res = 100)

  # ---- Gouvernance et Securite ---------------------------------------------
  output$gs_insec_qrt <- renderPlot(
    bar_prop(df_gs(), "insecurite_quartier",
             "Insecurite dans le quartier", fill_col = PAL$rouge), res = 100)
  output$gs_insec_dom <- renderPlot(
    bar_prop(df_gs(), "insecurite_maison",
             "Insecurite au domicile", fill_col = PAL$rouge), res = 100)
  output$gs_corr_pol <- renderPlot(
    bar_prop(df_gs(), "corrup_police",
             "Corruption policiere percue", fill_col = PAL$gris), res = 100)
  output$gs_corr_fonct <- renderPlot(
    bar_prop(df_gs(), "corrup_fonct",
             "Corruption des fonctionnaires percue", fill_col = PAL$gris), res = 100)

  # ---- Table brute ---------------------------------------------------------
  tbl_data <- reactive({
    cols <- input$tbl_cols
    if (is.null(cols) || length(cols) == 0)
      cols <- c("region_lbl", "milieu_lbl")
    df_filt() |>
      dplyr::select(dplyr::any_of(cols)) |>
      dplyr::mutate(dplyr::across(everything(), as.character))
  })

  output$tbl_raw <- DT::renderDataTable({
    tbl_data()
  }, options = list(
    pageLength = 20, scrollX = TRUE,
    language = list(
      search   = "Rechercher :",
      paginate = list(previous = "Precedent", `next` = "Suivant"),
      info     = "Lignes _START_ a _END_ sur _TOTAL_",
      lengthMenu = "Afficher _MENU_ lignes"
    )
  ), filter = "top", class = "stripe hover compact")

  # ---- QAQC ----------------------------------------------------------------
  qaqc_na <- reactive({
    purrr::map_dfr(names(DF_RAW), function(v) {
      n_na  <- sum(is.na(DF_RAW[[v]]))
      pct   <- round(n_na / N_TOTAL * 100, 2)
      data.frame(variable = v, n_na = n_na, pct_na = pct,
                 statut = ifelse(pct == 0, "OK",
                          ifelse(pct <= 5, "Faible",
                          ifelse(pct <= 20, "Modere", "Eleve"))),
                 stringsAsFactors = FALSE)
    })
  })

  output$qaqc_n      <- renderText(format(N_TOTAL, big.mark = " "))
  output$qaqc_ncols  <- renderText(format(ncol(DF_RAW), big.mark = " "))
  output$qaqc_na_moy <- renderText(paste0(round(mean(qaqc_na()$pct_na), 1), " %"))

  output$qaqc_controles <- renderTable({
    n_dup     <- sum(duplicated(DF_RAW$SbjNum, incomparables = NA))
    age_num   <- suppressWarnings(as.numeric(as.character(DF_RAW$Q1)))
    n_age_inv <- sum(age_num < 18 | age_num > 120, na.rm = TRUE)
    n_gps_ko  <- sum(is.na(DF_RAW$GPS_LA) | is.na(DF_RAW$GPS_LO))
    n_regions <- length(unique(DF_RAW$REGION[!is.na(DF_RAW$REGION)]))
    has_wt    <- "WITHINWT" %in% names(DF_RAW) && sum(!is.na(DF_RAW$WITHINWT)) > 0

    data.frame(
      Controle = c(
        "Unicite identifiant (SbjNum)",
        "Plage d'age plausible (18 ans et plus)",
        "Coordonnees GPS renseignees",
        "Couverture regionale (14 regions attendues)",
        "Variable de ponderation (WITHINWT)"
      ),
      Statut = c(
        ifelse(n_dup == 0,       "OK", "Anomalie detectee"),
        ifelse(n_age_inv == 0,   "OK", "Anomalie detectee"),
        ifelse(n_gps_ko == 0,    "OK", paste0(n_gps_ko, " valeurs manquantes")),
        ifelse(n_regions == 14,  "OK (14/14)", paste0(n_regions, " / 14")),
        ifelse(has_wt, "Disponible", "Absente")
      ),
      Detail = c(
        paste0(n_dup, " doublon(s)"),
        paste0(n_age_inv, " valeur(s) hors plage"),
        paste0(N_TOTAL - n_gps_ko, " / ", N_TOTAL, " GPS valides"),
        paste0(n_regions, " region(s) presentes"),
        ifelse(has_wt, paste0(sum(!is.na(DF_RAW$WITHINWT)), " poids non manquants"),
               "Ponderation indisponible")
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$qaqc_na_table <- DT::renderDataTable({
    qaqc_na() |>
      dplyr::filter(pct_na > 10) |>
      dplyr::arrange(dplyr::desc(pct_na)) |>
      dplyr::select(Variable = variable, "NA (n)" = n_na,
                    "NA (%)" = pct_na, Statut = statut) |>
      head(60)
  }, options = list(pageLength = 10, scrollX = TRUE,
                    language = list(search = "Rechercher :")),
  class = "compact stripe")

  output$qaqc_na_hist <- renderPlot({
    d <- qaqc_na()
    ggplot(d, aes(x = pct_na)) +
      geom_histogram(binwidth = 5, fill = PAL$bleu, colour = "white", alpha = 0.85) +
      geom_vline(xintercept = 20, colour = PAL$rouge, linetype = "dashed",
                 linewidth = 0.9) +
      annotate("text", x = 21, y = Inf, label = "Seuil critique (20 %)",
               hjust = 0, vjust = 2, colour = PAL$rouge, size = 3.4, fontface = "bold") +
      labs(title = "Distribution des taux de valeurs manquantes",
           subtitle = paste0(sum(d$pct_na == 0), " variables sans NA sur ", nrow(d)),
           caption = CAPTION_STD, x = "Taux de NA (%)", y = "Nombre de variables") +
      theme_afro()
  }, res = 100)

  output$qaqc_region_cov <- renderPlot({
    d <- DF |>
      dplyr::filter(!is.na(region_lbl)) |>
      dplyr::count(region_lbl, name = "n") |>
      dplyr::mutate(
        statut = ifelse(n >= 40, "Couverture adequate", "Couverture faible")
      )
    ggplot(d, aes(x = reorder(region_lbl, n), y = n, fill = statut)) +
      geom_col(width = 0.68, alpha = 0.9) +
      geom_text(aes(label = n), hjust = -0.12, size = 3.2, colour = PAL$gris) +
      scale_fill_manual(
        values = c("Couverture adequate" = PAL$vert, "Couverture faible" = PAL$rouge),
        name   = NULL) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
      labs(title = "Effectif enquete par region",
           caption = CAPTION_STD, x = NULL, y = "Nombre d'observations") +
      theme_afro() +
      theme(legend.position = "bottom")
  }, res = 100)

  # ---- Telechargement ------------------------------------------------------
  dl_handler <- function(data_fn) {
    downloadHandler(
      filename = function() paste0("afrobarometer_sn_", Sys.Date(), ".csv"),
      content  = function(file) {
        data_fn() |>
          dplyr::mutate(dplyr::across(everything(), as.character)) |>
          write.csv(file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  }
  output$dl_data  <- dl_handler(df_filt)
  output$dl_data2 <- dl_handler(tbl_data)
}

shinyApp(ui, server)
