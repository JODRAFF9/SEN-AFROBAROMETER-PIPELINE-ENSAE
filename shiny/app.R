# ==============================================================================
# SHINY APP — Plateforme de visualisation Afrobarometer Sénégal Round 9 (2022)
# Lancer : shiny::runApp("shiny/")
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(haven)
library(here)

# ── Palette professionnelle ────────────────────────────────────────────────────
PAL <- list(
  vert    = "#009A44",   # Sénégal vert
  rouge   = "#CE1126",   # Sénégal rouge
  jaune   = "#FCD116",   # Sénégal jaune
  bleu    = "#1B3A6B",   # Bleu institutionnel
  gris    = "#4A5568",   # Gris ardoise
  clair   = "#F7F8FA",   # Fond clair
  bordure = "#D1D5DB"    # Gris bordure
)

PAL_BARS <- c("#1B3A6B", "#009A44", "#CE1126", "#FCD116", "#4A5568",
              "#2E7D32", "#7B1FA2", "#E65100", "#0277BD", "#558B2F")

# ── Chargement des données ─────────────────────────────────────────────────────
charger_donnees <- function() {
  path <- here("input", "base.dta")
  if (!file.exists(path)) stop("Fichier introuvable : input/base.dta")
  df_raw <- haven::read_dta(path)
  meta   <- list(
    labels     = attr(df_raw[[1]], "label"),
    col_labels = sapply(df_raw, function(x) attr(x, "label") %||% ""),
    val_labels = lapply(df_raw, function(x) attr(x, "labels"))
  )

  # Noms des colonnes avec leurs labels
  col_lbl <- setNames(
    sapply(df_raw, function(x) { l <- attr(x, "label"); if (is.null(l)) "" else l }),
    names(df_raw)
  )

  # Décoder les labels de valeurs (zap_labels → facteur lisible)
  df <- df_raw |>
    haven::as_factor() |>
    dplyr::mutate(dplyr::across(where(is.character), as.factor))

  # Variables dérivées ─────────────────────────────────────────────────────────

  # Région lisible
  df$region_lbl <- as.character(df$REGION)
  df$milieu_lbl <- as.character(df$URBRUR)

  # Sexe
  df$sexe <- as.character(df$Q100)

  # Âge numérique
  df$age_num <- suppressWarnings(as.numeric(as.character(df_raw$Q1)))
  df$tranche_age <- cut(df$age_num, breaks = c(17, 24, 34, 44, 54, 120),
                        labels = c("18-24", "25-34", "35-44", "45-54", "55+"),
                        right  = TRUE)

  # Score privation (Q6A–Q6E : 0=Jamais … 4=Toujours ; 8/9 → NA)
  priv_cols <- c("Q6A","Q6B","Q6C","Q6D","Q6E")
  priv_num  <- df_raw |>
    dplyr::select(dplyr::all_of(priv_cols)) |>
    dplyr::mutate(dplyr::across(everything(), ~ {
      v <- as.numeric(as.character(.))
      ifelse(v >= 8, NA_real_, v)
    }))
  df$score_privation <- rowMeans(priv_num, na.rm = TRUE)
  df$privation_cat <- cut(df$score_privation,
                          breaks = c(-Inf, 0.5, 1.5, 2.5, Inf),
                          labels = c("Aucune", "Légère", "Modérée", "Sévère"))

  # Score actifs (Q90A–Q90F : 0=pas dans ménage, 1=ménage, 2=personnel)
  act_cols <- c("Q90A","Q90B","Q90C","Q90D","Q90E","Q90F")
  act_num  <- df_raw |>
    dplyr::select(dplyr::all_of(act_cols)) |>
    dplyr::mutate(dplyr::across(everything(), ~ {
      v <- as.numeric(as.character(.))
      ifelse(v >= 8, NA_real_, pmin(v, 1))  # binaire : possède ou non
    }))
  df$score_actifs <- rowSums(act_num, na.rm = TRUE)

  # Emploi
  emp_raw <- suppressWarnings(as.numeric(as.character(df_raw$Q93A)))
  df$statut_emploi <- factor(
    emp_raw,
    levels = 0:3,
    labels = c("Sans emploi\n(ne cherche pas)", "Sans emploi\n(cherche)",
               "Temps partiel", "Temps plein")
  )

  # Direction pays (Q3)
  df$direction_pays <- as.character(df$Q3)

  # Perception économie pays (Q4A) et perso (Q4B)
  df$econ_pays   <- as.character(df$Q4A)
  df$econ_perso  <- as.character(df$Q4B)

  # Sécurité (Q7A, Q7B)
  df$insecurite_quartier <- as.character(df$Q7A)
  df$insecurite_maison   <- as.character(df$Q7B)

  # Corruption police (Q43C, Q43E)
  df$corrup_police <- as.character(df$Q43C)
  df$corrup_fonct  <- as.character(df$Q43E)

  list(df = df, col_lbl = col_lbl)
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

# Chargement global
loaded   <- charger_donnees()
DF       <- loaded$df
COL_LBL  <- loaded$col_lbl
N_TOTAL  <- nrow(DF)

REGIONS  <- sort(unique(as.character(DF$region_lbl)))
MILIEUX  <- sort(unique(as.character(DF$milieu_lbl)))
SEXES    <- sort(unique(as.character(DF$sexe)))
TRANCHES <- levels(DF$tranche_age)

# ── Thème ggplot ──────────────────────────────────────────────────────────────
theme_afro <- function() {
  theme_minimal(base_size = 13, base_family = "sans") +
    theme(
      plot.title        = element_text(colour = PAL$bleu, face = "bold", size = 14),
      plot.subtitle     = element_text(colour = PAL$gris, size = 11),
      plot.caption      = element_text(colour = "#9CA3AF", size = 8.5, hjust = 1),
      axis.text         = element_text(colour = PAL$gris),
      axis.title        = element_text(colour = PAL$gris, size = 11),
      panel.grid.major  = element_line(colour = "#E5E7EB", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      legend.position   = "right",
      legend.title      = element_text(colour = PAL$gris, face = "bold", size = 10),
      legend.text       = element_text(colour = PAL$gris, size = 9.5),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.background  = element_rect(fill = "white", colour = NA),
      strip.text        = element_text(colour = PAL$bleu, face = "bold")
    )
}

CAPTION_STD <- "Source : Afrobarometer Round 9 – Sénégal (2022) | ENSAE Dakar"

# ── Helper : barre horizontale avec proportions ───────────────────────────────
bar_prop <- function(df, var, titre, subtitle = "", fill_col = PAL$bleu,
                     exclude_vals = c("Refuse de répondre [Ne pas lire]",
                                      "Je ne sais pas [Ne pas lire]",
                                      "Refuse de répondre",
                                      "Je ne sais pas")) {
  d <- df |>
    dplyr::filter(!is.na(.data[[var]]),
                  !as.character(.data[[var]]) %in% exclude_vals) |>
    dplyr::count(.data[[var]], name = "n") |>
    dplyr::mutate(
      pct   = n / sum(n) * 100,
      label = paste0(round(pct, 1), "%"),
      val   = as.character(.data[[var]])
    )

  ggplot(d, aes(x = reorder(val, pct), y = pct)) +
    geom_col(fill = fill_col, width = 0.65, alpha = 0.92) +
    geom_text(aes(label = label), hjust = -0.15, size = 3.8,
              colour = PAL$gris, fontface = "bold") +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                       labels = function(x) paste0(x, "%")) +
    labs(title = titre, subtitle = subtitle, caption = CAPTION_STD,
         x = NULL, y = "% des répondants") +
    theme_afro()
}

# ── Helper : grouped bar (var x groupvar) ─────────────────────────────────────
bar_groupe <- function(df, var, groupvar, titre, subtitle = "",
                       exclude_vals = c("Refuse de répondre [Ne pas lire]",
                                        "Je ne sais pas [Ne pas lire]",
                                        "Refuse de répondre",
                                        "Je ne sais pas")) {
  d <- df |>
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[groupvar]]),
                  !as.character(.data[[var]]) %in% exclude_vals) |>
    dplyr::count(.data[[groupvar]], .data[[var]], name = "n") |>
    dplyr::group_by(.data[[groupvar]]) |>
    dplyr::mutate(pct = n / sum(n) * 100) |>
    dplyr::ungroup()

  ggplot(d, aes(x = .data[[var]], y = pct,
                fill = as.character(.data[[groupvar]]))) +
    geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.92) +
    geom_text(aes(label = paste0(round(pct, 0), "%")),
              position = position_dodge(0.75), vjust = -0.4,
              size = 3.2, colour = PAL$gris) +
    scale_fill_manual(values = PAL_BARS, name = groupvar) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                       labels = function(x) paste0(x, "%")) +
    labs(title = titre, subtitle = subtitle, caption = CAPTION_STD,
         x = NULL, y = "% des répondants") +
    theme_afro() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
}

# ==============================================================================
# UI
# ==============================================================================
ui <- page_navbar(
  title = tags$span(
    tags$img(src = "https://flagicons.lipis.dev/flags/4x3/sn.svg",
             height = "22px", style = "margin-right:8px;vertical-align:middle;"),
    "Afrobarometer Sénégal — Round 9 (2022)"
  ),
  theme = bs_theme(
    version        = 5,
    primary        = "#1B3A6B",
    secondary      = "#009A44",
    success        = "#009A44",
    info           = "#0277BD",
    warning        = "#FCD116",
    danger         = "#CE1126",
    base_font      = font_google("Inter"),
    heading_font   = font_google("Inter"),
    bg             = "#F7F8FA",
    fg             = "#1F2937",
    "navbar-bg"    = "#1B3A6B",
    "navbar-light-color" = "#FFFFFF"
  ),
  bg = "#1B3A6B", fg = "#FFFFFF",
  fillable = FALSE,

  # ── Sidebar de filtres (partagée) ──────────────────────────────────────────
  nav_panel(
    "Vue d'ensemble",
    icon = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filtres",
        width = 260,
        bg = "white",
        selectInput("f_region", "Région",
                    choices = c("Toutes" = "Toutes", REGIONS),
                    selected = "Toutes"),
        selectInput("f_milieu", "Milieu",
                    choices = c("Tous" = "Tous", MILIEUX),
                    selected = "Tous"),
        checkboxGroupInput("f_sexe", "Sexe",
                           choices = SEXES, selected = SEXES),
        checkboxGroupInput("f_age", "Tranche d'âge",
                           choices = TRANCHES, selected = TRANCHES),
        hr(),
        uiOutput("ui_n_obs"),
        downloadButton("dl_data", "Télécharger les données filtrées",
                       class = "btn-sm btn-outline-primary w-100 mt-2")
      ),

      # KPI cards
      layout_column_wrap(
        width = 1/4,
        value_box("Individus enquêtés", textOutput("kpi_n"),
                  showcase = icon("users"), theme = "primary"),
        value_box("% urbain", textOutput("kpi_urbain"),
                  showcase = icon("city"), theme = "success"),
        value_box("Privation moyenne", textOutput("kpi_priv"),
                  showcase = icon("house-crack"), theme = "warning"),
        value_box("Score actifs moyen", textOutput("kpi_actifs"),
                  showcase = icon("tv"), theme = "info")
      ),

      layout_column_wrap(
        width = 1/2,
        card(card_header("Direction du pays (Q3)"),
             plotOutput("plot_direction", height = 220)),
        card(card_header("Perception économique – Pays vs Perso (Q4A/Q4B)"),
             plotOutput("plot_econ", height = 220))
      ),

      layout_column_wrap(
        width = 1/2,
        card(card_header("Distribution par région"),
             plotOutput("plot_region", height = 300)),
        card(card_header("Pyramide par âge et sexe"),
             plotOutput("plot_pyramide", height = 300))
      )
    )
  ),

  nav_panel(
    "Conditions de vie",
    icon = icon("house"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        width = 260, bg = "white",
        selectInput("cv_region", "Région",
                    choices = c("Toutes" = "Toutes", REGIONS), selected = "Toutes"),
        selectInput("cv_milieu", "Milieu",
                    choices = c("Tous" = "Tous", MILIEUX), selected = "Tous"),
        radioButtons("cv_group", "Ventiler par :",
                     choices = c("Aucun" = "none", "Milieu" = "milieu_lbl",
                                 "Sexe" = "sexe", "Tranche d'âge" = "tranche_age"),
                     selected = "none")
      ),
      layout_column_wrap(
        width = 1/3,
        card(card_header("Manque de nourriture (Q6A)"),
             plotOutput("cv_q6a", height = 260)),
        card(card_header("Manque d'eau potable (Q6B)"),
             plotOutput("cv_q6b", height = 260)),
        card(card_header("Manque de médicaments (Q6C)"),
             plotOutput("cv_q6c", height = 260))
      ),
      layout_column_wrap(
        width = 1/3,
        card(card_header("Manque de combustible (Q6D)"),
             plotOutput("cv_q6d", height = 260)),
        card(card_header("Manque de revenus (Q6E)"),
             plotOutput("cv_q6e", height = 260)),
        card(card_header("Score de privation par région"),
             plotOutput("cv_priv_region", height = 260))
      )
    )
  ),

  nav_panel(
    "Actifs & Emploi",
    icon = icon("briefcase"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        width = 260, bg = "white",
        selectInput("ae_region", "Région",
                    choices = c("Toutes" = "Toutes", REGIONS), selected = "Toutes"),
        radioButtons("ae_group", "Ventiler par :",
                     choices = c("Aucun" = "none", "Milieu" = "milieu_lbl",
                                 "Sexe" = "sexe"),
                     selected = "none")
      ),
      layout_column_wrap(
        width = 1/2,
        card(card_header("Statut d'emploi (Q93A)"),
             plotOutput("ae_emploi", height = 300)),
        card(card_header("Score d'actifs par région (radar)"),
             plotOutput("ae_actifs_region", height = 300))
      ),
      layout_column_wrap(
        width = 1,
        card(card_header("Possession de biens par type (Q90A–F)"),
             plotOutput("ae_biens", height = 300))
      )
    )
  ),

  nav_panel(
    "Gouvernance & Sécurité",
    icon = icon("landmark"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        width = 260, bg = "white",
        selectInput("gs_region", "Région",
                    choices = c("Toutes" = "Toutes", REGIONS), selected = "Toutes"),
        selectInput("gs_milieu", "Milieu",
                    choices = c("Tous" = "Tous", MILIEUX), selected = "Tous")
      ),
      layout_column_wrap(
        width = 1/2,
        card(card_header("Insécurité dans le quartier (Q7A)"),
             plotOutput("gs_insec_qrt", height = 260)),
        card(card_header("Insécurité au domicile (Q7B)"),
             plotOutput("gs_insec_dom", height = 260))
      ),
      layout_column_wrap(
        width = 1/2,
        card(card_header("Corruption policière (Q43C)"),
             plotOutput("gs_corr_pol", height = 260)),
        card(card_header("Corruption fonctionnaires (Q43E)"),
             plotOutput("gs_corr_fonct", height = 260))
      )
    )
  ),

  nav_panel(
    "Données brutes",
    icon = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Colonnes à afficher",
        width = 280, bg = "white",
        checkboxGroupInput("tbl_cols", NULL,
          choices  = c("region_lbl", "milieu_lbl", "sexe", "tranche_age",
                       "direction_pays", "econ_pays", "econ_perso",
                       "score_privation", "score_actifs", "statut_emploi"),
          selected = c("region_lbl", "milieu_lbl", "sexe", "tranche_age",
                       "score_privation", "score_actifs")
        )
      ),
      card(DT::dataTableOutput("tbl_raw"))
    )
  ),

  nav_panel(
    "Contrôle qualité",
    icon = icon("shield-check"),
    layout_column_wrap(
      width = 1/3,
      value_box("Observations totales", textOutput("qaqc_n"),
                showcase = icon("database"), theme = "primary"),
      value_box("Variables analysées", textOutput("qaqc_ncols"),
                showcase = icon("table-columns"), theme = "info"),
      value_box("Taux NA moyen", textOutput("qaqc_na_moy"),
                showcase = icon("circle-exclamation"), theme = "warning")
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Contrôles de cohérence"),
        tableOutput("qaqc_controles")
      ),
      card(
        card_header("Variables avec taux de NA > 10%"),
        DT::dataTableOutput("qaqc_na_table")
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Distribution des taux de NA"),
        plotOutput("qaqc_na_hist", height = 260)
      ),
      card(
        card_header("Répartition géographique — couverture des régions"),
        plotOutput("qaqc_region_cov", height = 260)
      )
    )
  ),

  nav_panel(
    "À propos",
    icon = icon("info-circle"),
    card(
      card_header("Pipeline Afrobarometer Sénégal"),
      markdown("
**Source** : Afrobarometer Round 9 — Sénégal (2022)
**Organisme** : ENSAE Dakar
**N** : 1 200 individus enquêtés dans les 14 régions du Sénégal

---

### Modules de l'application

| Onglet | Description |
|--------|-------------|
| Vue d'ensemble | KPIs, direction du pays, économie, pyramide des âges |
| Conditions de vie | Privations alimentaires, eau, soins, combustible, revenus |
| Actifs & Emploi | Biens du ménage, statut d'emploi, score d'actifs |
| Gouvernance & Sécurité | Insécurité, corruption perçue |
| Données brutes | Table interactive exportable |

---

### Variables clés

- **Q6A–Q6E** : Fréquence des privations (0 = Jamais … 4 = Toujours)
- **Q90A–F** : Possession de biens (radio, TV, voiture, ordinateur, banque, téléphone)
- **Q93A** : Statut d'emploi salarié
- **Q4A/Q4B** : Évaluation de la situation économique (pays / personnelle)
- **Q43C/Q43E** : Corruption perçue (police / fonctionnaires)
      ")
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ── Données filtrées (onglet Vue d'ensemble) ────────────────────────────────
  df_filt <- reactive({
    d <- DF
    if (!is.null(input$f_region) && input$f_region != "Toutes")
      d <- d |> dplyr::filter(region_lbl == input$f_region)
    if (!is.null(input$f_milieu) && input$f_milieu != "Tous")
      d <- d |> dplyr::filter(milieu_lbl == input$f_milieu)
    if (!is.null(input$f_sexe) && length(input$f_sexe) > 0)
      d <- d |> dplyr::filter(sexe %in% input$f_sexe)
    if (!is.null(input$f_age) && length(input$f_age) > 0)
      d <- d |> dplyr::filter(as.character(tranche_age) %in% input$f_age)
    d
  })

  # Données conditions de vie
  df_cv <- reactive({
    d <- DF
    if (!is.null(input$cv_region) && input$cv_region != "Toutes")
      d <- d |> dplyr::filter(region_lbl == input$cv_region)
    if (!is.null(input$cv_milieu) && input$cv_milieu != "Tous")
      d <- d |> dplyr::filter(milieu_lbl == input$cv_milieu)
    d
  })

  # Données actifs/emploi
  df_ae <- reactive({
    d <- DF
    if (!is.null(input$ae_region) && input$ae_region != "Toutes")
      d <- d |> dplyr::filter(region_lbl == input$ae_region)
    d
  })

  # Données gouvernance/sécurité
  df_gs <- reactive({
    d <- DF
    if (!is.null(input$gs_region) && input$gs_region != "Toutes")
      d <- d |> dplyr::filter(region_lbl == input$gs_region)
    if (!is.null(input$gs_milieu) && input$gs_milieu != "Tous")
      d <- d |> dplyr::filter(milieu_lbl == input$gs_milieu)
    d
  })

  # ── KPIs ────────────────────────────────────────────────────────────────────
  output$kpi_n      <- renderText(format(nrow(df_filt()), big.mark = " "))
  output$kpi_urbain <- renderText({
    d <- df_filt()
    paste0(round(mean(d$milieu_lbl == "Urbain", na.rm = TRUE) * 100, 1), "%")
  })
  output$kpi_priv <- renderText({
    paste0(round(mean(df_filt()$score_privation, na.rm = TRUE), 2), " / 4")
  })
  output$kpi_actifs <- renderText({
    paste0(round(mean(df_filt()$score_actifs, na.rm = TRUE), 2), " / 6")
  })

  output$ui_n_obs <- renderUI({
    tags$p(style = "color:#4A5568;font-size:0.85rem;",
           icon("filter"), " ", nrow(df_filt()), " observations sélectionnées")
  })

  # ── Vue d'ensemble — plots ──────────────────────────────────────────────────
  output$plot_direction <- renderPlot({
    bar_prop(df_filt(), "direction_pays",
             "Direction générale du pays",
             fill_col = PAL$bleu)
  }, res = 96)

  output$plot_econ <- renderPlot({
    excl <- c("Refuse de répondre [Ne pas lire]", "Je ne sais pas [Ne pas lire]",
              "Refuse de répondre", "Je ne sais pas")
    d <- df_filt() |>
      dplyr::select(econ_pays, econ_perso) |>
      tidyr::pivot_longer(everything(), names_to = "indicateur", values_to = "val") |>
      dplyr::filter(!is.na(val), !as.character(val) %in% excl) |>
      dplyr::mutate(indicateur = ifelse(indicateur == "econ_pays",
                                        "Économie nationale", "Conditions perso.")) |>
      dplyr::count(indicateur, val) |>
      dplyr::group_by(indicateur) |>
      dplyr::mutate(pct = n / sum(n) * 100) |>
      dplyr::ungroup()

    ggplot(d, aes(x = val, y = pct, fill = indicateur)) +
      geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.92) +
      scale_fill_manual(values = c("Économie nationale" = PAL$bleu,
                                    "Conditions perso."  = PAL$vert),
                        name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                         labels = function(x) paste0(x, "%")) +
      labs(title = "Perception économique", caption = CAPTION_STD,
           x = NULL, y = "% des répondants") +
      theme_afro() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }, res = 96)

  output$plot_region <- renderPlot({
    d <- df_filt() |>
      dplyr::filter(!is.na(region_lbl)) |>
      dplyr::count(region_lbl, name = "n") |>
      dplyr::mutate(pct = n / sum(n) * 100)
    ggplot(d, aes(x = reorder(region_lbl, pct), y = pct)) +
      geom_col(fill = PAL$bleu, width = 0.7, alpha = 0.9) +
      geom_text(aes(label = paste0(round(pct, 0), "%")), hjust = -0.1,
                size = 3.2, colour = PAL$gris) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                         labels = function(x) paste0(x, "%")) +
      labs(title = "Répartition par région", caption = CAPTION_STD,
           x = NULL, y = "%") +
      theme_afro()
  }, res = 96)

  output$plot_pyramide <- renderPlot({
    d <- df_filt() |>
      dplyr::filter(!is.na(tranche_age), sexe %in% c("Homme", "Femme")) |>
      dplyr::count(tranche_age, sexe) |>
      dplyr::group_by(sexe) |>
      dplyr::mutate(pct = n / sum(n) * 100,
                    pct_dir = ifelse(sexe == "Homme", -pct, pct)) |>
      dplyr::ungroup()
    ggplot(d, aes(x = tranche_age, y = pct_dir, fill = sexe)) +
      geom_col(width = 0.7, alpha = 0.9) +
      scale_fill_manual(values = c("Homme" = PAL$bleu, "Femme" = PAL$rouge),
                        name = "Sexe") +
      scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
      coord_flip() +
      labs(title = "Pyramide des âges", caption = CAPTION_STD,
           x = NULL, y = "% (Hommes ← | → Femmes)") +
      theme_afro()
  }, res = 96)

  # ── Conditions de vie ───────────────────────────────────────────────────────
  cv_plot <- function(col, titre) {
    renderPlot({
      d <- df_cv()
      grp <- input$cv_group
      if (grp == "none") {
        bar_prop(d, col, titre)
      } else {
        bar_groupe(d, col, grp, titre)
      }
    }, res = 96)
  }

  output$cv_q6a <- cv_plot("Q6A", "Manque de nourriture")
  output$cv_q6b <- cv_plot("Q6B", "Manque d'eau potable")
  output$cv_q6c <- cv_plot("Q6C", "Manque de médicaments")
  output$cv_q6d <- cv_plot("Q6D", "Manque de combustible")
  output$cv_q6e <- cv_plot("Q6E", "Manque de revenus")

  output$cv_priv_region <- renderPlot({
    d <- df_cv() |>
      dplyr::filter(!is.na(region_lbl)) |>
      dplyr::group_by(region_lbl) |>
      dplyr::summarise(priv_moy = mean(score_privation, na.rm = TRUE), .groups = "drop")
    ggplot(d, aes(x = reorder(region_lbl, priv_moy), y = priv_moy)) +
      geom_col(fill = PAL$rouge, alpha = 0.85, width = 0.7) +
      geom_text(aes(label = round(priv_moy, 2)), hjust = -0.1,
                size = 3.2, colour = PAL$gris) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Privation moyenne / 4", caption = CAPTION_STD,
           x = NULL, y = "Score moyen") +
      theme_afro()
  }, res = 96)

  # ── Actifs & Emploi ─────────────────────────────────────────────────────────
  output$ae_emploi <- renderPlot({
    d <- df_ae()
    grp <- input$ae_group
    if (grp == "none") {
      bar_prop(d, "statut_emploi", "Statut d'emploi salarié", fill_col = PAL$vert)
    } else {
      bar_groupe(d, "statut_emploi", grp, "Statut d'emploi salarié")
    }
  }, res = 96)

  output$ae_actifs_region <- renderPlot({
    d <- df_ae() |>
      dplyr::filter(!is.na(region_lbl)) |>
      dplyr::group_by(region_lbl) |>
      dplyr::summarise(actifs_moy = mean(score_actifs, na.rm = TRUE), .groups = "drop")
    ggplot(d, aes(x = reorder(region_lbl, actifs_moy), y = actifs_moy)) +
      geom_col(fill = PAL$bleu, alpha = 0.87, width = 0.7) +
      geom_text(aes(label = round(actifs_moy, 2)), hjust = -0.1,
                size = 3.2, colour = PAL$gris) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, 6)) +
      labs(title = "Score d'actifs moyen / 6", caption = CAPTION_STD,
           x = NULL, y = "Score moyen") +
      theme_afro()
  }, res = 96)

  output$ae_biens <- renderPlot({
    biens <- c("Q90A" = "Radio", "Q90B" = "Télévision", "Q90C" = "Voiture/Moto",
               "Q90D" = "Ordinateur", "Q90E" = "Compte bancaire", "Q90F" = "Téléphone")
    grp <- input$ae_group

    d <- df_ae() |>
      dplyr::select(dplyr::any_of(c(names(biens), grp))) |>
      tidyr::pivot_longer(dplyr::any_of(names(biens)),
                          names_to = "bien", values_to = "val") |>
      dplyr::mutate(
        bien_lbl = biens[bien],
        possede  = as.character(val) %in% c("Oui (en possède personnellement)",
                                             "Quelqu'un d'autre dans le ménage en possède")
      ) |>
      dplyr::filter(!is.na(possede))

    if (grp != "none" && grp %in% names(d)) {
      d2 <- d |>
        dplyr::group_by(bien_lbl, .data[[grp]]) |>
        dplyr::summarise(pct = mean(possede) * 100, .groups = "drop")
      ggplot(d2, aes(x = bien_lbl, y = pct, fill = .data[[grp]])) +
        geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.92) +
        scale_fill_manual(values = PAL_BARS, name = grp) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                           labels = function(x) paste0(x, "%")) +
        labs(title = "Possession de biens (%)", caption = CAPTION_STD,
             x = NULL, y = "%") +
        theme_afro()
    } else {
      d2 <- d |>
        dplyr::group_by(bien_lbl) |>
        dplyr::summarise(pct = mean(possede) * 100, .groups = "drop")
      ggplot(d2, aes(x = reorder(bien_lbl, pct), y = pct)) +
        geom_col(fill = PAL$vert, alpha = 0.88, width = 0.65) +
        geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1,
                  size = 3.8, colour = PAL$gris) +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                           labels = function(x) paste0(x, "%")) +
        labs(title = "Possession de biens (%)", caption = CAPTION_STD,
             x = NULL, y = "% des ménages") +
        theme_afro()
    }
  }, res = 96)

  # ── Gouvernance & Sécurité ──────────────────────────────────────────────────
  output$gs_insec_qrt <- renderPlot(
    bar_prop(df_gs(), "insecurite_quartier",
             "Insécurité dans le quartier", fill_col = PAL$rouge), res = 96)

  output$gs_insec_dom <- renderPlot(
    bar_prop(df_gs(), "insecurite_maison",
             "Insécurité au domicile", fill_col = PAL$rouge), res = 96)

  output$gs_corr_pol <- renderPlot(
    bar_prop(df_gs(), "corrup_police",
             "Corruption policière perçue", fill_col = PAL$gris), res = 96)

  output$gs_corr_fonct <- renderPlot(
    bar_prop(df_gs(), "corrup_fonct",
             "Corruption des fonctionnaires perçue", fill_col = PAL$gris), res = 96)

  # ── Table brute ─────────────────────────────────────────────────────────────
  output$tbl_raw <- DT::renderDataTable({
    cols <- input$tbl_cols
    if (is.null(cols) || length(cols) == 0) cols <- c("region_lbl", "milieu_lbl")
    df_filt() |>
      dplyr::select(dplyr::any_of(cols)) |>
      dplyr::mutate(dplyr::across(everything(), as.character))
  },
  options = list(pageLength = 20, scrollX = TRUE, language = list(
    search = "Rechercher :", paginate = list(previous = "Précédent", `next` = "Suivant"),
    info = "Lignes _START_ à _END_ sur _TOTAL_"
  )),
  filter = "top", class = "stripe hover compact")

  # ── QAQC ────────────────────────────────────────────────────────────────────

  # Taux de NA sur les colonnes clés de la base brute
  qaqc_na <- reactive({
    df_raw <- haven::read_dta(here("input", "base.dta"))
    purrr::map_dfr(names(df_raw), function(v) {
      n_total <- nrow(df_raw)
      n_na    <- sum(is.na(df_raw[[v]]))
      pct_na  <- round(n_na / n_total * 100, 2)
      data.frame(variable = v, n_na = n_na, pct_na = pct_na,
                 statut = ifelse(pct_na == 0, "OK",
                          ifelse(pct_na <= 5, "Faible",
                          ifelse(pct_na <= 20, "Modéré", "Élevé"))),
                 stringsAsFactors = FALSE)
    })
  })

  output$qaqc_n      <- renderText(format(N_TOTAL, big.mark = " "))
  output$qaqc_ncols  <- renderText(ncol(haven::read_dta(here("input","base.dta"))))
  output$qaqc_na_moy <- renderText({
    paste0(round(mean(qaqc_na()$pct_na), 1), "%")
  })

  output$qaqc_controles <- renderTable({
    df_raw <- haven::read_dta(here("input", "base.dta"))

    # Unicité SbjNum
    n_dup <- sum(duplicated(df_raw$SbjNum, incomparables = NA))
    # Plage d'âge (Q1)
    age_num <- suppressWarnings(as.numeric(as.character(df_raw$Q1)))
    n_age_inv <- sum(age_num < 18 | age_num > 120, na.rm = TRUE)
    # Coordonnées GPS valides
    n_gps_ko <- sum(is.na(df_raw$GPS_LA) | is.na(df_raw$GPS_LO))
    # Couverture régions (14 attendues)
    n_regions <- length(unique(df_raw$REGION[!is.na(df_raw$REGION)]))
    # Poids disponible
    has_weight <- "WITHINWT" %in% names(df_raw) &&
                  sum(!is.na(df_raw$WITHINWT)) > 0

    data.frame(
      Contrôle = c("Unicité identifiant (SbjNum)",
                   "Plage d'âge plausible (≥18 ans)",
                   "Coordonnées GPS renseignées",
                   "Couverture régionale (14 régions)",
                   "Variable de pondération (WITHINWT)"),
      Statut = c(
        ifelse(n_dup == 0, "✅ OK", "⚠️ Anomalie"),
        ifelse(n_age_inv == 0, "✅ OK", "⚠️ Anomalie"),
        ifelse(n_gps_ko == 0, "✅ OK", paste0("⚠️ ", n_gps_ko, " manquants")),
        ifelse(n_regions == 14, "✅ OK (14/14)", paste0("⚠️ ", n_regions, "/14")),
        ifelse(has_weight, "✅ Disponible", "⚠️ Absente")
      ),
      Détail = c(
        paste0(n_dup, " doublon(s) détecté(s)"),
        paste0(n_age_inv, " valeur(s) hors plage"),
        paste0(N_TOTAL - n_gps_ko, "/", N_TOTAL, " GPS valides"),
        paste0(n_regions, " région(s) présentes"),
        ifelse(has_weight,
               paste0(sum(!is.na(df_raw$WITHINWT)), " poids non-NA"),
               "Pondération manquante")
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$qaqc_na_table <- DT::renderDataTable({
    qaqc_na() |>
      dplyr::filter(pct_na > 10) |>
      dplyr::arrange(dplyr::desc(pct_na)) |>
      dplyr::select(Variable = variable, `NA (n)` = n_na,
                    `NA (%)` = pct_na, Statut = statut) |>
      head(50)
  }, options = list(pageLength = 10, scrollX = TRUE), class = "compact stripe")

  output$qaqc_na_hist <- renderPlot({
    d <- qaqc_na()
    ggplot(d, aes(x = pct_na)) +
      geom_histogram(binwidth = 5, fill = PAL$bleu, colour = "white", alpha = 0.85) +
      geom_vline(xintercept = 20, colour = PAL$rouge, linetype = "dashed",
                 linewidth = 0.8) +
      annotate("text", x = 21, y = Inf, label = "Seuil 20%",
               hjust = 0, vjust = 1.5, colour = PAL$rouge, size = 3.5) +
      labs(title = "Distribution des taux de valeurs manquantes",
           subtitle = paste0(sum(d$pct_na == 0), " variables sans NA sur ", nrow(d)),
           caption = CAPTION_STD, x = "% de NA", y = "Nb de variables") +
      theme_afro()
  }, res = 96)

  output$qaqc_region_cov <- renderPlot({
    d <- DF |>
      dplyr::filter(!is.na(region_lbl)) |>
      dplyr::count(region_lbl, name = "n") |>
      dplyr::mutate(
        taux = n / N_TOTAL * 100,
        statut = ifelse(n >= 40, "Couverture adéquate", "Faible couverture")
      )
    ggplot(d, aes(x = reorder(region_lbl, n), y = n, fill = statut)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = n), hjust = -0.15, size = 3.3, colour = PAL$gris) +
      scale_fill_manual(values = c("Couverture adéquate" = PAL$vert,
                                    "Faible couverture"   = PAL$rouge),
                        name = NULL) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = "Effectif par région", caption = CAPTION_STD,
           x = NULL, y = "Nombre d'observations") +
      theme_afro()
  }, res = 96)

  # ── Téléchargement ──────────────────────────────────────────────────────────
  output$dl_data <- downloadHandler(
    filename = function() paste0("afrobarometer_sn_filtre_", Sys.Date(), ".csv"),
    content  = function(file) {
      df_filt() |>
        dplyr::mutate(dplyr::across(everything(), as.character)) |>
        write.csv(file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
