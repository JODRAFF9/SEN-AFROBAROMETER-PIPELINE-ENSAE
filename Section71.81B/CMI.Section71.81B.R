# ============================================================
# PIPELINE DE TRAITEMENT - TROIS SECTIONS THÉMATIQUES
# Source  : Base Afrobarometer Sénégal (base.dta)
# Sections: Conditions environnementales | Médias | Influence pays étrangers
# ============================================================


# ============================================================
# 0. CHARGEMENT DES PACKAGES
# ============================================================
library(haven)       # Lecture des fichiers Stata (.dta)
library(dplyr)       # Manipulation de données (select, mutate, filter…)
library(tidyr)       # Mise en forme longue/large (pivot_longer…)
library(purrr)       # Programmation fonctionnelle (map, walk…)
library(ggplot2)     # Visualisation
library(labelled)    # Gestion des labels Stata (to_factor, val_labels…)
library(janitor)     # Nettoyage des noms et tableaux croisés (tabyl…)
library(questionr)   # Tableaux de fréquences enrichis (freq…)
library(openxlsx)    # Export Excel sans dépendance Java


# ============================================================
# 1. IMPORTATION DE LA BASE
# ============================================================
base <- read_dta("Base/base.dta")


# ============================================================
# 2. DÉFINITION DES FONCTIONS
# ============================================================

# ------------------------------------------------------------
# is_id(x)
# Vérifie qu'une colonne peut servir d'identifiant unique.
# Conditions : toutes les valeurs non-NA sont entières ET distinctes.
# Retourne TRUE/FALSE.
# ------------------------------------------------------------
is_id <- function(x) {
  vals <- x[!is.na(x)]                          # on retire les NA avant de tester
  length(unique(vals)) == length(vals) &&        # toutes les valeurs sont uniques
    all(vals %% 1 == 0)                          # toutes les valeurs sont entières
}


# ------------------------------------------------------------
# extraire_section(base, id_col, cols)
# Extrait un sous-ensemble de colonnes de la base principale.
#   base    : data.frame source
#   id_col  : indice (entier) de la colonne identifiant à conserver
#   cols    : vecteur d'indices des variables thématiques à extraire
# Retourne un data.frame réduit.
# ------------------------------------------------------------
extraire_section <- function(base, id_col, cols) {
  base[, c(id_col, cols)]
}


# ------------------------------------------------------------
# convertir_labels(df)
# Convertit toutes les variables labellisées (format Stata/haven)
# en facteurs R lisibles. Nécessaire avant toute analyse
# tabulaire ou graphique.
#   df : data.frame issu de read_dta()
# Retourne un data.frame avec les mêmes colonnes converties en facteur.
# ------------------------------------------------------------
convertir_labels <- function(df) {
  df |> mutate(across(where(is.labelled), to_factor))
}


# ------------------------------------------------------------
# recoder_manquants(df)
# Recode en NA les modalités correspondant aux non-réponses
# standard du questionnaire Afrobarometer :
#   - "Refuse de répondre [Ne pas lire]"
#   - "Je ne sais pas [Ne pas lire]"
#   - "Ne sait pas / N'en a pas suffisament entendu parlé [Ne sait pas]"
#   - "Non applicable [Ne pas lire]"
# Ces codes (8, 9, 7 selon les variables) doivent être exclus
# des distributions avant toute analyse.
#   df : data.frame avec des colonnes de type factor
# Retourne le data.frame avec les non-réponses remplacées par NA.
# ------------------------------------------------------------
recoder_manquants <- function(df) {
  modalites_manquantes <- c(
    "Refuse de répondre [Ne pas lire]",
    "Je ne sais pas [Ne pas lire]",
    "Ne sait pas / N'en a pas suffisament entendu parlé [Ne sait pas]",
    "Non applicable [Ne pas lire]"
  )
  df |> mutate(across(
    where(is.factor),
    ~ factor(ifelse(as.character(.) %in% modalites_manquantes, NA, as.character(.)))
  ))
}


# ------------------------------------------------------------
# tableau_freq(df, id_var)
# Calcule un tableau de fréquences (effectifs + pourcentages)
# pour toutes les variables d'un data.frame, en excluant
# la colonne identifiant.
#   df     : data.frame nettoyé (facteurs, NA recodés)
#   id_var : nom (chaîne) de la colonne identifiant à exclure
# Retourne un data.frame long avec les colonnes :
#   Variable | Modalité | Effectif | Pourcentage
# ------------------------------------------------------------
tableau_freq <- function(df, id_var = "SbjNum") {
  df |>
    select(-all_of(id_var)) |>             # on retire l'identifiant
    names() |>
    lapply(function(varname) {
      var <- df[[varname]]
      if (!is.factor(var)) var <- as.factor(var)
      tab <- table(var, useNA = "ifany")
      pct <- prop.table(tab) * 100
      data.frame(
        Variable    = varname,
        Modalite    = names(tab),
        Effectif    = as.vector(tab),
        Pourcentage = round(as.vector(pct), 2)
      )
    }) |>
    bind_rows()
}


# ------------------------------------------------------------
# exporter_excel(freq_df, chemin, nom_onglet)
# Exporte un tableau de fréquences dans un fichier Excel.
#   freq_df   : data.frame produit par tableau_freq()
#   chemin    : chemin complet du fichier de sortie (.xlsx)
#   nom_onglet: nom de l'onglet dans le classeur (défaut : "Fréquences")
# Crée le fichier et affiche un message de confirmation.
# ------------------------------------------------------------
exporter_excel <- function(freq_df, chemin, nom_onglet = "Fréquences") {
  wb <- createWorkbook()
  addWorksheet(wb, nom_onglet)
  writeData(wb, nom_onglet, freq_df)
  saveWorkbook(wb, chemin, overwrite = TRUE)
  message("Exporté : ", chemin)
}


# ------------------------------------------------------------
# grapher_freq(df, varname, titre)
# Produit un graphique en barres horizontales (ggplot2)
# pour une variable catégorielle, trié par pourcentage décroissant.
# Les non-réponses (NA) sont exclues du graphique.
#   df      : data.frame contenant la variable
#   varname : nom de la variable (chaîne)
#   titre   : titre du graphique (optionnel, défaut = varname)
# Retourne un objet ggplot.
# ------------------------------------------------------------
grapher_freq <- function(df, varname, titre = NULL) {
  label <- if (!is.null(titre)) titre else varname
  tab <- as.data.frame(table(df[[varname]], useNA = "no")) |>
    rename(Modalite = Var1, Effectif = Freq) |>
    mutate(Pourcentage = round(Effectif / sum(Effectif) * 100, 1))
  
  ggplot(tab, aes(x = reorder(Modalite, Pourcentage), y = Pourcentage)) +
    geom_bar(stat = "identity", fill = "#2C7BB6") +
    geom_text(aes(label = paste0(Pourcentage, "%")), hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = label, x = NULL, y = "Pourcentage (%)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))
}


# ------------------------------------------------------------
# diagnostiquer_nr(liste_df, id_var)
# Calcule le taux de non-réponse (NA) pour chaque variable
# à travers une liste de data.frames nettoyés.
# Utile pour identifier les questions les plus sensibles
# ou les plus mal renseignées.
#   liste_df : liste nommée de data.frames (après recoder_manquants)
#   id_var   : nom de la colonne identifiant à exclure
# Retourne un data.frame trié par taux de NA décroissant :
#   Variable | Taux_NA_pct
# ------------------------------------------------------------
diagnostiquer_nr <- function(liste_df, id_var = "SbjNum") {
  bind_rows(lapply(liste_df, function(df) df |> select(-all_of(id_var)))) |>
    summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 2))) |>
    pivot_longer(everything(), names_to = "Variable", values_to = "Taux_NA_pct") |>
    arrange(desc(Taux_NA_pct))
}


# ============================================================
# 3. VALIDATION DE L'IDENTIFIANT
# ============================================================

# On vérifie que SbjNum est bien un identifiant unique avant
# de commencer tout traitement. Si ce n'est pas le cas,
# stopifnot() interrompt l'exécution avec un message explicite.
stopifnot("SbjNum n'est pas un identifiant unique !" = is_id(base$SbjNum))
message("SbjNum validé comme identifiant unique.")


# ============================================================
# 4. EXTRACTION DES TROIS SECTIONS
# ============================================================
# Indices de colonnes (base R, commence à 1) :
#   Section 1 — Conditions environnementales : cols 1191–1205 → Q71_1 à Q73D
#   Section 2 — Médias et information        : cols 1206–1214 → Q74A à Q76C
#   Section 3 — Influence des pays étrangers : cols 1215–1236 → Q77 à Q81B_SEN_C

sub_df1 <- extraire_section(base, id_col = 1, cols = 1191:1205)
sub_df2 <- extraire_section(base, id_col = 1, cols = 1206:1214)
sub_df3 <- extraire_section(base, id_col = 1, cols = 1215:1236)


# ============================================================
# 5. CONVERSION DES LABELS EN FACTEURS
# ============================================================

# Les variables importées depuis Stata sont de type <labelled>.
# On les convertit en facteurs R pour pouvoir les analyser
# avec table(), ggplot2, etc.
label_df1 <- convertir_labels(sub_df1)
label_df2 <- convertir_labels(sub_df2)
label_df3 <- convertir_labels(sub_df3)


# ============================================================
# 6. RECODAGE DES NON-RÉPONSES EN NA
# ============================================================

# Les codes 7 (Non applicable), 8 (Refuse) et 9 (Ne sait pas)
# sont exclus des analyses de distribution.
# Ils sont remplacés par NA à cette étape.
clean_df1 <- recoder_manquants(label_df1)
clean_df2 <- recoder_manquants(label_df2)
clean_df3 <- recoder_manquants(label_df3)


# ============================================================
# 7. TABLEAUX DE FRÉQUENCES
# ============================================================

# Section 1 — Conditions environnementales
# (Q71_1, Q71_2, Q71, Q72A–Q72E, Q73A–Q73D)
freq_s1 <- tableau_freq(clean_df1, id_var = "SbjNum")
cat("\n====== SECTION 1 : Conditions environnementales ======\n")
print(freq_s1)

# Section 2 — Médias et sources d'information
# (Q74A Radio, Q74B TV, Q74C Presse, Q74D Internet, Q74E Réseaux sociaux, Q75, Q76A–Q76C)
freq_s2 <- tableau_freq(clean_df2, id_var = "SbjNum")
cat("\n====== SECTION 2 : Médias et sources d'information ======\n")
print(freq_s2)

# Section 3 — Influence des pays étrangers au Sénégal
# (Q77, Q78A–Q78F, Q79A_SEN, Q79B_SEN, Q80A_SEN–Q80E_SEN, Q81A_SEN, Q81B_SEN_A–C)
freq_s3 <- tableau_freq(clean_df3, id_var = "SbjNum")
cat("\n====== SECTION 3 : Influence des pays étrangers ======\n")
print(freq_s3)


# ============================================================
# 8. EXPORTS EXCEL
# ============================================================
dir.create("Outputs", showWarnings = FALSE)

# Un fichier par section
exporter_excel(freq_s1, "Outputs/freq_section1_environnement.xlsx", "Environnement")
exporter_excel(freq_s2, "Outputs/freq_section2_medias.xlsx",        "Médias")
exporter_excel(freq_s3, "Outputs/freq_section3_influence_pays.xlsx","Influence_pays")

# Fichier groupé avec un onglet par section
wb_all <- createWorkbook()
addWorksheet(wb_all, "Section1_Environnement")
addWorksheet(wb_all, "Section2_Médias")
addWorksheet(wb_all, "Section3_Influence_pays")
writeData(wb_all, "Section1_Environnement",  freq_s1)
writeData(wb_all, "Section2_Médias",         freq_s2)
writeData(wb_all, "Section3_Influence_pays", freq_s3)
saveWorkbook(wb_all, "Outputs/freq_toutes_sections.xlsx", overwrite = TRUE)
message("Exporté : Outputs/freq_toutes_sections.xlsx")


# ============================================================
# 9. VISUALISATIONS GRAPHIQUES
# ============================================================

# --- Section 2 : exposition aux médias (Q74A–Q74E) ---
# On itère sur les 5 variables média avec leurs titres lisibles
medias_vars   <- c("Q74A",  "Q74B",        "Q74C",         "Q74D",     "Q74E")
medias_titres <- c("Radio", "Télévision",  "Presse écrite","Internet", "Réseaux sociaux")

plots_medias <- map2(medias_vars, medias_titres, function(v, t) {
  grapher_freq(clean_df2, v, t)
})

pdf("Outputs/graphiques_medias.pdf", width = 10, height = 6)
walk(plots_medias, print)
dev.off()
message("Exporté : Outputs/graphiques_medias.pdf")


# --- Section 3 : image des pays étrangers (Q78A–Q78E_SEN) ---
# On itère sur les 5 pays couverts par le questionnaire
pays_vars   <- c("Q78A",  "Q78B",        "Q78C",  "Q78D_SEN", "Q78E_SEN")
pays_titres <- c("Chine", "États-Unis",  "Japon", "France",   "Russie")

plots_pays <- map2(pays_vars, pays_titres, function(v, t) {
  grapher_freq(clean_df3, v, paste("Image :", t))
})

pdf("Outputs/graphiques_image_pays.pdf", width = 10, height = 6)
walk(plots_pays, print)
dev.off()
message("Exporté : Outputs/graphiques_image_pays.pdf")


# --- Section 1 : gravité du problème de pollution (Q72A) ---
p_env <- grapher_freq(
  clean_df1,
  "Q72A",
  "Q72A. Gravité de la pollution (accumulation de déchets, etc.)"
)
ggsave("Outputs/graph_Q72A_gravite_pollution.png", p_env,
       width = 9, height = 5, dpi = 150)
message("Exporté : Outputs/graph_Q72A_gravite_pollution.png")


# ============================================================
# 10. DIAGNOSTIC DES NON-RÉPONSES
# ============================================================

# On agrège les trois sections nettoyées pour mesurer le taux
# de NA par variable. Cela permet de repérer les questions
# les plus sensibles ou les plus souvent éludées.
taux_nr <- diagnostiquer_nr(
  liste_df = list(clean_df1, clean_df2, clean_df3),
  id_var   = "SbjNum"
)

cat("\n====== TOP 10 — Variables avec le plus de non-réponses ======\n")
print(head(taux_nr, 10))


# ============================================================
# 11. RÉSUMÉ FINAL EN CONSOLE
# ============================================================
cat("\n", strrep("=", 60), "\n")
cat("RÉSUMÉ DU PIPELINE\n")
cat(strrep("=", 60), "\n")
cat(sprintf("  Section 1 – Environnement  : %d variables, %d observations\n",
            ncol(clean_df1) - 1, nrow(clean_df1)))
cat(sprintf("  Section 2 – Médias         : %d variables, %d observations\n",
            ncol(clean_df2) - 1, nrow(clean_df2)))
cat(sprintf("  Section 3 – Influence pays : %d variables, %d observations\n",
            ncol(clean_df3) - 1, nrow(clean_df3)))
cat(strrep("=", 60), "\n\n")
