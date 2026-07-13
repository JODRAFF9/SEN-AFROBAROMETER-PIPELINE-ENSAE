# ==============================================================================
# CONFIG.R — Configuration centrale du pipeline Afrobarometer Sénégal
# Pour un nouveau round : mettre à jour ROUND, FICHIER_BRUT, et remplir
# la colonne "Variable nouveau round" dans input/variables_mapping.xlsx.
# ==============================================================================

# ── Packages utilitaires ──────────────────────────────────────────────────────
if (!requireNamespace("here",    quietly = TRUE)) install.packages("here")
if (!requireNamespace("readxl",  quietly = TRUE)) install.packages("readxl")
library(here)

# ── Chemins ──────────────────────────────────────────────────────────────────
PATHS <- list(
  input  = here::here("input"),
  output = here::here("output"),
  qaqc   = here::here("output", "qaqc")
)

# ── Identifiant du round ──────────────────────────────────────────────────────
ROUND <- list(
  numero  = 9,
  annee   = 2022,
  pays    = "SEN"
)

# ── Nom du fichier de données brutes ─────────────────────────────────────────
FICHIER_BRUT <- "base.dta"

# ==============================================================================
# LECTURE DU FICHIER DE MAPPING (input/variables_mapping.xlsx)
# La colonne "Variable nouveau round" prend la priorité sur la colonne
# "Variable round actuel" quand elle est renseignée.
# ==============================================================================
.chemin_mapping <- file.path(PATHS$input, "variables_mapping.xlsx")

if (!file.exists(.chemin_mapping)) {
  stop("Fichier de mapping introuvable : ", .chemin_mapping,
       "\nCréez-le depuis le script utilitaire ou replacez-le dans input/.")
}

.mapping_vars <- readxl::read_excel(.chemin_mapping, sheet = "Variables",
                                     col_types = "text")
.mapping_mod  <- readxl::read_excel(.chemin_mapping, sheet = "Modalités",
                                     col_types = "text")

# Résoudre le nom effectif de chaque variable :
# si "Variable nouveau round" est renseigné → on l'utilise, sinon on garde l'actuel
.mapping_vars$var_effective <- ifelse(
  !is.na(.mapping_vars[["Variable nouveau round  ✏️"]]) &
    nchar(trimws(.mapping_vars[["Variable nouveau round  ✏️"]])) > 0,
  trimws(.mapping_vars[["Variable nouveau round  ✏️"]]),
  trimws(.mapping_vars[["Variable round actuel (R9)"]])
)

# Fonction helper interne : retrouve la variable effective par nom pipeline
.v <- function(nom_pipeline) {
  idx <- which(.mapping_vars[["Nom pipeline (R)"]] == nom_pipeline)
  if (length(idx) == 0) stop("Variable introuvable dans le mapping : ", nom_pipeline)
  toupper(.mapping_vars$var_effective[idx[1]])
}

# ── Identifiant individu ──────────────────────────────────────────────────────
ID_INDIVIDU <- .v("id_individu")

# ── Mapping des variables par groupe ─────────────────────────────────────────
VARS_DEMO <- list(
  age             = .v("age"),
  genre           = .v("genre"),
  niveau_etudes   = .v("niveau_etudes"),
  langue_domicile = .v("langue_domicile")
)

VARS_GEO <- list(
  region      = .v("region"),
  departement = .v("departement"),
  milieu      = .v("milieu"),
  commune     = .v("commune"),
  arrondismt  = .v("arrondismt")
)

VARS_EMPLOI <- list(
  statut_emploi_principal = .v("statut_emploi_principal"),
  activite_principale     = .v("activite_principale"),
  activite_secondaire     = .v("activite_secondaire"),
  privation_revenus       = .v("privation_revenus")
)

VARS_BIENS <- list(
  radio      = .v("radio"),
  television = .v("television"),
  vehicule   = .v("vehicule"),
  ordinateur = .v("ordinateur"),
  telephone  = .v("telephone"),
  internet   = .v("internet")
)

VARS_SERVICES <- list(
  source_eau        = .v("source_eau"),
  assainissement    = .v("assainissement"),
  electricite_acces = .v("electricite_acces"),
  electricite_freq  = .v("electricite_freq")
)

VARS_VIE_MENAGE <- list(
  manque_nourriture  = .v("manque_nourriture"),
  manque_eau         = .v("manque_eau"),
  manque_soins       = .v("manque_soins"),
  manque_combustible = .v("manque_combustible"),
  manque_revenus     = .v("manque_revenus")
)

# ── Mapping ISIC Rev 4 — codes secteur Afrobarometer → section ISIC ──────────
ISIC_MAPPING <- data.frame(
  code_afrobarometer = 0:21,
  isic_section = c(
    NA, NA, NA,
    "A", "G", "C", "O", "O", "Q", "P", "T",
    "G", "F", "H", "I", "K", "M", "J", "S", "B", "D", "E"
  ),
  isic_libelle = c(
    "Non applicable (sans emploi)",
    "Non applicable (étudiant)",
    "Non applicable (femme au foyer)",
    "A - Agriculture, sylviculture et pêche",
    "G - Commerce; réparation d'automobiles",
    "C - Industrie manufacturière",
    "O - Administration publique et défense",
    "O - Administration publique et défense",
    "Q - Santé humaine et action sociale",
    "P - Enseignement",
    "T - Ménages en tant qu'employeurs",
    "G - Commerce; réparation d'automobiles",
    "F - Construction",
    "H - Transport et entreposage",
    "I - Hébergement et restauration",
    "K - Activités financières et d'assurance",
    "M - Activités spécialisées, scientifiques et techniques",
    "J - Information et communication",
    "S - Autres activités de services",
    "B - Industries extractives",
    "D - Production et distribution d'énergie",
    "E - Distribution d'eau; gestion des déchets"
  ),
  stringsAsFactors = FALSE
)

# ── Codes numériques de non-réponse → NA ─────────────────────────────────────
CODES_MANQUANTS_NUM <- c(7, 8, 9, 97, 98, 99, 997, 998, 999, 9995, 9997, 9998, 9999, -1, -99)

# ── Labels de non-réponse → NA ───────────────────────────────────────────────
CODES_MANQUANTS_LABEL <- c(
  "Don't know", "Refused", "Missing", "Not asked", "Not applicable",
  "Ne sait pas", "Refusé", "Manquant", "Non posée", "Non applicable"
)

# ── Seuils QAQC ──────────────────────────────────────────────────────────────
SEUILS_QAQC <- list(
  pct_na_alerte     = 0.20,
  pct_na_exclusion  = 0.50,
  seuil_outlier_iqr = 3.0,
  seuil_pvalue_mar  = 0.05
)
