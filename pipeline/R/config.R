# ==============================================================================
# CONFIG.R — Configuration centrale du pipeline Afrobarometer Sénégal
# Adapte ce fichier pour chaque nouveau round sans toucher au reste du pipeline.
# ==============================================================================

# ── Packages utilitaires ──────────────────────────────────────────────────────
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# ── Chemins ──────────────────────────────────────────────────────────────────
PATHS <- list(
  input  = here::here("pipeline", "input"),
  output = here::here("pipeline", "output"),
  qaqc   = here::here("pipeline", "output", "qaqc")
)

# ── Identifiant du round ──────────────────────────────────────────────────────
ROUND <- list(
  numero  = 9,
  annee   = 2022,
  pays    = "SEN"
)

# ── Nom du fichier de données brutes ─────────────────────────────────────────
FICHIER_BRUT <- "base.dta"

# ── Colonne identifiant individu ──────────────────────────────────────────────
# SbjNum est l'identifiant unique dans la base Afrobarometer Sénégal Round 9
ID_INDIVIDU <- "SbjNum"

# ── Mapping des variables — DÉMOGRAPHIQUES ────────────────────────────────────
# Basé sur la base réelle : SEN_R9 (1200 obs, 1487 variables)
VARS_DEMO <- list(
  age              = "Q1",       # Age du répondant (numérique continu)
  genre            = "Q100",     # Sexe du Répondant (1=Homme, 2=Femme)
  niveau_etudes    = "Q94",      # Plus haut niveau d'instruction
  # Branche d'études : non présente explicitement dans cette base
  # situation_matrim : non collectée dans ce round Afrobarometer
  # lien_cm : Afrobarometer = 1 répondant / ménage, variable non présente
  langue_domicile  = "Q2"        # Langue parlée à la maison (proxy identité)
)

# ── Mapping des variables — GÉOGRAPHIQUES ─────────────────────────────────────
VARS_GEO <- list(
  region      = "REGION",        # Région (660=Dakar...675=Ziguinchor)
  departement = "Constitutency", # Département/Circonscription
  milieu      = "URBRUR",        # 1=Rural, 2=Urbain
  commune     = "PSU1",          # Ville/Commune
  arrondismt  = "PSU2"           # Arrondissement
)

# ── Mapping des variables — PROFIL EMPLOI ─────────────────────────────────────
VARS_EMPLOI <- list(
  statut_emploi_principal   = "Q93A",      # Travail salarié (0=Non/pas cherche, 1=Non/cherche, 2=Temps partiel, 3=Temps plein)
  activite_principale       = "Q93B_yes",  # Activité principale (secteur -> ISIC)
  activite_secondaire       = "Q93B_2",    # Dernière activité principale
  # Revenus : Afrobarometer mesure la privation de revenus (Q6E), pas le montant
  privation_revenus         = "Q6E"        # Manque de revenus en espèces (0=Jamais...4=Toujours)
)

# ── Mapping des variables — BIENS POSSÉDÉS ────────────────────────────────────
# Q90X = 0 (personne), 1 (autre membre), 2 (répondant personnellement)
VARS_BIENS <- list(
  radio      = "Q90A",   # Radio
  television = "Q90B",   # Télévision
  vehicule   = "Q90C",   # Voiture ou moto
  ordinateur = "Q90D",   # Ordinateur
  telephone  = "Q90F",   # Téléphone portable
  internet   = "Q90G"    # Accès internet sur téléphone
)

# ── Mapping des variables — ACCÈS AUX SERVICES SOCIAUX ────────────────────────
VARS_SERVICES <- list(
  source_eau         = "Q91A",   # Source principale d'eau
  assainissement     = "Q91B",   # Type de latrines
  electricite_acces  = "Q92A",   # Accès à l'électricité (0=Non, 1=Oui)
  electricite_freq   = "Q92B"    # Fréquence disponibilité électricité
)

# ── Mapping des variables — CONDITIONS DE VIE (privations) ────────────────────
# Q6X : Au cours des 12 derniers mois, avez-vous manqué de X ?
# 0=Jamais, 1=1-2 fois, 2=Quelques fois, 3=Plusieurs fois, 4=Toujours
VARS_VIE_MENAGE <- list(
  manque_nourriture  = "Q6A",
  manque_eau         = "Q6B",
  manque_soins       = "Q6C",
  manque_combustible = "Q6D",
  manque_revenus     = "Q6E"
)

# ── Mapping ISIC Rev 4 — codes secteur Afrobarometer → section ISIC ──────────
# Q93B_yes / Q93B_2 : activité principale
ISIC_MAPPING <- data.frame(
  code_afrobarometer = 0:21,
  isic_section = c(
    NA,   # 0 = N'a jamais eu d'emploi
    NA,   # 1 = Elève/étudiant
    NA,   # 2 = Femme au ménage
    "A",  # 3 = Agriculture/ferme/pêche/foresterie
    "G",  # 4 = Commerçant/marchand ambulant/vendeur
    "C",  # 5 = Artisan/métier qualifié
    "O",  # 6 = Fonctionnaire de l'état / gouvernement
    "O",  # 7 = Enseignant / travailleur de la santé (service public)
    "Q",  # 8 = Travailleur du secteur de la santé
    "P",  # 9 = Enseignant
    "T",  # 10 = Employé de maison / travailleur domestique
    "G",  # 11 = Commerce informel de détail
    "F",  # 12 = Construction
    "H",  # 13 = Transport
    "I",  # 14 = Restauration/hôtellerie
    "K",  # 15 = Banque/finance/assurance
    "M",  # 16 = Professions libérales
    "J",  # 17 = Informatique/technologie
    "S",  # 18 = Autres services
    "B",  # 19 = Mines/extractif
    "D",  # 20 = Energie/électricité
    "E"   # 21 = Eau/assainissement/environnement
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

# ── Seuils QAQC ──────────────────────────────────────────────────────────────
SEUILS_QAQC <- list(
  pct_na_alerte     = 0.20,
  pct_na_exclusion  = 0.50,
  seuil_outlier_iqr = 3.0,
  seuil_pvalue_mar  = 0.05
)
