<div align="center">

# SEN AFROBAROMETER PIPELINE | ENSAE Dakar

![Typing SVG](https://readme-typing-svg.herokuapp.com?font=Fira+Code&pause=1000&color=2F81F7&center=true&vCenter=true&width=700&lines=SEN+AFROBAROMETER+PIPELINE%7C+ENSAE+Dakar;Présenté+par:;Ibrahim+ADAM+ALASSANE;Moussa+DIAKITE;Fallou+NGOM;Cheikh+Sadibou+NGOM;Gnalen+SANGARE;Seman+Giovanni+Jocelyn+GADO;Sié+Rachid+TRAORE;INGENIEURS+STATISTICIENS+ECONOMISTES+EN+FIN+DE+FORMATION;Sous+la+supervision+de:;M.MBodj)

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-%3E%3D4.1-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Afrobarometer](https://img.shields.io/badge/données-Afrobarometer%20Round%209-orange)](https://www.afrobarometer.org/countries/senegal/)

</div>

---

## Description du projet

Ce projet s'inscrit dans le cadre de la formation d'ingénieurs statisticiens économistes à l'**ENSAE Dakar**. Il consiste à concevoir un **pipeline complet, reproductible et scalable** de traitement des données Afrobarometer Sénégal (Round 9, 2022).

L'objectif est d'extraire, nettoyer et consolider les données brutes en **deux tables structurées** — individus et ménages — accompagnées d'un **rapport QAQC automatique en HTML**.

---

## Structure du projet

```
SEN-AFROBAROMETER-PIPELINE-ENSAE/
│
├── pipeline/
│   ├── main.R                  ← Point d'entrée unique du pipeline
│   ├── input/                  ← Déposer la base brute ici (base.dta)
│   ├── output/                 ← Tables consolidées et rapport QAQC générés
│   └── R/
│       ├── config.R            ← Mapping variables, ISIC Rev 4, seuils QAQC
│       ├── 01_import.R         ← Import, nettoyage, détection outliers
│       ├── 02_individus.R      ← Table individus consolidée
│       ├── 03_menages.R        ← Table ménages consolidée
│       ├── 04_qaqc.R           ← Contrôle qualité + estimations primaires
│       ├── 05_export.R         ← Export CSV / Excel / HTML
│       └── qaqc_report.Rmd     ← Template rapport QAQC HTML
│
├── Section7.10/                ← Traitement section conditions de vie
├── Section71.81B/              ← Traitement sections médias & influence étrangère
└── Description_et_litige_foncier_et_la_corruption/
```

---

## Installation

### Packages requis

```r
install.packages(c(
  "haven",      # Lecture fichiers Stata (.dta)
  "labelled",   # Gestion des labels Stata
  "dplyr",      # Manipulation de données
  "tidyr",      # Mise en forme
  "purrr",      # Programmation fonctionnelle
  "stringr",    # Traitement de chaînes
  "here",       # Chemins relatifs
  "tibble",     # Tableaux modernes
  # Pour le rapport QAQC HTML (optionnel mais recommandé) :
  "rmarkdown",  # Génération HTML
  "knitr",      # Tricotage
  "kableExtra", # Tableaux enrichis
  "ggplot2",    # Visualisations
  "openxlsx"    # Export Excel
))
```

---

## Utilisation

### 1. Préparer les données

Placer la base brute dans `pipeline/input/` :

```
pipeline/input/base.dta
```

> Formats acceptés : `.dta` (Stata), `.sav` (SPSS), `.csv`

### 2. Lancer le pipeline

```r
# Depuis la racine du projet
source("pipeline/main.R")
```

Ou en ligne de commande :

```bash
Rscript pipeline/main.R
```

### 3. Sorties générées

```
pipeline/output/
├── table_individus_R9_2022.csv    ← Table individus (1 200 lignes)
├── table_menages_R9_2022.csv      ← Table ménages (1 200 lignes)
└── qaqc/
    ├── QAQC_Afrobarometer_R9_2022.html   ← Rapport interactif
    └── QAQC_Afrobarometer_R9_2022.xlsx   ← Rapport Excel colorisé
```

---

## Variables extraites

### Table individus

| Dimension | Variables |
|-----------|-----------|
| **Démographiques** | Âge, genre, niveau d'instruction |
| **Géographiques** | Région (14 régions), département, milieu urbain/rural |
| **Emploi** | Statut d'emploi, secteur d'activité classifié ISIC Rev 4 |
| **Biens possédés** | Radio, TV, véhicule, ordinateur, téléphone, internet + score d'actifs |
| **Services sociaux** | Source d'eau, assainissement, accès à l'électricité |

### Table ménages

| Dimension | Variables |
|-----------|-----------|
| **Profil répondant** | Mêmes variables démographiques |
| **Localisation** | Région, département, milieu, commune |
| **Services zone** | Eau, assainissement, électricité dans la zone de dénombrement |
| **Conditions de vie** | Privations alimentaires, eau, soins, combustible, revenus |
| **Indice de privation** | Score composite (0–5) + groupe de privation |

---

## Rapport QAQC

Le rapport HTML généré automatiquement contient :

- **Vue d'ensemble** : métriques clés avec indicateurs colorisés (vert / orange / rouge)
- **Valeurs manquantes** : taux de NA par variable avec seuils visuels (alerte >20%, critique >50%)
- **Valeurs aberrantes** : détection par méthode IQR (facteur × 3)
- **Contrôles de cohérence** : unicité des identifiants, plages de valeurs
- **Estimations primaires** : distributions genre, éducation, région, milieu, emploi, ISIC Rev 4, actifs, privation

---

## Adapter le pipeline à un nouveau round

Le pipeline est conçu pour être **scalable**. Pour un nouveau round Afrobarometer, il suffit de modifier **uniquement `config.R`** :

```r
# Changer le numéro de round et l'année
ROUND <- list(numero = 10, annee = 2025, pays = "SEN")

# Adapter le nom du fichier source
FICHIER_BRUT <- "base_r10.dta"

# Mettre à jour le mapping des variables si elles ont changé
VARS_DEMO <- list(
  age           = "Q1",
  genre         = "Q100",
  niveau_etudes = "Q94",
  ...
)
```

---

## Source des données

Les données utilisées proviennent du programme **[Afrobarometer](https://www.afrobarometer.org/countries/senegal/)**, un programme de recherche panafricain qui réalise des enquêtes d'opinion auprès des citoyens sur :

- la démocratie et la gouvernance
- les conditions de vie
- les services publics

**Round 9 Sénégal (2022)** : 1 200 répondants, 1 487 variables.

---

## Équipe

| Nom | Rôle |
|-----|------|
| Ibrahim ADAM ALASSANE | Ingénieur Statisticien Économiste |
| Moussa DIAKITE | Ingénieur Statisticien Économiste |
| Fallou NGOM | Ingénieur Statisticien Économiste |
| Cheikh Sadibou NGOM | Ingénieur Statisticien Économiste |
| Gnalen SANGARE | Ingénieur Statisticien Économiste |
| Seman Giovanni Jocelyn GADO | Ingénieur Statisticien Économiste |
| Sié Rachid TRAORE | Ingénieur Statisticien Économiste |

**Superviseur :** M. MBodj — ENSAE Dakar

---

<div align="center">
  <sub>ENSAE Dakar · Formation ISE · 2022–2025</sub>
</div>
