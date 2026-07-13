<img src="https://capsule-render.vercel.app/api?type=waving&height=220&color=0:009A44,50:FDEF42,100:E31B23&text=SEN%20AFROBAROMETER%20PIPELINE&fontSize=36&fontColor=ffffff&fontAlignY=38&desc=Pipeline%20de%20donn%C3%A9es%20d%E2%80%99opinion%20%7C%20ENSAE%20Dakar&descSize=16&descAlignY=58&animation=fadeIn&section=header" width="100%"/>

<div align="center">

![Typing SVG](https://readme-typing-svg.herokuapp.com?font=Fira+Code&pause=1000&color=E31B23&center=true&vCenter=true&width=700&lines=SEN+AFROBAROMETER+PIPELINE+%7C+ENSAE+Dakar;Ibrahim+ADAM+ALASSANE;Moussa+DIAKITE;Fallou+NGOM;Cheikh+Sadibou+NGOM;Gnalen+SANGARE;Seman+Giovanni+Jocelyn+GADO;Sie+Rachid+TRAORE;ISE+EN+FIN+DE+FORMATION+%7C+Superviseur+%3A+M.+MBodj)

<br/>

[![License](https://img.shields.io/badge/licence-MIT-009A44?style=for-the-badge)](LICENSE)
[![R](https://img.shields.io/badge/R-%3E%3D%204.1-E31B23?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Afrobarometer](https://img.shields.io/badge/Afrobarometer-Round%209%20%C2%B7%202022-FDEF42?style=for-the-badge&logoColor=black&labelColor=009A44)](https://www.afrobarometer.org/countries/senegal/)
[![ENSAE](https://img.shields.io/badge/ENSAE-Dakar-009A44?style=for-the-badge)](https://www.ensae.sn/)

</div>

<img src="https://capsule-render.vercel.app/api?type=soft&height=6&color=0:009A44,50:FDEF42,100:E31B23" width="100%"/>

## A propos

Ce projet, realise dans le cadre de la formation **ISE (Ingenieurs Statisticiens Economistes)** a l'ENSAE Dakar, fournit un **pipeline reproductible et scalable** de traitement des donnees Afrobarometer Senegal.

Il produit deux tables analytiques structurees (**individus** et **menages**), des estimations ponderees avec intervalles de confiance, des indicateurs composites de bien-etre et de vulnerabilite, ainsi qu'un **rapport QAQC interactif en HTML**.

<img src="https://capsule-render.vercel.app/api?type=soft&height=6&color=0:E31B23,50:FDEF42,100:009A44" width="100%"/>

## Schema du pipeline

```mermaid
flowchart TD
    RAW[(📁 input/base.dta\nBase brute Afrobarometer)]
    MAP[(📊 variables_mapping.xlsx\nMapping variables et modalites)]

    subgraph CONFIG ["⚙️ Configuration"]
        C[config.R\nResolution des noms de variables\nParametres round et seuils QAQC]
    end

    MAP --> C
    RAW --> STEP1

    subgraph STEP1 ["01 - Import"]
        I1[Lecture .dta / .sav / .csv]
        I2[Harmonisation noms snake_case]
        I3[Recodage non-reponses en NA]
        I4[Detection outliers IQR x3]
        I1 --> I2 --> I3 --> I4
    end

    C --> STEP1

    STEP1 --> STEP2
    STEP1 --> STEP3

    subgraph STEP2 ["02 - Table individus"]
        T1[Selection et renommage des variables]
        T2[Classification ISIC Rev 4]
        T3[Score d'actifs composite]
        T1 --> T2 --> T3
    end

    subgraph STEP3 ["03 - Table menages"]
        M1[Variables geographiques et services]
        M2[Privations conditions de vie]
        M3[Indice de privation 0-5]
        M1 --> M2 --> M3
    end

    STEP2 --> STEP4
    STEP3 --> STEP4

    subgraph STEP4 ["04 - Controle qualite QAQC"]
        Q1[Taux de NA par variable]
        Q2[Controles unicite et coherence]
        Q3[Estimations primaires]
        Q4[Rapport HTML interactif]
        Q1 --> Q2 --> Q3 --> Q4
    end

    STEP4 --> STEP5

    subgraph STEP5 ["05 - Ponderation"]
        P1[Detection variable de poids WITHINWT]
        P2[Application poids aux deux tables]
        P3[Proportions ponderees + IC 95%]
        P1 --> P2 --> P3
    end

    STEP5 --> STEP6

    subgraph STEP6 ["06 - Analyse thematique"]
        A1[Indice de bien-etre 0-100\nActifs + Services + Privation]
        A2[Segmentation vulnerabilite\n4 segments : Resilient a Tres vulnerable]
        A3[Tableaux croises ponderes\nGenre x Education, Milieu x Emploi]
        A4[Profil regional synthetique]
        A1 --> A2 --> A3 --> A4
    end

    STEP6 --> STEP7
    STEP6 --> STEP8

    subgraph STEP7 ["07 - Export"]
        E1[CSV individus et menages]
        E2[Excel QAQC colore]
        E3[Rapport HTML QAQC]
        E1 & E2 & E3
    end

    subgraph STEP8 ["08 - Cartographie"]
        C1[Chargement shapefile GADM Senegal]
        C2[Jointure regions x indicateurs]
        C3[6 cartes choroplethes\nBien-etre, Privation, Vulnerabilite\nActifs, Urbanisation, Emploi]
        C4[Panneau multi-cartes PNG]
        C1 --> C2 --> C3 --> C4
    end

    STEP7 --> OUT[(📂 output/\nTables CSV\nRapport QAQC HTML\nRapport Excel)]
    STEP8 --> MAPS[(📂 output/cartes/\n6 cartes PNG\nPanneau complet)]

    style CONFIG fill:#EBF2FA,stroke:#1A3A5C,color:#1A3A5C
    style STEP1  fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
    style STEP2  fill:#FFF3E0,stroke:#EF6C00,color:#EF6C00
    style STEP3  fill:#FFF3E0,stroke:#EF6C00,color:#EF6C00
    style STEP4  fill:#F3E5F5,stroke:#7B1FA2,color:#7B1FA2
    style STEP5  fill:#E0F7FA,stroke:#00838F,color:#00838F
    style STEP6  fill:#FCE4EC,stroke:#AD1457,color:#AD1457
    style STEP7  fill:#FBE9E7,stroke:#BF360C,color:#BF360C
    style STEP8  fill:#F1F8E9,stroke:#558B2F,color:#33691E
```

<img src="https://capsule-render.vercel.app/api?type=soft&height=6&color=0:009A44,50:FDEF42,100:E31B23" width="100%"/>

## Structure du projet

```
SEN-AFROBAROMETER-PIPELINE-ENSAE/
|
+-- main.R                        <- Point d'entree unique du pipeline (8 etapes)
+-- install_packages.R            <- Script d'installation des dependances
|
+-- input/
|   +-- base.dta                  <- Base brute Afrobarometer (a deposer ici)
|   +-- variables_mapping.xlsx    <- Mapping variables et modalites (editable par round)
|
+-- output/                       <- Generes automatiquement a l'execution
|   +-- qaqc/
|   +-- cartes/                   <- Cartes PNG generees par 08_cartographie.R
|
+-- R/
    +-- config.R                  <- Lit le mapping Excel, configure le pipeline
    +-- utils.R                   <- Fonctions utilitaires partagees
    +-- 01_import.R               <- Import, nettoyage, detection outliers
    +-- 02_individus.R            <- Table individus consolidee
    +-- 03_menages.R              <- Table menages consolidee
    +-- 04_qaqc.R                 <- Controle qualite et estimations primaires
    +-- 05_ponderation.R          <- Poids d'enquete et estimations ponderees IC 95%
    +-- 06_analyse.R              <- Indices composites, vulnerabilite, tableaux croises
    +-- 07_export.R               <- Export CSV / Excel / HTML
    +-- 08_cartographie.R         <- Cartes choroplethes par region (sf + ggplot2)
    +-- qaqc_report.Rmd           <- Template rapport QAQC HTML
```

<img src="https://capsule-render.vercel.app/api?type=soft&height=6&color=0:E31B23,50:FDEF42,100:009A44" width="100%"/>

## Demarrage rapide

### Etape 1 - Installer les packages R

Lancer le script d'installation fourni (gere les dependances systeme de `sf`) :

```r
source("install_packages.R")
```

```bash
# ou en ligne de commande
Rscript install_packages.R
```

Le script installe automatiquement les 3 groupes de packages, verifie les
dependances systeme GDAL/GEOS/PROJ necessaires pour la cartographie, et
affiche un bilan clair. Les packages de cartographie (`sf`, `geodata`,
`patchwork`) sont optionnels : si leur installation echoue, le pipeline
continue sans l'etape 08.

> **Linux / WSL** : si `sf` echoue, installer d'abord les libs systeme :
> ```bash
> sudo apt-get install -y libgdal-dev libgeos-dev libproj-dev
> ```
> puis relancer `install_packages.R`.

### Etape 2 - Deposer la base brute

```
input/base.dta        <- formats acceptes : .dta / .sav / .csv
```

### Etape 3 - Lancer le pipeline

```r
source("main.R")
```

```bash
# ou en ligne de commande
Rscript main.R
```

### Resultats generes

```
output/
+-- table_individus_R9_2022.csv          <- 1 200 individus x variables enrichies
+-- table_menages_R9_2022.csv            <- 1 200 menages x conditions de vie
+-- qaqc/
|   +-- QAQC_Afrobarometer_R9_2022.html <- Rapport interactif complet
|   +-- QAQC_Afrobarometer_R9_2022.xlsx <- Rapport Excel colorise
+-- cartes/
    +-- carte_bien_etre.png              <- Indice de bien-etre par region
    +-- carte_privation.png              <- Indice de privation par region
    +-- carte_vulnerabilite.png          <- Taux de vulnerabilite severe
    +-- carte_actifs.png                 <- Score d'actifs par region
    +-- carte_urbanisation.png           <- Taux d'urbanisation
    +-- carte_emploi.png                 <- Precarite de l'emploi
    +-- carte_panneau_complet.png        <- Toutes les cartes assemblees
```

<img src="https://capsule-render.vercel.app/api?type=soft&height=6&color=0:009A44,50:FDEF42,100:E31B23" width="100%"/>

## Variables produites

<details>
<summary><b>📋 Table individus</b></summary>

| Groupe | Variables |
|--------|-----------|
| 👤 **Demographiques** | Age, genre, niveau d'instruction, langue du domicile |
| 🗺️ **Geographiques** | Region (14), departement, milieu urbain/rural, commune |
| 💼 **Emploi** | Statut d'emploi, secteur ISIC Rev 4, activite principale et secondaire |
| 🏠 **Biens possedes** | Radio, TV, vehicule, ordinateur, telephone, internet, score d'actifs |
| 💧 **Services sociaux** | Source d'eau, assainissement, acces et frequence de l'electricite |
| ⚖️ **Poids d'enquete** | Variable `poids` issue de WITHINWT (ou poids unitaire si absente) |
| 📊 **Indices calcules** | Indice de bien-etre (0-100), score de vulnerabilite, segment de vulnerabilite |

</details>

<details>
<summary><b>📋 Table menages</b></summary>

| Groupe | Variables |
|--------|-----------|
| 👤 **Profil repondant** | Memes variables demographiques |
| 🗺️ **Localisation** | Region, departement, milieu, commune, arrondissement |
| 💧 **Services zone** | Eau, assainissement, electricite dans la zone |
| 📉 **Conditions de vie** | Privations alimentation, eau, soins, combustible, revenus |
| 📊 **Indice de privation** | Score composite 0-5 et groupe de privation |
| ⚖️ **Poids d'enquete** | Variable `poids` issue de WITHINWT |

</details>

<details>
<summary><b>📊 Sorties analytiques avancees</b></summary>

| Sortie | Description |
|--------|-------------|
| Estimations ponderees | Proportions + IC 95% pour genre, region, emploi, ISIC |
| Indice de bien-etre | Score composite 0-100 (actifs + services + privation) |
| Segmentation vulnerabilite | 4 segments : Resilient, Vulnerable modere, Vulnerable, Tres vulnerable |
| Tableaux croises ponderes | Genre x Education, Milieu x Emploi, Region x Vulnerabilite |
| Profil regional | Tableau synthetique par region (privation, emploi, actifs, urbanisation) |
| Cartes choroplethes | 6 cartes PNG + panneau assemble (fond GADM, geom_sf, patchwork) |

</details>

<img src="https://capsule-render.vercel.app/api?type=soft&height=6&color=0:E31B23,50:FDEF42,100:009A44" width="100%"/>

## Rapport QAQC

Le rapport HTML genere automatiquement comprend :

| Section | Contenu |
|---------|---------|
| 📐 **Taille des bases** | Base brute vs bases traitees - observations et variables |
| 📊 **Indicateurs cles** | Cartes metriques colorisees (vert / orange / rouge) |
| 🔍 **Valeurs manquantes** | Taux de NA par variable, seuils alerte (>20%) et critique (>50%) |
| ⚠️ **Valeurs aberrantes** | Detection IQR x3 sur variables numeriques |
| ✅ **Controles coherence** | Unicite identifiants, plages d'age |
| 📈 **Estimations primaires** | Distributions genre, education, region, emploi, ISIC, privation |

<img src="https://capsule-render.vercel.app/api?type=soft&height=6&color=0:009A44,50:FDEF42,100:E31B23" width="100%"/>

## Changer de round sans toucher au code

> Le pipeline est **scalable par conception** grace au fichier `input/variables_mapping.xlsx`.

```
+-------------------------------------------------------------+
|  WORKFLOW NOUVEAU ROUND                                     |
|                                                             |
|  1. Remplacer input/base.dta  par la nouvelle base          |
|                                                             |
|  2. Dans R/config.R, mettre a jour :                        |
|     ROUND <- list(numero = 10, annee = 2025, pays = "SEN")  |
|     FICHIER_BRUT <- "base_r10.dta"                          |
|                                                             |
|  3. Dans variables_mapping.xlsx, remplir les colonnes 🟡 :  |
|     . Feuille "Variables"  -> nouveaux noms de colonnes     |
|     . Feuille "Modalites"  -> nouveaux libellesde codes     |
|                                                             |
|  4. Relancer main.R  ✓                                      |
+-------------------------------------------------------------+
```

> Les cellules laissees vides conservent automatiquement les noms du round precedent.

<img src="https://capsule-render.vercel.app/api?type=soft&height=6&color=0:E31B23,50:FDEF42,100:009A44" width="100%"/>

## Source des donnees

> **[Afrobarometer](https://www.afrobarometer.org/countries/senegal/)** est un programme panafricain de recherche par enquetes mesurant les attitudes citoyennes sur la democratie, la gouvernance et les conditions de vie.

**Round 9 Senegal - 2022** : 1 200 repondants, 1 487 variables.

<img src="https://capsule-render.vercel.app/api?type=soft&height=6&color=0:009A44,50:FDEF42,100:E31B23" width="100%"/>

## Equipe

<div align="center">

| Nom | Formation |
|-----|-----------|
| Ibrahim ADAM ALASSANE | ISE - ENSAE Dakar |
| Moussa DIAKITE | ISE - ENSAE Dakar |
| Fallou NGOM | ISE - ENSAE Dakar |
| Cheikh Sadibou NGOM | ISE - ENSAE Dakar |
| Gnalen SANGARE | ISE - ENSAE Dakar |
| Seman Giovanni Jocelyn GADO | ISE - ENSAE Dakar |
| Sie Rachid TRAORE | ISE - ENSAE Dakar |

**Superviseur : M. MBodj - ENSAE Dakar**

</div>

<img src="https://capsule-render.vercel.app/api?type=waving&height=120&color=0:E31B23,50:FDEF42,100:009A44&section=footer&reversal=false" width="100%"/>
