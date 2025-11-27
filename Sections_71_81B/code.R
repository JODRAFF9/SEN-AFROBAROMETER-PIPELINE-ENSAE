# =========================================================
# Chargement des packages
# =========================================================
library(haven)     # Pour importer les données Stata (.dta)
library(dplyr)     # Manipulation de données
library(ggplot2)   # Visualisation
library(janitor)   # Nettoyage des noms de variables
library(skimr)     # Résumé rapide des données
library(readr)     # Pour lire/écrire des fichiers texte
library(summarytools) # Pour des stats descriptives claires
library(psych)
library(labelled)
library(purrr)
library(tidyr)
library(gridExtra)
library(kableExtra)
library(rmarkdown)
library(openxlsx)
library(janitor)
library(questionr)
# =========================================================
# Importation et exploration de la base
# =========================================================
base <- read_dta("Base/base.dta")

View(base) # Importation du fichier

#skim(base)

#look_for(base)

# =========================================================
# Extraction des labels de variables et de valeurs
# =========================================================

variable_info <- map_dfr(names(base), function(var) {
  var_data <- base[[var]]
  
  # Sécuriser la récupération du label de variable
  var_label <- attr(var_data, "label")
  if (is.null(var_label)) var_label <- NA
  var_label <- as.character(var_label)  # conversion en texte
  
  # Labels de valeurs (si variable catégorielle)
  val_labels <- attr(var_data, "labels")
  
  # Transformation lisible des labels de valeurs
  if (!is.null(val_labels)) {
    val_label_str <- paste0(val_labels, " = ", names(val_labels), collapse = "; ")
  } else {
    val_label_str <- NA
  }
  
  tibble(
    variable = var,
    label_variable = var_label,
    labels_valeurs = val_label_str
  )
})

# ================================================================================
# Aperçu du résultat
# ================================================================================
View(variable_info)

write.xlsx(variable_info, "variables_labels.xlsx")

label_df <- base %>%
  mutate(across(where(is.labelled), to_factor))

View(label_df)

# =========================================================
# Fonctions utiles
# =========================================================
get_label <- function(df, varname) {
  lbl <- attr(df[[varname]], "label")
  if (is.null(lbl)) return(NA)
  return(lbl)
}


frequency_label <- function(df, varname) {
  var <- df[[varname]]
  
  # Convertir en factor ou character
  if (!is.factor(var)) var <- as.factor(var)
  
  # Calcul des fréquences
  freq(var)
}

freq_tables <- function(data) {
  lapply(data, function(var) {
    # tableau brut
    tab <- table(var, useNA = "ifany")
    
    # pourcentages
    pct <- prop.table(tab) * 100
    
    # assemblage dans un data.frame clair
    data.frame(
      Modalité = names(tab),
      Effectif = as.vector(tab),
      Pourcentage = round(as.vector(pct), 2)
    )
  })
}

contingency_table_janitor <- function(df, var1, var2, prop = FALSE) {
  tab <- tabyl(df, .data[[var1]], .data[[var2]])
  if (prop) {
    tab <- adorn_percentages(tab, "row")   # pourcentage par ligne
  }
  tab <- adorn_totals(tab, c("row", "col")) # Totaux
  tab <- adorn_ns(tab)                      # ajoute effectifs si prop
  return(tab)
}


meme_variable <- function(data, var1, var2) {
  return(all(data[[var1]] == data[[var2]]))
}


variables_constantes <- function(data) {
  constantes <- sapply(data, function(x) length(unique(na.omit(x))) == 1)
  noms_constantes <- names(data)[constantes]
  return(noms_constantes)
}

variables_constantes(label_df)

que_des_oui_colonne <- function(data, variable) {
  x <- data[[variable]]
  x <- x[!is.na(x)]
  x <- tolower(x)
  all(x == "oui")
}

enquete_period <- function(data, date_var) {
  # Convertir la variable en Date si ce n'est pas déjà le cas
  data[[date_var]] <- as.Date(data[[date_var]])
  
  # Calculer début et fin
  debut <- min(data[[date_var]], na.rm = TRUE)
  fin   <- max(data[[date_var]], na.rm = TRUE)
  
  # Calculer la durée en jours
  duree <- as.numeric(fin - debut) + 1  # +1 pour inclure les deux dates
  
  # Retourner une phrase
  return(paste("La collecte a commencé le", debut,
               "et s'est terminée le", fin,
               "pour une durée totale de", duree, "jours."))
}

# Exemple d'utilisation
enquete_period(base, "Date")



is_id <- function(x) {
  # retirer les NA
  vals <- x[!is.na(x)]
  
  # tester unicité + valeurs entières
  unique(vals) |> length() == length(vals) &&
    all(vals %% 1 == 0)
}


# =========================================================
# Structure interne des données
# =========================================================
# l'enquête s'est passé les jours 2021-06-16 à 2022-06-18.
# On a 48 variables au debut et les 860 variables suivantes ont une logique à part et repetif (il y'a 20 itérations 
# de de bloc de 43 questions semblables: 860) et il reste alors 570 variables.
# à partir de la variable 1435, on n'a que des questions pour l'enquêteur sur le déroulement de l'entretien.
# =========================================================

# TRAITEMENT DE CES 03 SECTIONS 
##########################################################################################
# Variable d'identification

is_id(base$SbjNum)

## Extraction par indices de colonnes
sub_df <- base[, c(1,1191:1237)]
View(sub_df)

sub_label_df <- sub_df%>%
  mutate(across(where(is.labelled), to_factor))
View(sub_label_df)

##########################################################################################
# Parlons maintenant des conditions environnementales dans le pays et dans votre communauté
## Extraction par indices de colonnes
sub_df1 <- base[, c(1,1191:1205)]
View(sub_df1)

sub_label_df1 <- sub_df1%>%
  mutate(across(where(is.labelled), to_factor))
View(sub_label_df1)


## VUE D'ENSEMBLE
freq_tables(sub_label_df1)


##########################################################################################
# Maintenant, parlons des médias et de la façon dont vous obtenez les informations sur la politique et autres sujets.
## Extraction par indices de colonnes
sub_df2 <- base[, c(1,1206:1214)]
View(sub_df2)

sub_label_df2 <- sub_df2%>%
  mutate(across(where(is.labelled), to_factor))
View(sub_label_df2)

## VUE D'ENSEMBLE
freq_tables(sub_label_df2)


##########################################################################################
# Parlons de l'influence des autres pays au Sénégal
## Extraction par indices de colonnes
sub_df3 <- base[, c(1,1215:1236)]
View(sub_df3)

sub_label_df3 <- sub_df3%>%
  mutate(across(where(is.labelled), to_factor))
View(sub_label_df3)

## VUE D'ENSEMBLE
freq_tables(sub_label_df3)


