# ==============================================================================
# UTILS.R — Fonctions utilitaires partagées par tous les modules du pipeline
# ==============================================================================

# ── Opérateur null-coalescing ─────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ==============================================================================
# selectionner_renommer()
# Sélectionne un sous-ensemble de colonnes depuis `base` et les renomme
# selon un mapping nommé (nom_cible = "NOM_COLONNE_BASE").
# Seules les colonnes présentes dans `base` sont sélectionnées.
#
# Arguments :
#   base    — data.frame source
#   id_col  — nom de la colonne identifiant (sera renommée "id_individu")
#   mapping — liste nommée : list(nom_cible = "NOM_COL_BASE", ...)
#
# Retourne un data.frame avec id_individu + colonnes renommées.
# ==============================================================================
selectionner_renommer <- function(base, id_col, mapping) {

  dispo  <- mapping[unlist(mapping) %in% names(base)]

  if (length(dispo) == 0) {
    return(
      base |>
        dplyr::select(all_of(id_col)) |>
        dplyr::rename(id_individu = all_of(id_col))
    )
  }

  cols   <- unname(unlist(dispo))   # valeurs = noms dans la base
  cibles <- names(dispo)            # noms = noms cibles

  base |>
    dplyr::select(all_of(c(id_col, cols))) |>
    dplyr::rename(
      id_individu = all_of(id_col),
      !!!setNames(cols, cibles)
    )
}

# ==============================================================================
# libeller_region()
# Convertit les codes numériques REGION (Round 9 Sénégal) en libellés.
# ==============================================================================
libeller_region <- function(x) {
  dplyr::case_when(
    x == 660 ~ "Dakar",
    x == 661 ~ "Diourbel",
    x == 662 ~ "Fatick",
    x == 663 ~ "Kaffrine",
    x == 664 ~ "Kaolack",
    x == 665 ~ "Kédougou",
    x == 666 ~ "Kolda",
    x == 667 ~ "Louga",
    x == 668 ~ "Matam",
    x == 669 ~ "Saint-Louis",
    x == 670 ~ "Sédhiou",
    x == 671 ~ "Tambacounda",
    x == 672 ~ "Thiès",
    x == 673 ~ "Ziguinchor",
    TRUE     ~ as.character(x)
  )
}

# ==============================================================================
# libeller_milieu()
# Convertit les codes URBRUR (1/2) en libellés Rural / Urbain.
# ==============================================================================
libeller_milieu <- function(x) {
  dplyr::case_when(
    x == 1 ~ "Rural",
    x == 2 ~ "Urbain",
    TRUE   ~ NA_character_
  )
}

# ==============================================================================
# libeller_genre()
# Convertit les codes Q100 (1/2) en Homme / Femme.
# ==============================================================================
libeller_genre <- function(x) {
  dplyr::case_when(
    x == 1 ~ "Homme",
    x == 2 ~ "Femme",
    TRUE   ~ NA_character_
  )
}

# ==============================================================================
# libeller_niveau_etudes()
# Convertit les codes Q94 en libellés de niveau d'instruction.
# ==============================================================================
libeller_niveau_etudes <- function(x) {
  dplyr::case_when(
    x == 0 ~ "Aucun enseignement formel",
    x == 1 ~ "Enseignement informel/coranique",
    x == 2 ~ "Primaire incomplet",
    x == 3 ~ "Primaire complet",
    x == 4 ~ "Secondaire incomplet",
    x == 5 ~ "Secondaire complet",
    x == 6 ~ "Post-secondaire incomplet",
    x == 7 ~ "Université complet",
    TRUE   ~ NA_character_
  )
}

# ==============================================================================
# libeller_statut_emploi()
# Convertit les codes Q93A en libellés de statut d'emploi.
# ==============================================================================
libeller_statut_emploi <- function(x) {
  dplyr::case_when(
    x == 0 ~ "Inactif (ne cherche pas)",
    x == 1 ~ "Chômeur (cherche un emploi)",
    x == 2 ~ "Employé à temps partiel",
    x == 3 ~ "Employé à temps plein",
    TRUE   ~ NA_character_
  )
}

# ==============================================================================
# libeller_source_eau()
# Convertit les codes Q91A en libellés de source d'eau.
# ==============================================================================
libeller_source_eau <- function(x) {
  dplyr::case_when(
    x == 1 ~ "Robinet dans la maison",
    x == 2 ~ "Robinet dans la cour/concession",
    x == 3 ~ "Fontaine publique",
    x == 4 ~ "Puits protégé",
    x == 5 ~ "Puits non protégé",
    x == 6 ~ "Source/rivière",
    x == 7 ~ "Eau de pluie",
    x == 8 ~ "Eau en bouteille",
    TRUE   ~ NA_character_
  )
}

# ==============================================================================
# libeller_electricite()
# Convertit les codes Q92A (0/1) en Non / Oui.
# ==============================================================================
libeller_electricite <- function(x) {
  dplyr::case_when(
    x == 1 ~ "Oui",
    x == 0 ~ "Non",
    TRUE   ~ NA_character_
  )
}

# ==============================================================================
# appliquer_isic()
# Associe un vecteur de codes sectoriels Afrobarometer à la classification
# ISIC Rev 4 (section et libellé). Retourne une liste avec $section et $libelle.
#
# Arguments :
#   codes       — vecteur de codes numériques (Q93B_YES, Q93B_2, etc.)
#   isic_map    — data.frame ISIC_MAPPING défini dans config.R
# ==============================================================================
appliquer_isic <- function(codes, isic_map) {
  codes_int <- suppressWarnings(as.integer(as.character(codes)))
  idx       <- match(codes_int, isic_map$code_afrobarometer)
  list(
    section = isic_map$isic_section[idx],
    libelle = isic_map$isic_libelle[idx]
  )
}

# ==============================================================================
# pct_na()
# Calcule le pourcentage de valeurs manquantes dans un vecteur.
# ==============================================================================
pct_na <- function(x) round(mean(is.na(x)) * 100, 2)

# ==============================================================================
# statut_na()
# Classe le taux de NA selon les seuils définis dans config.R.
# ==============================================================================
statut_na <- function(pct, seuils = SEUILS_QAQC) {
  dplyr::case_when(
    pct == 0                                    ~ "OK",
    pct <= seuils$pct_na_alerte    * 100        ~ "Faible",
    pct <= seuils$pct_na_exclusion * 100        ~ "Alerte",
    TRUE                                        ~ "Critique"
  )
}
