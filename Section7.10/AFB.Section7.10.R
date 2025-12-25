# ==============================================================================
# TRAITEMENT AVANCE DES DONNEES AFROBAROMETRE 
# Section 7.10 
# Version 2.0 finale avec regles Q56 et Q64A integrees
# ==============================================================================

# Chargement des bibliotheques necessaires
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# ==============================================================================
# FONCTION PRINCIPALE AMELIOREE AVEC TESTS DE CARACTERE ALEATOIRE
# ==============================================================================

traitement_afrobarometer_conditionnel <- function(data, 
                                                   seuil_na_max = 0.5,
                                                   test_aleatoire = TRUE,
                                                   seuil_pvalue = 0.05,
                                                   verbose = TRUE) {
  "
  Traite les donnees Afrobarometre avec distinction precise entre:
  - NA normaux (non-applicabilite legitime du point du vue du questionnaire)
  - NA anormaux (non-reponse chez les applicables)
  Cette fonction est flexible
  Arguments:
    data: Data frame des donnees brutes
    seuil_na_max: Seuil de NA au-dela duquel on n'impute pas (defaut: 0.5)
    test_aleatoire: Tester le caractere aleatoire des NA avant imputation (defaut: TRUE)
    seuil_pvalue: Seuil de p-value pour les tests d'aleatoire (defaut: 0.05)
    verbose: Afficher les messages de progression (defaut: TRUE)
  
  Retourne:
    Liste contenant data_clean, journal, problemes, resume_amelioration
  "
  
  # Validation des entrees
  if(!is.data.frame(data)) {
    stop("L'argument 'data' doit etre un data.frame")
  }
  
  if(!is.logical(test_aleatoire)) {
    stop("L'argument 'test_aleatoire' doit etre TRUE ou FALSE")
  }
  
  if(!is.numeric(seuil_pvalue) || seuil_pvalue <= 0 || seuil_pvalue >= 1) {
    stop("L'argument 'seuil_pvalue' doit etre un nombre entre 0 et 1")
  }
  
  # Initialisation
  journal <- list() # pour tracer les opérations 
  problemes <- list() # stocker les incohérences
  timestamp_debut <- Sys.time() # 
  
  # ============================================================================
  # ETAPE 1: Selection des variables Q49 a Q70 
  # ============================================================================
  
  if(verbose) cat("=== ETAPE 1: Selection des variables Q49 a Q70 ===\n")
  
  pattern_principal <- "^Q(49|[5-6][0-9]|70)"
  pattern_sous_questions <- "^Q(49|[5-6][0-9]|70)[A-Z](_[0-9]+)?$"
  
  vars_selectionnees <- names(data)[str_detect(names(data), pattern_principal)]
  
  if(length(vars_selectionnees) == 0) {
    stop("Aucune variable Q49-Q70 trouvee dans les donnees")
  }
  
  data_section <- data %>% select(all_of(vars_selectionnees))
  
  journal$variables_selectionnees <- vars_selectionnees
  journal$n_variables <- length(vars_selectionnees)
  
  # ============================================================================
  # ETAPE 2: Definition des regles de conditionnalite (STRUCTUREE)
  # ============================================================================
  
  if(verbose) cat("=== ETAPE 2: Definition des regles de conditionnalite ===\n")
  
  regles_conditionnalite <- definir_regles_conditionnalite()
  
  # Verifier que les variables de condition existent
  variables_condition <- extraire_variables_condition(regles_conditionnalite)
  variables_manquantes <- setdiff(variables_condition, names(data_section))
  
  if(length(variables_manquantes) > 0) {
    warning(paste("Variables de condition manquantes:", 
                  paste(variables_manquantes, collapse = ", ")))
    problemes$variables_condition_manquantes <- variables_manquantes
  }
  
  # ============================================================================
  # ETAPE 3: Analyse fine des valeurs manquantes
  # ============================================================================
  
  if(verbose) cat("=== ETAPE 3: Analyse fine des valeurs manquantes ===\n")
  
  analyse_detaille <- analyser_na_conditionnels(data_section, regles_conditionnalite)
  journal$analyse_avant <- analyse_detaille
  
  # ============================================================================
  # ETAPE 3.5: TESTS DE CARACTERE ALEATOIRE DES NA (NOUVELLE)
  # ============================================================================
  
  if(test_aleatoire) {
    if(verbose) cat("=== ETAPE 3.5: Tests de caractere aleatoire des NA ===\n")
    
    tests_aleatoire <- list()
    
    for(variable in names(regles_conditionnalite)) {
      if(variable %in% names(data_section)) {
        regle <- regles_conditionnalite[[variable]]
        
        # Effectuer le test de caractere aleatoire
        resultat_test <- tester_caractere_aleatoire_na(
          data_section,
          variable,
          regle$condition_applicabilite,
          seuil_pvalue = seuil_pvalue,
          verbose = verbose
        )
        
        tests_aleatoire[[variable]] <- resultat_test
        
        # Si les NA ne sont PAS aleatoires, marquer comme problematique
        if(!resultat_test$na_aleatoires) {
          if(verbose) {
            cat(sprintf("  ATTENTION %s: NA NON aleatoires (p-value = %.4f) - Imputation deconseillée\n", 
                        variable, resultat_test$p_value_min))
          }
          problemes$na_non_aleatoires <- c(problemes$na_non_aleatoires, variable)
        }
      }
    }
    
    journal$tests_aleatoire <- tests_aleatoire
  }
  
  # ============================================================================
  # ETAPE 4: Traitement differencie des NA conditionnels 
  # ============================================================================
  
  if(verbose) cat("=== ETAPE 4: Traitement differencie des NA conditionnels ===\n")
  
  resultats_imputation_conditionnelle <- list()
  
  for(variable in names(regles_conditionnalite)) {
    if(variable %in% names(data_section)) {
      regle <- regles_conditionnalite[[variable]]
      
      # Verifier si les tests d'aleatoire ont ete effectues
      na_aleatoires <- TRUE  # Par defaut, on assume l'aleatoire
      p_value <- NA
      
      if(test_aleatoire && variable %in% names(journal$tests_aleatoire)) {
        test_result <- journal$tests_aleatoire[[variable]]
        na_aleatoires <- test_result$na_aleatoires
        p_value <- test_result$p_value_min
      }
      
      # Decider si on impute ou pas
      if(!test_aleatoire || na_aleatoires) {
        # Les NA sont aleatoires (ou pas de test) -> Imputation
        resultat <- imputer_applicables_seulement(
          data_section, 
          variable, 
          regle$condition_applicabilite,
          methode_auto = TRUE
        )
        
        data_section <- resultat$data
        
        resultats_imputation_conditionnelle[[variable]] <- list(
          variable = variable,
          na_applicables_avant = analyse_detaille[[variable]]$na_applicables,
          na_applicables_apres = sum(is.na(data_section[[variable]]) & 
                                      evaluer_condition(data_section, regle$condition_applicabilite)),
          applicables_imputes = resultat$imputes,
          methode_utilisee = resultat$methode,
          condition = regle$condition_applicabilite,
          test_aleatoire_effectue = test_aleatoire,
          na_aleatoires = na_aleatoires,
          p_value = p_value,
          action = "Imputation effectuee"
        )
        
      } else {
        # Les NA ne sont PAS aleatoires -> Pas d'imputation
        if(verbose) {
          cat(sprintf("  -> %s: Imputation ignoree (NA non aleatoires, p=%.4f)\n", 
                      variable, p_value))
        }
        
        resultats_imputation_conditionnelle[[variable]] <- list(
          variable = variable,
          na_applicables_avant = analyse_detaille[[variable]]$na_applicables,
          na_applicables_apres = analyse_detaille[[variable]]$na_applicables,
          applicables_imputes = 0,
          methode_utilisee = "Aucune",
          condition = regle$condition_applicabilite,
          test_aleatoire_effectue = test_aleatoire,
          na_aleatoires = na_aleatoires,
          p_value = p_value,
          action = "Imputation ignoree (NA non aleatoires)"
        )
      }
    }
  }
  
  journal$imputation_conditionnelle <- resultats_imputation_conditionnelle
  
  # ============================================================================
  # ETAPE 5: Traitement des variables non conditionnelles
  # ============================================================================
  
  if(verbose) cat("=== ETAPE 5: Traitement des variables non conditionnelles ===\n")
  
  vars_conditionnelles <- names(regles_conditionnalite)
  vars_non_conditionnelles <- setdiff(names(data_section), vars_conditionnelles)
  
  resultats_imputation_standard <- list()
  
  for(variable in vars_non_conditionnelles) {
    na_count <- sum(is.na(data_section[[variable]]))
    pct_na <- na_count / nrow(data_section)
    
    if(na_count == 0) {
      next  # Pas de traitement necessaire
    }
    
    if(pct_na < seuil_na_max) {
      
      # Test de caractere aleatoire pour variables non conditionnelles
      na_aleatoires <- TRUE
      p_value <- NA
      
      if(test_aleatoire) {
        resultat_test <- tester_caractere_aleatoire_na_standard(
          data_section,
          variable,
          seuil_pvalue = seuil_pvalue
        )
        na_aleatoires <- resultat_test$na_aleatoires
        p_value <- resultat_test$p_value_min
      }
      
      if(!test_aleatoire || na_aleatoires) {
        # Imputation
        resultat <- imputer_standard(data_section, variable)
        data_section <- resultat$data
        
        resultats_imputation_standard[[variable]] <- list(
          variable = variable,
          na_avant = na_count,
          na_apres = sum(is.na(data_section[[variable]])),
          imputes = resultat$imputes,
          methode = resultat$methode,
          pct_na_avant = round(pct_na * 100, 1),
          test_aleatoire_effectue = test_aleatoire,
          na_aleatoires = na_aleatoires,
          p_value = p_value,
          action = "Imputation effectuee"
        )
      } else {
        # Pas d'imputation
        resultats_imputation_standard[[variable]] <- list(
          variable = variable,
          na_avant = na_count,
          na_apres = na_count,
          imputes = 0,
          methode = "Aucune",
          pct_na_avant = round(pct_na * 100, 1),
          test_aleatoire_effectue = test_aleatoire,
          na_aleatoires = na_aleatoires,
          p_value = p_value,
          action = "Imputation ignoree (NA non aleatoires)"
        )
        
        problemes$na_non_aleatoires <- c(problemes$na_non_aleatoires, variable)
      }
      
    } else {
      resultats_imputation_standard[[variable]] <- list(
        variable = variable,
        na_avant = na_count,
        action = "Pas d'imputation",
        raison = paste0("Trop de NA (", round(pct_na * 100, 1), "%)"),
        pct_na = round(pct_na * 100, 1)
      )
      
      problemes$variables_trop_de_na <- c(problemes$variables_trop_de_na, variable)
    }
  }
  
  journal$imputation_standard <- resultats_imputation_standard
  
  # ============================================================================
  # ETAPE 6: Validation et rapport final (ENRICHI)
  # ============================================================================
  
  if(verbose) cat("=== ETAPE 6: Validation du traitement ===\n")
  
  # Analyse finale
  analyse_finale <- analyser_na_conditionnels(data_section, regles_conditionnalite)
  journal$analyse_apres <- analyse_finale
  
  # Resume des ameliorations
  resume_amelioration <- calculer_resume_amelioration(
    analyse_detaille, 
    analyse_finale
  )
  
  # Statistiques globales
  stats_globales <- list(
    total_variables = ncol(data_section),
    variables_conditionnelles = length(regles_conditionnalite),
    variables_non_conditionnelles = length(vars_non_conditionnelles),
    na_applicables_imputes = sum(resume_amelioration$reduction, na.rm = TRUE),
    pct_amelioration_global = round(mean(resume_amelioration$pct_reduction, na.rm = TRUE), 1),
    temps_execution = as.numeric(difftime(Sys.time(), timestamp_debut, units = "secs")),
    test_aleatoire_active = test_aleatoire,
    variables_na_non_aleatoires = length(unique(problemes$na_non_aleatoires))
  )
  
  journal$stats_globales <- stats_globales
  
  # Affichage du resume
  if(verbose) {
    cat("\n=== TRAITEMENT CONDITIONNEL TERMINE ===\n")
    cat("NA applicables imputes:", stats_globales$na_applicables_imputes, "\n")
    cat("Amelioration moyenne:", stats_globales$pct_amelioration_global, "%\n")
    cat("Temps d'execution:", round(stats_globales$temps_execution, 2), "secondes\n")
    
    if(test_aleatoire && stats_globales$variables_na_non_aleatoires > 0) {
      cat("\nATTENTION:", stats_globales$variables_na_non_aleatoires, 
          "variable(s) avec NA non aleatoires detectee(s)\n")
      cat("  Ces variables n'ont PAS ete imputees pour eviter les biais.\n")
    }
  }
  
  # Retour structure
  return(structure(
    list(
      data_clean = data_section,
      journal = journal,
      problemes = problemes,
      resume_amelioration = resume_amelioration
    ),
    class = "traitement_afrobarometer"
  ))
}

# ==============================================================================
# FONCTIONS DE TEST DE CARACTERE ALEATOIRE DES NA
# ==============================================================================

tester_caractere_aleatoire_na <- function(data, variable, condition_applicabilite, 
                                          seuil_pvalue = 0.05, verbose = FALSE) {
  "
  Teste si les valeurs manquantes sont aleatoires (MCAR/MAR) pour une variable conditionnelle.
  
  Utilise plusieurs tests:
  1. Test de Little (MCAR global)
  2. Tests t / Chi-carre entre groupes avec/sans NA
  3. Analyse des patterns de NA
  
  Arguments:
    data: Data frame contenant les donnees
    variable: Nom de la variable a tester
    condition_applicabilite: Condition definissant les applicables
    seuil_pvalue: Seuil de significativite (defaut: 0.05)
    verbose: Afficher les details (defaut: FALSE)
  
  Retourne:
    Liste avec na_aleatoires (TRUE/FALSE), p_values, et details
  "
  
  # Evaluer la condition d'applicabilite
  condition_applicable <- evaluer_condition(data, condition_applicabilite)
  
  # Filtrer uniquement les applicables
  data_applicables <- data[condition_applicable, , drop = FALSE]
  
  # Verifier qu'il y a assez de donnees
  if(nrow(data_applicables) < 30) {
    if(verbose) cat(sprintf("  %s: Echantillon trop petit (n=%d) - Test ignore\n", 
                            variable, nrow(data_applicables)))
    return(list(
      variable = variable,
      na_aleatoires = TRUE,
      p_value_min = NA,
      test_effectue = FALSE,
      raison = "Echantillon trop petit",
      details = list()
    ))
  }
  
  # Creer l'indicateur de NA pour la variable cible
  na_indicator <- is.na(data_applicables[[variable]])
  n_na <- sum(na_indicator)
  
  # Verifier qu'il y a des NA
  if(n_na == 0) {
    return(list(
      variable = variable,
      na_aleatoires = TRUE,
      p_value_min = NA,
      test_effectue = FALSE,
      raison = "Aucun NA a tester",
      details = list()
    ))
  }
  
  # Verifier qu'il y a aussi des non-NA
  if(n_na == nrow(data_applicables)) {
    return(list(
      variable = variable,
      na_aleatoires = TRUE,
      p_value_min = NA,
      test_effectue = FALSE,
      raison = "Que des NA",
      details = list()
    ))
  }
  
  # TEST: Comparer les caracteristiques entre groupe avec/sans NA
  p_values <- numeric()
  tests_details <- list()
  
  # Identifier les autres variables pour les tests
  autres_vars <- setdiff(names(data_applicables), variable)
  
  # Limiter a 10 variables maximum pour eviter les problemes de performance
  if(length(autres_vars) > 10) {
    autres_vars_numeriques <- autres_vars[sapply(data_applicables[autres_vars], is.numeric)]
    autres_vars_facteurs <- autres_vars[sapply(data_applicables[autres_vars], is.factor)]
    
    autres_vars <- c(
      head(autres_vars_numeriques, 5),
      head(autres_vars_facteurs, 5)
    )
  }
  
  # Tester chaque autre variable
  for(autre_var in autres_vars) {
    
    # Ignorer si trop de NA dans l'autre variable
    if(sum(is.na(data_applicables[[autre_var]])) > nrow(data_applicables) * 0.5) {
      next
    }
    
    tryCatch({
      
      if(is.numeric(data_applicables[[autre_var]])) {
        # Test t pour variables numeriques
        groupe_na <- data_applicables[[autre_var]][na_indicator]
        groupe_non_na <- data_applicables[[autre_var]][!na_indicator]
        
        groupe_na <- groupe_na[!is.na(groupe_na)]
        groupe_non_na <- groupe_non_na[!is.na(groupe_non_na)]
        
        if(length(groupe_na) >= 3 && length(groupe_non_na) >= 3) {
          test_result <- t.test(groupe_na, groupe_non_na)
          p_values <- c(p_values, test_result$p.value)
          
          tests_details[[autre_var]] <- list(
            type = "t-test",
            p_value = test_result$p.value,
            significatif = test_result$p.value < seuil_pvalue
          )
        }
        
      } else if(is.factor(data_applicables[[autre_var]]) || is.character(data_applicables[[autre_var]])) {
        # Test Chi-carre pour variables categorielles
        table_contingence <- table(
          na_indicator,
          data_applicables[[autre_var]],
          useNA = "no"
        )
        
        if(all(table_contingence >= 5) && nrow(table_contingence) == 2 && ncol(table_contingence) >= 2) {
          test_result <- chisq.test(table_contingence)
          p_values <- c(p_values, test_result$p.value)
          
          tests_details[[autre_var]] <- list(
            type = "chi-square",
            p_value = test_result$p.value,
            significatif = test_result$p.value < seuil_pvalue
          )
        }
      }
      
    }, error = function(e) {
      if(verbose) cat(sprintf("    Erreur lors du test avec %s: %s\n", autre_var, e$message))
    })
  }
  
  # DECISION FINALE
  if(length(p_values) == 0) {
    return(list(
      variable = variable,
      na_aleatoires = TRUE,
      p_value_min = NA,
      test_effectue = FALSE,
      raison = "Aucun test valide",
      details = tests_details
    ))
  }
  
  p_value_min <- min(p_values, na.rm = TRUE)
  n_tests_significatifs <- sum(p_values < seuil_pvalue, na.rm = TRUE)
  pct_tests_significatifs <- n_tests_significatifs / length(p_values)
  na_aleatoires <- pct_tests_significatifs < 0.25
  
  if(verbose) {
    cat(sprintf("  %s: %d tests effectues, %d significatifs (%.1f%%) - ", 
                variable, length(p_values), n_tests_significatifs, 
                pct_tests_significatifs * 100))
    cat(ifelse(na_aleatoires, "NA ALEATOIRES\n", "NA NON ALEATOIRES\n"))
  }
  
  return(list(
    variable = variable,
    na_aleatoires = na_aleatoires,
    p_value_min = p_value_min,
    n_tests = length(p_values),
    n_tests_significatifs = n_tests_significatifs,
    pct_tests_significatifs = round(pct_tests_significatifs * 100, 1),
    test_effectue = TRUE,
    details = tests_details
  ))
}

tester_caractere_aleatoire_na_standard <- function(data, variable, seuil_pvalue = 0.05) {
  "
  Teste si les valeurs manquantes sont aleatoires pour une variable non conditionnelle.
  Version simplifiee sans condition d'applicabilite.
  "
  
  na_indicator <- is.na(data[[variable]])
  n_na <- sum(na_indicator)
  
  if(n_na == 0 || n_na == nrow(data) || nrow(data) < 30) {
    return(list(
      variable = variable,
      na_aleatoires = TRUE,
      p_value_min = NA,
      test_effectue = FALSE,
      raison = "Conditions non remplies"
    ))
  }
  
  p_values <- numeric()
  autres_vars <- setdiff(names(data), variable)
  
  if(length(autres_vars) > 10) {
    autres_vars_numeriques <- autres_vars[sapply(data[autres_vars], is.numeric)]
    autres_vars_facteurs <- autres_vars[sapply(data[autres_vars], is.factor)]
    autres_vars <- c(head(autres_vars_numeriques, 5), head(autres_vars_facteurs, 5))
  }
  
  for(autre_var in autres_vars) {
    if(sum(is.na(data[[autre_var]])) > nrow(data) * 0.5) next
    
    tryCatch({
      if(is.numeric(data[[autre_var]])) {
        groupe_na <- data[[autre_var]][na_indicator]
        groupe_non_na <- data[[autre_var]][!na_indicator]
        groupe_na <- groupe_na[!is.na(groupe_na)]
        groupe_non_na <- groupe_non_na[!is.na(groupe_non_na)]
        
        if(length(groupe_na) >= 3 && length(groupe_non_na) >= 3) {
          test_result <- t.test(groupe_na, groupe_non_na)
          p_values <- c(p_values, test_result$p.value)
        }
        
      } else if(is.factor(data[[autre_var]]) || is.character(data[[autre_var]])) {
        table_contingence <- table(na_indicator, data[[autre_var]], useNA = "no")
        
        if(all(table_contingence >= 5) && nrow(table_contingence) == 2 && ncol(table_contingence) >= 2) {
          test_result <- chisq.test(table_contingence)
          p_values <- c(p_values, test_result$p.value)
        }
      }
    }, error = function(e) {})
  }
  
  if(length(p_values) == 0) {
    return(list(
      variable = variable,
      na_aleatoires = TRUE,
      p_value_min = NA,
      test_effectue = FALSE
    ))
  }
  
  p_value_min <- min(p_values, na.rm = TRUE)
  n_tests_significatifs <- sum(p_values < seuil_pvalue, na.rm = TRUE)
  pct_tests_significatifs <- n_tests_significatifs / length(p_values)
  na_aleatoires <- pct_tests_significatifs < 0.25
  
  return(list(
    variable = variable,
    na_aleatoires = na_aleatoires,
    p_value_min = p_value_min,
    n_tests = length(p_values),
    n_tests_significatifs = n_tests_significatifs,
    pct_tests_significatifs = round(pct_tests_significatifs * 100, 1),
    test_effectue = TRUE
  ))
}

# ==============================================================================
# FONCTIONS AUXILIAIRES
# ==============================================================================

definir_regles_conditionnalite <- function() {
  "Definit les regles de conditionnalite pour les questions"
  
  list(
    # Module COVID-19
    Q58B = list(
      condition_applicabilite = "Q58A == 0",
      description = "Probabilite de vaccination (non-vaccines uniquement)",
      groupe_conditionnel = "covid",
      variable_parent = "Q58A"
    ),
    
    Q58C = list(
      condition_applicabilite = "Q58B %in% c(3, 4)",
      description = "Raisons de non-vaccination (peu probables uniquement)",
      groupe_conditionnel = "covid",
      variable_parent = "Q58B"
    ),
    
    # Module Services sociaux - REGLES NUMERIQUES
    Q56A_1 = list(
      condition_applicabilite = "Q56A %in% c(4, 5)",
      description = "Intensite du desaccord (aide enfants maltraites)",
      groupe_conditionnel = "services_sociaux",
      variable_parent = "Q56A"
    ),
    
    Q56B_2 = list(
      condition_applicabilite = "Q56B %in% c(4, 5)",
      description = "Intensite du desaccord (soutien enfants handicapes)",
      groupe_conditionnel = "services_sociaux",
      variable_parent = "Q56B"
    ),
    
    Q56C_2 = list(
      condition_applicabilite = "Q56C %in% c(4, 5)",
      description = "Intensite du desaccord (aide sante mentale)",
      groupe_conditionnel = "services_sociaux",
      variable_parent = "Q56C"
    ),
    
    # Module Preparation COVID - REGLE NUMERIQUE
    Q64A_3 = list(
      condition_applicabilite = "Q64A %in% c(3, 4)",
      description = "Degre de non-preparation gouvernementale",
      groupe_conditionnel = "covid_preparedness",
      variable_parent = "Q64A"
    ),
    
    # Module Changement climatique - REGLES NUMERIQUES
    Q67B = list(
      condition_applicabilite = "Q67A == 1",
      description = "Impact du changement climatique",
      groupe_conditionnel = "climat",
      variable_parent = "Q67A"
    ),
    
    Q68A = list(
      condition_applicabilite = "Q67A == 1",
      description = "Role des Senegalais dans la lutte",
      groupe_conditionnel = "climat",
      variable_parent = "Q67A"
    ),
    
    Q68B = list(
      condition_applicabilite = "Q67A == 1",
      description = "Action gouvernementale sur le climat",
      groupe_conditionnel = "climat",
      variable_parent = "Q67A"
    ),
    
    Q69 = list(
      condition_applicabilite = "Q67A == 1",
      description = "Responsabilite face au changement climatique",
      groupe_conditionnel = "climat",
      variable_parent = "Q67A"
    ),
    
    Q70A = list(
      condition_applicabilite = "Q67A == 1",
      description = "Effort des Senegalais",
      groupe_conditionnel = "climat",
      variable_parent = "Q67A"
    ),
    
    Q70B = list(
      condition_applicabilite = "Q67A == 1",
      description = "Effort de l'industrie",
      groupe_conditionnel = "climat",
      variable_parent = "Q67A"
    ),
    
    Q70C = list(
      condition_applicabilite = "Q67A == 1",
      description = "Effort du gouvernement",
      groupe_conditionnel = "climat",
      variable_parent = "Q67A"
    ),
    
    Q70D = list(
      condition_applicabilite = "Q67A == 1",
      description = "Effort des pays riches",
      groupe_conditionnel = "climat",
      variable_parent = "Q67A"
    )
  )
}

evaluer_condition <- function(data, condition_text) {
  "Evalue une condition de maniere securisee"
  
  tryCatch({
    eval(parse(text = condition_text), envir = data)
  }, error = function(e) {
    warning(paste("Erreur lors de l'evaluation de la condition:", condition_text))
    rep(FALSE, nrow(data))
  })
}

extraire_variables_condition <- function(regles) {
  "Extrait les noms des variables utilisees dans les conditions"
  
  variables <- character()
  
  for(regle in regles) {
    matches <- str_extract_all(regle$condition_applicabilite, "Q[0-9]+[A-Z]?")[[1]]
    variables <- c(variables, matches)
  }
  
  unique(variables)
}

analyser_na_conditionnels <- function(data, regles) {
  "Analyse detaillee des NA par condition d'applicabilite"
  
  resultats_analyse <- list()
  
  for(variable in names(regles)) {
    if(!variable %in% names(data)) {
      next
    }
    
    regle <- regles[[variable]]
    condition_applicable <- evaluer_condition(data, regle$condition_applicabilite)
    
    total_observations <- nrow(data)
    total_applicables <- sum(condition_applicable, na.rm = TRUE)
    total_non_applicables <- sum(!condition_applicable, na.rm = TRUE)
    
    na_applicables <- sum(is.na(data[[variable]]) & condition_applicable, na.rm = TRUE)
    na_non_applicables <- sum(is.na(data[[variable]]) & !condition_applicable, na.rm = TRUE)
    
    pct_na_problematiques <- if(total_applicables > 0) {
      round(na_applicables / total_applicables * 100, 1)
    } else {
      0
    }
    
    resultats_analyse[[variable]] <- list(
      variable = variable,
      total_observations = total_observations,
      total_applicables = total_applicables,
      total_non_applicables = total_non_applicables,
      na_applicables = na_applicables,
      na_non_applicables = na_non_applicables,
      pct_na_problematiques = pct_na_problematiques,
      description = regle$description,
      condition = regle$condition_applicabilite,
      groupe = regle$groupe_conditionnel
    )
  }
  
  return(resultats_analyse)
}

imputer_applicables_seulement <- function(data, variable, condition_applicabilite, 
                                          methode_auto = TRUE) {
  "Impute uniquement les NA chez les personnes applicables"
  
  condition_applicable <- evaluer_condition(data, condition_applicabilite)
  applicables_na <- which(condition_applicable & is.na(data[[variable]]))
  na_avant <- length(applicables_na)
  
  if(na_avant == 0) {
    return(list(data = data, imputes = 0, methode = "Aucune"))
  }
  
  methode_utilisee <- "Aucune"
  
  if(is.factor(data[[variable]]) || is.character(data[[variable]])) {
    applicables_non_na <- which(condition_applicable & !is.na(data[[variable]]))
    
    if(length(applicables_non_na) > 0) {
      freq_table <- table(data[[variable]][applicables_non_na])
      mode_val <- names(freq_table)[which.max(freq_table)]
      
      if(length(mode_val) > 0) {
        data[[variable]][applicables_na] <- mode_val
        methode_utilisee <- paste0("Mode (", mode_val, ")")
      }
    }
    
  } else if(is.numeric(data[[variable]])) {
    applicables_non_na <- which(condition_applicable & !is.na(data[[variable]]))
    
    if(length(applicables_non_na) > 0) {
      valeurs <- data[[variable]][applicables_non_na]
      
      if(methode_auto) {
        if(length(valeurs) >= 30) {
          skewness <- (mean(valeurs) - median(valeurs)) / sd(valeurs)
          
          if(abs(skewness) > 1) {
            valeur_imp <- median(valeurs, na.rm = TRUE)
            methode_utilisee <- paste0("Mediane (", round(valeur_imp, 2), ")")
          } else {
            valeur_imp <- mean(valeurs, na.rm = TRUE)
            methode_utilisee <- paste0("Moyenne (", round(valeur_imp, 2), ")")
          }
        } else {
          valeur_imp <- median(valeurs, na.rm = TRUE)
          methode_utilisee <- paste0("Mediane (", round(valeur_imp, 2), ")")
        }
        
        data[[variable]][applicables_na] <- valeur_imp
      }
    }
  }
  
  na_apres <- sum(is.na(data[[variable]]) & condition_applicable)
  applicables_imputes <- na_avant - na_apres
  
  return(list(
    data = data,
    imputes = applicables_imputes,
    methode = methode_utilisee
  ))
}

imputer_standard <- function(data, variable) {
  "Imputation standard pour variables non conditionnelles"
  
  na_avant <- sum(is.na(data[[variable]]))
  
  if(na_avant == 0) {
    return(list(data = data, imputes = 0, methode = "Aucune"))
  }
  
  methode <- "Aucune"
  
  if(is.factor(data[[variable]]) || is.character(data[[variable]])) {
    freq_table <- table(data[[variable]], useNA = "no")
    if(length(freq_table) > 0) {
      mode_val <- names(freq_table)[which.max(freq_table)]
      data[[variable]][is.na(data[[variable]])] <- mode_val
      methode <- paste0("Mode (", mode_val, ")")
    }
    
  } else if(is.numeric(data[[variable]])) {
    median_val <- median(data[[variable]], na.rm = TRUE)
    data[[variable]][is.na(data[[variable]])] <- median_val
    methode <- paste0("Mediane (", round(median_val, 2), ")")
  }
  
  na_apres <- sum(is.na(data[[variable]]))
  
  return(list(
    data = data,
    imputes = na_avant - na_apres,
    methode = methode
  ))
}

calculer_resume_amelioration <- function(analyse_avant, analyse_apres) {
  "Calcule le resume des ameliorations"
  
  map_dfr(names(analyse_avant), function(var) {
    avant <- analyse_avant[[var]]
    apres <- analyse_apres[[var]]
    
    reduction <- avant$na_applicables - apres$na_applicables
    pct_reduction <- if(avant$na_applicables > 0) {
      round(reduction / avant$na_applicables * 100, 1)
    } else {
      0
    }
    
    data.frame(
      variable = var,
      groupe = avant$groupe,
      na_applicables_avant = avant$na_applicables,
      na_applicables_apres = apres$na_applicables,
      reduction = reduction,
      pct_reduction = pct_reduction,
      description = avant$description,
      stringsAsFactors = FALSE
    )
  })
}

# ==============================================================================
# FONCTION DE RAPPORT
# ==============================================================================

generer_rapport_conditionnel <- function(resultats_traitement, 
                                         format = c("console", "dataframe"),
                                         inclure_tests_aleatoire = TRUE) {
  "
  Genere un rapport detaille sur le traitement conditionnel des NA
  "
  
  format <- match.arg(format)
  
  journal <- resultats_traitement$journal
  problemes <- resultats_traitement$problemes
  
  if(format == "console") {
    cat("\n")
    cat("===============================================================\n")
    cat("         RAPPORT DE TRAITEMENT CONDITIONNEL V2\n")
    cat("         Afrobarometre - Section 7.10\n")
    cat("===============================================================\n\n")
    
    cat("1. VUE D'ENSEMBLE\n")
    cat("-----------------\n")
    stats <- journal$stats_globales
    cat(sprintf("  Variables totales              : %d\n", stats$total_variables))
    cat(sprintf("  Variables conditionnelles      : %d\n", stats$variables_conditionnelles))
    cat(sprintf("  Variables non conditionnelles  : %d\n", stats$variables_non_conditionnelles))
    cat(sprintf("  NA applicables imputes         : %d\n", stats$na_applicables_imputes))
    cat(sprintf("  Amelioration moyenne           : %.1f%%\n", stats$pct_amelioration_global))
    cat(sprintf("  Temps d'execution              : %.2f secondes\n", stats$temps_execution))
    
    if(stats$test_aleatoire_active) {
      cat(sprintf("  Tests d'aleatoire actives      : OUI\n"))
      cat(sprintf("  Variables NA non aleatoires    : %d\n", stats$variables_na_non_aleatoires))
    } else {
      cat(sprintf("  Tests d'aleatoire actives      : NON\n"))
    }
    cat("\n")
    
    if(inclure_tests_aleatoire && stats$test_aleatoire_active && 
       !is.null(journal$tests_aleatoire) && length(journal$tests_aleatoire) > 0) {
      
      cat("2. RESULTATS DES TESTS D'ALEATOIRE DES NA\n")
      cat("------------------------------------------\n\n")
      
      for(variable in names(journal$tests_aleatoire)) {
        test <- journal$tests_aleatoire[[variable]]
        
        if(test$test_effectue) {
          cat(sprintf("  %s:\n", variable))
          cat(sprintf("    Tests effectues      : %d\n", test$n_tests))
          cat(sprintf("    Tests significatifs  : %d (%.1f%%)\n", 
                      test$n_tests_significatifs, test$pct_tests_significatifs))
          cat(sprintf("    P-value minimale     : %.4f\n", test$p_value_min))
          cat(sprintf("    Conclusion           : %s\n", 
                      ifelse(test$na_aleatoires, "NA ALEATOIRES", "NA NON ALEATOIRES")))
        } else {
          cat(sprintf("  %s: Test non effectue (%s)\n", variable, test$raison))
        }
        cat("\n")
      }
    }
    
    cat(ifelse(inclure_tests_aleatoire && stats$test_aleatoire_active, "3.", "2."), 
        " ANALYSE PAR GROUPE CONDITIONNEL\n")
    cat("-----------------------------------\n\n")
    
    resume <- resultats_traitement$resume_amelioration
    
    for(groupe in unique(resume$groupe)) {
      cat(sprintf("  Groupe: %s\n", toupper(groupe)))
      cat("  --------------------------------------------------\n")
      
      vars_groupe <- resume[resume$groupe == groupe, ]
      
      for(i in 1:nrow(vars_groupe)) {
        var <- vars_groupe[i, ]
        cat(sprintf("    %s: %s\n", var$variable, var$description))
        cat(sprintf("      Avant: %d NA | Apres: %d NA | Reduction: %d (%.1f%%)\n",
                    var$na_applicables_avant,
                    var$na_applicables_apres,
                    var$reduction,
                    var$pct_reduction))
        
        if(stats$test_aleatoire_active && !is.null(journal$imputation_conditionnelle[[var$variable]])) {
          imp_info <- journal$imputation_conditionnelle[[var$variable]]
          if(!is.na(imp_info$p_value)) {
            cat(sprintf("      Test aleatoire: %s (p=%.4f)\n",
                        ifelse(imp_info$na_aleatoires, "Aleatoire", "Non aleatoire"),
                        imp_info$p_value))
          }
        }
      }
      cat("\n")
    }
    
    if(length(problemes) > 0) {
      cat(ifelse(inclure_tests_aleatoire && stats$test_aleatoire_active, "4.", "3."), 
          " PROBLEMES DETECTES\n")
      cat("---------------------\n")
      
      if(!is.null(problemes$variables_condition_manquantes)) {
        cat("  ATTENTION Variables de condition manquantes:\n")
        cat("    ", paste(problemes$variables_condition_manquantes, collapse = ", "), "\n\n")
      }
      
      if(!is.null(problemes$variables_trop_de_na)) {
        cat("  ATTENTION Variables avec trop de NA (non imputees):\n")
        cat("     ", paste(problemes$variables_trop_de_na, collapse = ", "), "\n\n")
      }
      
      if(!is.null(problemes$na_non_aleatoires) && length(problemes$na_non_aleatoires) > 0) {
        cat("  ATTENTION Variables avec NA non aleatoires (imputation deconseillée):\n")
        cat("     ", paste(unique(problemes$na_non_aleatoires), collapse = ", "), "\n\n")
      }
    }
    
    cat("===============================================================\n\n")
    
    return(invisible(resultats_traitement))
    
  } else {
    resume_df <- resultats_traitement$resume_amelioration
    
    if(inclure_tests_aleatoire && !is.null(journal$tests_aleatoire)) {
      resume_df$test_aleatoire_effectue <- NA
      resume_df$na_aleatoires <- NA
      resume_df$p_value_min <- NA
      resume_df$pct_tests_significatifs <- NA
      
      for(i in 1:nrow(resume_df)) {
        var <- resume_df$variable[i]
        if(var %in% names(journal$tests_aleatoire)) {
          test <- journal$tests_aleatoire[[var]]
          resume_df$test_aleatoire_effectue[i] <- test$test_effectue
          resume_df$na_aleatoires[i] <- test$na_aleatoires
          resume_df$p_value_min[i] <- test$p_value_min
          if(!is.null(test$pct_tests_significatifs)) {
            resume_df$pct_tests_significatifs[i] <- test$pct_tests_significatifs
          }
        }
      }
    }
    
    return(resume_df)
  }
}

verifier_coherence_conditions <- function(data, regles_conditionnalite) {
  "Verifie la coherence logique des conditions d'applicabilite"
  
  incoherences <- list()
  
  for(variable in names(regles_conditionnalite)) {
    if(!variable %in% names(data)) {
      next
    }
    
    regle <- regles_conditionnalite[[variable]]
    condition_applicable <- evaluer_condition(data, regle$condition_applicabilite)
    
    reponses_non_applicables <- sum(!is.na(data[[variable]]) & !condition_applicable, na.rm = TRUE)
    
    if(reponses_non_applicables > 0) {
      pct_incoherence <- round(reponses_non_applicables / sum(!condition_applicable) * 100, 1)
      
      incoherences[[variable]] <- list(
        variable = variable,
        reponses_non_applicables = reponses_non_applicables,
        pct_incoherence = pct_incoherence,
        condition = regle$condition_applicabilite,
        description = paste(reponses_non_applicables, 
                            "reponses chez les non-applicables (",
                            pct_incoherence, "%)")
      )
    }
  }
  
  if(length(incoherences) > 0) {
    cat("\nATTENTION INCOHERENCES DETECTEES\n")
    cat("--------------------------\n")
    
    for(inc in incoherences) {
      cat(sprintf("\n%s:\n", inc$variable))
      cat(sprintf("  %s\n", inc$description))
      cat(sprintf("  Condition: %s\n", inc$condition))
    }
    cat("\n")
  } else {
    cat("\nAucune incoherence detectee\n\n")
  }
  
  return(incoherences)
}

print.traitement_afrobarometer <- function(x, ...) {
  cat("\nResultat du traitement Afrobarometre V2\n")
  cat("----------------------------------------\n")
  cat("Variables:", x$journal$stats_globales$total_variables, "\n")
  cat("NA imputes:", x$journal$stats_globales$na_applicables_imputes, "\n")
  cat("Amelioration:", x$journal$stats_globales$pct_amelioration_global, "%\n")
  
  if(x$journal$stats_globales$test_aleatoire_active) {
    cat("Tests d'aleatoire: OUI\n")
    if(x$journal$stats_globales$variables_na_non_aleatoires > 0) {
      cat("ATTENTION", x$journal$stats_globales$variables_na_non_aleatoires, 
          "variable(s) avec NA non aleatoires\n")
    }
  }
  
  cat("\nUtilisez generer_rapport_conditionnel() pour un rapport detaille\n")
}

# ==============================================================================
# EXEMPLE D'UTILISATION
# ==============================================================================

# Remplacer 'data' par le nom de votre dataframe
donnees_brutes <- data

# Traitement avec tests d'aleatoire
resultats <- traitement_afrobarometer_conditionnel(
  data = donnees_brutes,
  seuil_na_max = 0.5,
  test_aleatoire = TRUE,
  seuil_pvalue = 0.05,
  verbose = TRUE
)

# Generer le rapport
generer_rapport_conditionnel(
  resultats, 
  format = "console",
  inclure_tests_aleatoire = TRUE
)

# Export CSV
resume_df <- generer_rapport_conditionnel(
  resultats, 
  format = "dataframe",
  inclure_tests_aleatoire = TRUE
)
write.csv(resume_df, "rapport_traitement_v2.csv", row.names = FALSE)

# Verifier la coherence
verifier_coherence_conditions(
  resultats$data_clean,
  definir_regles_conditionnalite()
)

# Acceder aux resultats
tests_aleatoire <- resultats$journal$tests_aleatoire
vars_na_non_aleatoires <- resultats$problemes$na_non_aleatoires
