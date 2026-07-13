# Lancer la plateforme de visualisation
# En ligne de commande : Rscript shiny/run_app.R
# En interactif        : source("shiny/run_app.R")

pkgs <- c("shiny", "bslib", "dplyr", "ggplot2", "tidyr", "haven", "here",
          "DT", "purrr", "stringr", "scales")
manquants <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(manquants) > 0) {
  stop("Packages manquants : ", paste(manquants, collapse = ", "),
       "\nInstallez avec : install.packages(c('",
       paste(manquants, collapse = "','"), "'))")
}

shiny::runApp(
  appDir = here::here("shiny"),
  host   = "0.0.0.0",
  port   = 3838,
  launch.browser = interactive()
)
