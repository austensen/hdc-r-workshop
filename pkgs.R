# Packages that have already been installed in this RStudio Cloud project

pkgs <- c(
  "tidyverse", # tidyverse installs a collection of many packages
  "DBI",
  "RPostgres",
  "getPass",
  "knitr",
  "rmarkdown",
  "radix",
  "remotes",
  "sf",
  "mapview"
)

install.packages(pkgs)

# This package isn't on the official R package repository (CRAN)
remotes::install_github("austensen/geoclient")
remotes::install_github("rstudio/radix")
