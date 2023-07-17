# Main script

library(renv)
library(rmarkdown)
library(quarto)

### Package / dependency version management with 'renv'
# only ran this to initialize 'renv'
# renv::init()
# renv::snapshot()
# renv::restore() # to revert to the previous state as encoded in the lockfile

### generate data from SHARE data files (created by AS)
rmarkdown::render("01_datacleaning.Rmd")

### run analysis scripts
quarto_render("02_analyses.qmd")