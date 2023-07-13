# Main script

### Package / dependency version management with 'renv'
# only ran this to initialize 'renv'
# renv::init()
# renv::snapshot()
# renv::restore() # to revert to the previous state as encoded in the lockfile

### generate data from SHARE data files 
source("01_datacleaning.Rmd")

### run analysis scripts
# source("02_analyses.qmd")