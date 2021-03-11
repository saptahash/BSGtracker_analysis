#!/bin/bash
sudo apt-get install libcurl4-openssl-dev
sudo apt install -y libudunits2-0 libudunits2-dev
sudo apt install libgdal-dev
Rscript -e 'install.packages("remotes")'
Rscript -e 'remotes::install_cran(c("lubridate", "tidyverse", "readr", "data.table"))'
Rscript -e 'remotes::install_github("hadley/dplyr", build_vignettes = FALSE)'
