#!/usr/bin/Rscript

# chmod 0755 render_hydrocarbon_plots.R

# $ sudo wget -P /etc/yum.repos.d/ https://copr.fedoraproject.org/coprs/petersen/pandoc-el5/repo/epel-5/petersen-pandoc-el5-epel-5.repo
# $ yum install pandoc pandoc-citeproc

# devtools::install_github("rstudio/rmarkdown")

# Set-up
library(plyr)
library(rmarkdown)
library(tidyr)

setwd("/home/stuartg/hydrocarbons_plotting")

# Render html documents
file_markdown <- list.files("/home/stuartg/hydrocarbons_plotting", pattern = ".Rmd")
l_ply(file_markdown, render, output_format = "html_document", quiet = TRUE)

# Copy files
message("Copying files to webserver...")
system("rsync -r /home/stuartg/hydrocarbons_plotting/libs /srv/shiny-server/plots/")
system("rsync -a --include=*.html --exclude=* /home/stuartg/hydrocarbons_plotting/ /srv/shiny-server/plots/")
