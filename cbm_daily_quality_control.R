#==============================================================================
# 
# Purpose: Generating daily quality control reports for CBM
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

# Clean the environment
rm(list = ls())

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)

# Install and load required packages to run script
pacman::p_load(httr,         
               jsonlite,
               dplyr,
               tidyr,
               stringr,
               lubridate,
               ggplot2,
               writexl,
               readxl,
               knitr,
               kableExtra,
               flextable, 
               officer,
               here,
               rmarkdown,
               conflicted)

# Setting up parameters 
source(here("2_Scripts", "cbm_parameters.R"))

# Data retrieval from Kobo
source(here("2_Scripts", "cbm_data_retrieval.R"))

# Data cleaning 
source(here("2_Scripts", "cbm_data_cleaning.R"))

# Creating secondary data frames
source(here("2_Scripts", "cbm_creating_data_frames.R"))

# Building tables
source(here("2_Scripts", "cbm_building_tables.R"))

#-------------------------------
# Produce QC report in R Markdown 
#-------------------------------

# Produce quality control report 
render(
  input = here("2_Scripts", "cbm_daily QC report.Rmd"),
  output_file = paste0(
    "C:/Users/msfe-jerusalem-epi/OneDrive - MSF/1_Epidemiology/3_Projects/2_DMC_2024/2_CBS/7_Quality control/CBM_Daily QC report_", 
    Sys.Date(), 
    ".docx"
  )
)

#==============================================================================
# End of code
#==============================================================================


