#==============================================================================
# 
# Purpose: Generating end-of-round CBM reports
# Date created: February 2025
# Author: Laurence Campeau
#
#==============================================================================

# Clean the environment
rm(list = ls())

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)

# Install and load required packages to run scripts
pacman::p_load(here,
               httr,         
               jsonlite,
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
               patchwork,
               grid,
               gridExtra,
               RColorBrewer,
               sf,
               leaflet,
               dplyr, 
               conflicted)

# Setting up parameters 
source(here("2_Scripts", "cbm_parameters.R"))

# Data retrieval from Kobo
source(here("2_Scripts", "cbm_data_retrieval.R"))

# Cleaning main data frame 
source(here("2_Scripts", "cbm_data_cleaning.R"))

# Creating secondary data frames
source(here("2_Scripts", "cbm_creating_data_frames.R"))

# Survey coverage
source(here("2_Scripts", "cbm_survey_coverage.R"))

# Demographics
source(here("2_Scripts", "cbm_demographics.R"))

# Mortality
source(here("2_Scripts", "cbm_deaths.R"))

# Pregnancy module
source(here("2_Scripts", "cbm_pregnancy.R"))

# Health seeking behaviors module
source(here("2_Scripts", "cbm_health_seeking.R"))

# Vaccination module
source(here("2_Scripts", "cbm_vax.R"))

# Food security and nutrition module
source(here("2_Scripts", "cbm_food_security.R"))

# Mental health
source(here("2_Scripts", "cbm_mental_health.R"))

# Access to water module
source(here("2_Scripts", "cbm_access_to_water.R"))

# Experiences of violence
source(here("2_Scripts", "cbm_violence.R"))

# Financial hardship
source(here("2_Scripts", "cbm_financial_hardship.R"))

#-------------------------------
# Produce end-of-round report in R Markdown 
#-------------------------------

render(
  input = here("2_Scripts", "cbm_end of round report.Rmd"),
  output_file = paste0(
    "C:/Users/msfe-jerusalem-epi/OneDrive - MSF/1_Epidemiology/3_Projects/2_DMC_2024/2_CBS/6_Reports/CBM_End of round report_", 
    Sys.Date(), 
    ".docx"
  )
)

#==============================================================================
# End of code
#==============================================================================

