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

# Function to source a module only if it's active
source_if_active <- function(module_name, script_path) {
  if (module_name %in% active_modules$module &&
      active_modules$status[active_modules$module == module_name] == "active") {
    source(here("2_Scripts", script_path))
  }
}

# Source modules based on their status
source_if_active("mortality", "cbm_deaths.R")
source_if_active("pregnancy", "cbm_pregnancy.R")
source_if_active("health_seeking", "cbm_health_seeking.R")
source_if_active("vaccination", "cbm_vax.R")
source_if_active("food_security", "cbm_food_security.R")
source_if_active("water_access", "cbm_access_to_water.R")
source_if_active("violence", "cbm_violence.R")
source_if_active("financial_hardship", "cbm_financial_hardship.R")
source_if_active("mental_health", "cbm_mental_health.R")

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

