#==============================================================================
# 
# Purpose: Extracting parameters
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

# Extract today's date for file names
date_output <- Sys.Date()

# Set preference for package when conflict 
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("lag", "dplyr")

#-------------------------------
# List of communities
#-------------------------------

community_list <- read_excel(here("2_Scripts", "1_Input_Files", "community_list.xlsx"))

#-------------------------------
# Round calendar 
#-------------------------------

round_dates <- read_excel(here("2_Scripts", "1_Input_Files","round_calendar.xlsx"))

round_dates <- round_dates %>%
  mutate(start_date = as.Date(start_date, format = "%Y-%m-%d")) %>%
  mutate(end_date = as.Date(end_date, format = "%Y-%m-%d")) %>%
  mutate(round_num = paste("Round", round_num))

#-------------------------------
# Active modules
#-------------------------------

active_modules <- read_excel(here("2_Scripts", "1_Input_Files", "active_modules.xlsx"))

pregnancy_active <- "pregnancy" %in% active_modules$module &&
  active_modules$status[active_modules$module == "pregnancy"] == "active"

#==============================================================================
# End of code
#==============================================================================

