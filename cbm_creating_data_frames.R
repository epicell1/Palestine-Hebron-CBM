#==============================================================================
# 
# Purpose: Creating dataframes and backing up data
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Creating data frames
#-------------------------------

# Mental health module
data_mh <- data_edit %>% 
  dplyr::select(id, community, doi, mh) %>%
  unnest(mh)

# Pregnancy module
data_preg <- data_edit %>%
  dplyr::select(id, community, doi, pregnancy_info) %>%
  unnest(pregnancy_info)

# Health module
data_health <- data_edit %>%
  dplyr::select(id, community, doi, health_info) %>%
  unnest(health_info)

# Vax module
data_vax <- data_edit %>%
  dplyr::select(id, community, doi, vax) %>%
  unnest(vax)

# MUAC module
data_muac <- data_edit %>%
  dplyr::select(id, community, doi, children_nutrition) %>%
  unnest(children_nutrition)

# Death module
data_deaths <- data_edit %>%
  dplyr::select(id, community, doi, mortality_death_num, death_info) %>%
  unnest(death_info)


#-------------------------------
# Saving data frames
#-------------------------------

# Identify date based on system settings
today <- format(Sys.Date(), "%Y_%m_%d")

# Construct file names
file_name_data <- paste0("cbm_data_", today, ".xlsx")

file_name_mh <- paste0("cbm_mh_", today, ".xlsx")

file_name_preg <- paste0("cbm_preg_", today, ".xlsx")

file_name_health <- paste0("cbm_health_", today, ".xlsx")

file_name_vax <- paste0("cbm_vax_", today, ".xlsx")

file_name_muac <- paste0("cbm_muac_", today, ".xlsx")

file_name_deaths <- paste0("cbm_deaths_", today, ".xlsx")

# Specify the save location
save_location <- "C:/Users/msfe-jerusalem-epi/OneDrive - MSF/1_Epidemiology/3_Projects/2_DMC_2024/2_CBS/1_Data"

# Save the dataframes 

tryCatch({
  write_xlsx(data_edit, path = file.path(save_location, file_name_data))
  print(paste("Successfully saved:", file_name_data))
}, error = function(e) {
  print(paste("Error saving", file_name_data, ":", e))
})

# Save data_mh
tryCatch({
  write_xlsx(data_mh, path = file.path(save_location, file_name_mh))
  print(paste("Successfully saved:", file_name_mh))
}, error = function(e) {
  print(paste("Error saving", file_name_mh, ":", e))
})

# Save data_preg
tryCatch({
  write_xlsx(data_preg, path = file.path(save_location, file_name_preg))
  print(paste("Successfully saved:", file_name_preg))
}, error = function(e) {
  print(paste("Error saving", file_name_preg, ":", e))
})

# Save data_health
tryCatch({
  write_xlsx(data_health, path = file.path(save_location, file_name_health))
  print(paste("Successfully saved:", file_name_health))
}, error = function(e) {
  print(paste("Error saving", file_name_health, ":", e))
})

# Save data_vax
tryCatch({
  write_xlsx(data_vax, path = file.path(save_location, file_name_vax))
  print(paste("Successfully saved:", file_name_vax))
}, error = function(e) {
  print(paste("Error saving", file_name_vax, ":", e))
})

# Save data_muac
tryCatch({
  write_xlsx(data_muac, path = file.path(save_location, file_name_muac))
  print(paste("Successfully saved:", file_name_muac))
}, error = function(e) {
  print(paste("Error saving", file_name_muac, ":", e))
})

# Save data_deaths
tryCatch({
  write_xlsx(data_deaths, path = file.path(save_location, file_name_deaths))
  print(paste("Successfully saved:", file_name_deaths))
}, error = function(e) {
  print(paste("Error saving", file_name_deaths, ":", e))
})

#==============================================================================
# End of code
#==============================================================================