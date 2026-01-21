#==============================================================================
# 
# Purpose: Cleaning main dataframe
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Cleaning main data frame
#-------------------------------

# Create new edit data frame
data_edit <- data_raw
  
# Clean header names  
names(data_edit) <- str_replace_all(names(data_raw), "/", "_")

# Identify columns to be removed
columns_to_remove <- c("id", "formhub_uuid", "meta_instanceID", "_xform_id_string",
                        "_uuid", "_attachments", "_status", "_geolocation",
                         "_tags", "_notes")

# Remove columns not required
data_edit <- data_edit %>%
  dplyr::select(-all_of(columns_to_remove)) %>%
  rename(id = `_id`)

# Remove the prefixes from variable names 
names(data_edit) <- gsub("^interview_info_full_survey_", "", names(data_edit))
names(data_edit) <- gsub("^module_", "", names(data_edit))
names(data_edit) <- gsub("^hh_info_", "", names(data_edit))
names(data_edit) <- gsub("^interview_info_", "", names(data_edit))

data_edit <- data_edit %>%
  rename(violence_witnessed = violence_violence_witnessed) %>%
  rename(finances_factors = finances_finances_factors) %>%
  rename(water_difficulties = water_water_difficulties) %>%
  rename(water_source_drinking = water_water_source_drinking) %>%
  rename(disability = disability_disability) %>%
  rename(pregnancy_num = pregnancy_preg_num)   

# Create the 'CHW' variable by keeping the last 4 characters of 'username'
data_edit$CHW <- substr(data_edit$username, nchar(data_edit$username) - 3, nchar(data_edit$username))

# Manually changing the username for interviews done with epi phone
data_edit <- data_edit %>%
  mutate(CHW = case_when(
    CHW == "_epi" & community == "jinba" ~ "chw8",  
    community %in% c("wad_jwaya") ~ "chw5",  
    TRUE ~ CHW  
  ))

# Convert dates to proper date-time variables
data_edit$start <- ymd_hms(data_edit$start)
data_edit$end <- ymd_hms(data_edit$end)

# Create variable with interview duration 
data_edit$interview_duration <- round(as.numeric(difftime(data_edit$end, data_edit$start, units = "mins")), 2)

### Adding a suffix to the HH ID when there are duplicates ###

# Step 1: Count occurrences of each hh_id per CHW
data_edit <- data_edit %>%
  group_by(CHW, hh_id) %>%
  mutate(count = n()) %>%  # Count the number of occurrences for each hh_id
  ungroup()

# Step 2: Assign a suffix based on duplicate occurrences
data_edit <- data_edit %>%
  group_by(CHW, hh_id) %>%
  mutate(suffix = ifelse(count > 1, 
                         letters[rank(hh_id, ties.method = "first")],  # Assign letter based on rank order of hh_id
                         "")) %>%
  ungroup()

# Step 3: Create the final hh_id by combining the original hh_id and the suffix
data_edit <- data_edit %>%
  mutate(hh_id = paste0(hh_id, suffix)) %>%
  dplyr::select(-count, -suffix)  # Clean up temporary columns

# Changing character to numeric
data_edit$hh_size <- as.numeric(as.character(data_edit$hh_size))
data_edit$hh_adult_female <- as.numeric(as.character(data_edit$hh_adult_female))
data_edit$hh_adult_male <- as.numeric(as.character(data_edit$hh_adult_male))
data_edit$hh_teens <- as.numeric(as.character(data_edit$hh_teens))
data_edit$hh_children <- as.numeric(as.character(data_edit$hh_children))
data_edit$hh_infants <- as.numeric(as.character(data_edit$hh_infants))

# Calculate the total number of individuals under 5 (children + infants)
total_under_5 <- sum(data_edit$hh_children, na.rm = TRUE) + sum(data_edit$hh_infants, na.rm = TRUE)

# Calculate the total number of individuals (household size)
total_population <- sum(data_edit$hh_size, na.rm = TRUE)

# Calculate the percentage of individuals under 5
percentage_under_5 <- (total_under_5 / total_population) * 100


# Replace raw community names with clean community names from community_list
data_edit <- data_edit %>%
  mutate(
    community = ifelse(
      community %in% community_list$community,
      community_list$community_clean[match(community, community_list$community)],
      community  # Keep original name if not in community_list
    )
  )

# Clean up CHW names
data_edit <- data_edit %>%
  mutate(
    CHW = case_when(
      CHW == "chw3" ~ "CHW 3",
      CHW == "chw4" ~ "CHW 4",
      CHW == "chw5" ~ "CHW 5",
      CHW == "chw6" ~ "CHW 6",
      CHW == "chw7" ~ "CHW 7",
      CHW == "chw8" ~ "CHW 8",
      TRUE ~ CHW  # Keep all other CHW names as they are
    )
  )

#-------------------------------
# Creating subset for current round only
#-------------------------------

# Assign round number based on parameters in input file
data_edit <- data_edit %>%
  rowwise() %>%  # Ensures operation is row-wise
  mutate(round_num = round_dates$round_num[which(doi >= round_dates$start_date & doi <= round_dates$end_date)[1]]) %>%
  ungroup()

# Print the most recent round
most_recent_round <- max(data_edit$round_num, na.rm = TRUE)
print(most_recent_round)

# Create a subset of the dataframe with the most recent month_year
data_edit_current <- data_edit %>%
filter(round_num == most_recent_round)

#-------------------------------
# Export full data to Excel for back up
#-------------------------------

# Get the current date
current_date <- Sys.Date()

# Create the file path with the date appended
file_path <- paste0("C:/Users/msfe-jerusalem-epi/OneDrive - MSF/1_Epidemiology/3_Projects/2_DMC_2024/2_CBS/1_Data/2_2025/data_edit_", current_date, ".xlsx")

# Export to Excel
write_xlsx(data_edit, file_path)

#==============================================================================
# End of code
#==============================================================================

  