#==============================================================================
# 
# Purpose: Analyzing pregnancy data
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Cleaning data frame
#-------------------------------

# Remove variables starting with the specified prefix
data_edit_preg <- data_preg %>%
  rename_with(~ gsub("^(interview_info/full_survey/pregnancy_info/)", "", .x), everything())

#-------------------------------
# Creating subset with current round only
#-------------------------------

# Convert date to proper date-time variables
data_edit_preg$doi <- ymd(data_edit_preg$doi)

# Assign round number based on parameters in input file
data_edit_preg <- data_edit_preg %>%
  rowwise() %>%  # Ensures operation is row-wise
  mutate(round = round_dates$round_num[which(doi >= round_dates$start_date & doi <= round_dates$end_date)[1]]) %>%
  ungroup()

# Create a subset of the dataframe with the current round only
data_edit_preg_current <- data_edit_preg %>%
  filter(round == most_recent_round)

#-------------------------------
# Creating variables
#-------------------------------

# Calculating fields for the R markdown 
# Number of pregnancies
num_preg <- nrow(data_edit_preg_current)

# Number and proportion of live births
live_births <- sum(data_edit_preg_current$preg_outcome == "livebirth", na.rm = TRUE)
live_births_percent <- round((live_births / num_preg) * 100, 1)

# Number and proportion of still births
still_births <- sum(data_edit_preg_current$preg_outcome == "stillbirth", na.rm = TRUE)
still_births_percent <- round((still_births / num_preg) * 100, 1)

# Number and proportion of miscarriages
miscarriages <- sum(data_edit_preg_current$preg_outcome == "miscarriage_abortion", na.rm = TRUE)
miscarriages_percent <- round((miscarriages / num_preg) * 100, 1)

# Delivered in hospital
hosp_delivery <- sum(data_edit_preg_current$preg_location == "hospital", na.rm = TRUE)
hosp_delivery_percent <- round((hosp_delivery / num_preg) * 100, 1)

# Attended ANC
ANC_attended <- sum(data_edit_preg_current$preg_anc == "y", na.rm = TRUE)
ANC_attended_percent <- round((ANC_attended / num_preg) * 100, 1)

# Calculate the average number of consultations
data_edit_preg_current <- data_edit_preg_current %>%
  mutate(preg_anc_num = as.numeric(preg_anc_num))
num_consultations <- round(mean(data_edit_preg_current$preg_anc_num, na.rm = TRUE), 1)

# Attended PNC
PNC_attended <- sum(data_edit_preg_current$preg_pnc == "y", na.rm = TRUE)
PNC_attended_percent <- round((PNC_attended / num_preg) * 100, 1)

#-------------------------------
# Building Table: Pregnancy Outcomes per Round
#-------------------------------

# Summarize the data by round
pregnancy_table <- data_edit_preg %>%
  group_by(round) %>%
  summarise(
    num_pregnancy_outcomes = n(),  # Total number of pregnancy outcomes
    live_births = sum(preg_outcome == "livebirth", na.rm = TRUE),  # Total live births
    still_births = sum(preg_outcome == "stillbirth", na.rm = TRUE),  # Total still births
    miscarriages_abortions = sum(preg_outcome == "miscarriage_abortion", na.rm = TRUE),  # Total miscarriages/abortions
    hosp_delivery_percent = round(sum(preg_location == "hospital", na.rm = TRUE) / n() * 100),  # Proportion delivered in hospital
    ANC_attended_percent = round(sum(preg_anc == "y", na.rm = TRUE) / n() * 100),  # Proportion attended ANC
    avg_ANC_consultations = round(mean(as.numeric(preg_anc_num), na.rm = TRUE), 1),  # Average number of ANC consultations
    PNC_attended_percent = round(sum(preg_pnc == "y", na.rm = TRUE) / n() * 100, 1),  # Proportion attended PNC (retain decimals)
    .groups = "drop"
  ) %>%
  # Convert non-decimal metrics to integers to remove decimals
  mutate(
    num_pregnancy_outcomes = as.integer(num_pregnancy_outcomes),
    live_births = as.integer(live_births),
    still_births = as.integer(still_births),
    miscarriages_abortions = as.integer(miscarriages_abortions),
    hosp_delivery_percent = as.integer(hosp_delivery_percent),
    ANC_attended_percent = as.integer(ANC_attended_percent)
  )

# Reshape the data for better table structure
pregnancy_table_long <- pregnancy_table %>%
  pivot_longer(
    cols = -round,  # All columns except "round"
    names_to = "indicator",  # Column names become indicators
    values_to = "value"  # Values populate the table
  )

# Reshape further to have rounds as columns
pregnancy_table_wide <- pregnancy_table_long %>%
  pivot_wider(
    names_from = round,
    values_from = value
  )

# Clean up indicator names
pregnancy_table_wide <- pregnancy_table_wide %>%
  mutate(
    indicator = case_when(
      indicator == "num_pregnancy_outcomes" ~ "Number of Pregnancy Outcomes",
      indicator == "live_births" ~ "Live Births",
      indicator == "still_births" ~ "Still Births",
      indicator == "miscarriages_abortions" ~ "Miscarriages/Abortions",
      indicator == "hosp_delivery_percent" ~ "Delivered in Hospital (%)",
      indicator == "ANC_attended_percent" ~ "Attended ANC (%)",
      indicator == "avg_ANC_consultations" ~ "Average Number of ANC Consultations",
      indicator == "PNC_attended_percent" ~ "Attended PNC (%)",
      TRUE ~ indicator  # Keep original name if no match
    )
  )

# Create the flextable
pregnancy_evolution_flextable <- flextable(pregnancy_table_wide) %>%
  set_header_labels(indicator = "") %>%  # Remove the first column title
  width(j = 1, width = 2) %>%  # Make the first column wider
  width(j = 2:ncol(pregnancy_table_wide), width = 1.5) %>%  # Set width for other columns
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header") %>%  # Change font size for the header row
  set_table_properties(layout = "autofit")  # Ensure the table adapts to content

# Print the table
pregnancy_evolution_flextable


#==============================================================================
# End of code
#==============================================================================