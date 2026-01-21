#==============================================================================
# 
# Purpose: Demographics 
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Creating variables
#-------------------------------

# Average household size
average_household_size <- round(mean(data_edit_current$hh_size, na.rm = TRUE), 1)
min_household_size <- min(data_edit_current$hh_size, na.rm = TRUE)
max_household_size <- max(data_edit_current$hh_size, na.rm = TRUE)

# Percentage female
female_percentage <- round(
  sum(data_edit_current$hh_adult_female, na.rm = TRUE) /
    (sum(data_edit_current$hh_adult_female, na.rm = TRUE) + 
       sum(data_edit_current$hh_adult_male, na.rm = TRUE)) * 100, 1)

# Female-headed households
female_headed_households_percentage <- round(
  sum(!is.na(data_edit_current$hh_adult_male) & 
        !is.na(data_edit_current$hh_adult_female) & 
        data_edit_current$hh_adult_male == 0 & 
        data_edit_current$hh_adult_female > 0) /
    sum(!is.na(data_edit_current$hh_adult_male) & 
          !is.na(data_edit_current$hh_adult_female)) * 100, 1)

# Proportion under 5
under_5_percentage <- round(
  sum(data_edit_current$hh_children + 
        data_edit_current$hh_infants, na.rm = TRUE) /
    sum(data_edit_current$hh_size, na.rm = TRUE) * 100, 1)
under_5_count <- sum(data_edit_current$hh_children + data_edit_current$hh_infants, na.rm = TRUE)

# Proportion 5-17 years old
aged_5_to_17_percentage <- round(
  sum(data_edit_current$hh_teens, na.rm = TRUE) /
    sum(data_edit_current$hh_size, na.rm = TRUE) * 100, 1)
aged_5_to_17_count <- sum(data_edit_current$hh_teens, na.rm = TRUE)

# Disabilities 
disability_percentage <- round(
  nrow(filter(data_edit_current, disability == "y")) /
    nrow(filter(data_edit_current, disability %in% c("y", "n"))) * 100, 1)
disability_count <- nrow(filter(data_edit_current, disability == "y"))

# Calculate the number and proportion of cases with a disability per community
disability_stats_per_community <- data_edit_current %>%
  filter(disability %in% c("y", "n")) %>%
  group_by(Community = community) %>%
  summarise(
    `Households with disabled person` = sum(disability == "y"),
    `Number of households` = n(),
    `% of households with disabled person` = round((`Households with disabled person` / `Number of households`) * 100, 1)
  ) %>%
  arrange(desc(`% of households with disabled person`))

# Create a flextable
disability_stats_table <- flextable(disability_stats_per_community) %>%
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header")  %>% # Change font size for the header row 
  width(j = 1:4, width = 1.2)  # Adjust column width


#-------------------------------
# Building Table: Summary Statistics per Time Period
#-------------------------------

# Summarize the data by round_num
summary_table <- data_edit %>%
  group_by(round_num) %>%
  summarise(
    average_household_size = round(mean(hh_size, na.rm = TRUE), 1),
    female_percentage = round(
      sum(hh_adult_female, na.rm = TRUE) /
        (sum(hh_adult_female, na.rm = TRUE) + sum(hh_adult_male, na.rm = TRUE)) * 100, 1),
    female_headed_households_percentage = round(
      sum(!is.na(hh_adult_male) & !is.na(hh_adult_female) &
            hh_adult_male == 0 & hh_adult_female > 0) /
        sum(!is.na(hh_adult_male) & !is.na(hh_adult_female)) * 100, 1),
    under_5_percentage = round(
      sum(hh_children + hh_infants, na.rm = TRUE) /
        sum(hh_size, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# Reshape the data for a better table structure
summary_table_long <- summary_table %>%
  pivot_longer(
    cols = -round_num,  # All columns except "round_num"
    names_to = "indicator",  # The column names become indicators
    values_to = "value"  # The values populate the table
  )

# Reshape further to have rounds as columns
summary_table_wide <- summary_table_long %>%
  pivot_wider(
    names_from = round_num,
    values_from = value
  )

# Clean up table names
summary_table_wide <- summary_table_wide %>%
  mutate(
    indicator = case_when(
      indicator == "average_household_size" ~ "Average HH size",
      indicator == "female_percentage" ~ "Female %",
      indicator == "female_headed_households_percentage" ~ "Female-headed HH %",
      indicator == "under_5_percentage" ~ "U5 %",
      TRUE ~ indicator  # Keep original name if no match
    )
  )

# Create the flextable
demographics_per_round <- flextable(summary_table_wide) %>%
  set_header_labels(indicator = "") %>%  # Remove the first column title
  width(j = 1, width = 2) %>%  # Make the first column wider
  width(j = 2:ncol(summary_table_wide), width = 1.5) %>%  # Set width for other columns
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header") %>%  # Change font size for the header row
  set_table_properties(layout = "autofit")  # Ensure the table adapts to content

# Print the table
demographics_per_round

#==============================================================================
# End of code
#==============================================================================
