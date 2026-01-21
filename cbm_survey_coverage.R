#==============================================================================
# 
# Purpose: Summarizing survey coverage
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================
#-------------------------------
# Creating values for report
#-------------------------------

# Calculate the total number of households visited
total_households_full <- nrow(data_edit_current)

# Calculate the number of unique communities visited
total_communities <- data_edit_current %>%
  distinct(community) %>%
  nrow()

# Calculate the number of absent households (hh_present == "n")
absent_households <- data_edit_current %>%
  filter(hh_present == "n") %>%
  nrow()

# Calculate the number of households that declined participation (consent == "n")
declined_households <- data_edit_current %>%
  filter(consent == "n") %>%
  nrow()

# Calculate the number of households that completed the interview (consent == "y")
completed_households <- data_edit_current %>%
  filter(consent == "y") %>%
  nrow()

# Calculate the total number of individuals represented (sum of hh_size for completed households)
total_individuals <- data_edit_current %>%
  filter(consent == "y") %>%
  summarise(total = sum(hh_size, na.rm = TRUE)) %>%
  pull(total)

#-------------------------------
# Building table 1 - Overview per community
#-------------------------------

# Overall table with number of interviews done per community
overview_table <- data_edit_current %>%
  group_by(community) %>%
  summarise(
    num_households_visited = n_distinct(id),  # Count distinct hh_id (households visited)
    num_households_present = sum(hh_present == "y", na.rm = TRUE),  # Households present (hh_present = y)
    num_households_absent = sum(hh_present == "n", na.rm = TRUE),   # Households absent (hh_present = n)
    num_refusals = sum(consent == "n", na.rm = TRUE),  # Refusals (consent = n)
    num_households_interviewed = sum(consent == "y", na.rm = TRUE),  # Households interviewed (consent = y)
    percent_interviewed = round((num_households_interviewed / num_households_visited) * 100, 0), # Percent of HH interviewed rounded to whole number
    .groups = "drop"
  )

# Calculate totals (sum across all communities)
total_row <- overview_table %>%
  summarise(
    community = "Total",  # Set name of the total row
    num_households_visited = sum(num_households_visited, na.rm = TRUE),
    num_households_present = sum(num_households_present, na.rm = TRUE),
    num_households_absent = sum(num_households_absent, na.rm = TRUE),
    num_refusals = sum(num_refusals, na.rm = TRUE),
    num_households_interviewed = sum(num_households_interviewed, na.rm = TRUE),
    percent_interviewed = round((num_households_interviewed / num_households_visited) * 100, 0)
  )

# Combine the total row with the original table
overview_table_with_total <- bind_rows(overview_table, total_row)

# Create a flextable from the summary table with the total row
overview_table_flex <- flextable(overview_table_with_total) %>%
  set_header_labels(
    community = "Community",
    num_households_visited = "HH visited",
    num_households_present = "HH present",
    num_households_absent = "HH absent",
    num_refusals = "Refusals",
    num_households_interviewed = "HH interviewed",
    percent_interviewed = "% of HH interviewed"
  ) %>%
  colformat_int(
    j = c("num_households_visited", "num_households_present", "num_households_absent", "num_refusals", "num_households_interviewed", "percent_interviewed"),
    na_str = "NA"
  ) %>%
  # Set column widths to ensure all headers fit on one row
  width(
    j = c("community", "num_households_visited", "num_households_present", "num_households_absent", "num_refusals", "num_households_interviewed", "percent_interviewed"),
    width = 1.5  # Adjust width to fit the headers on one row
  ) %>%
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header")  %>% # Change font size for the header row
  # Adjust table properties for better spacing and alignment
  set_table_properties(
    width = 0.9,  # Adjust the width of the table
    layout = "autofit"  # Ensure the table adapts to the content
  ) 

# Display the flextable summary table with the total row
overview_table_flex


#-------------------------------
# Building table 2 - Progression over time
#-------------------------------

# Summarize the data by round
progression_table <- data_edit %>%
  group_by(round_num) %>%
  summarise(
    num_households_visited = n_distinct(id),  # Count distinct households visited
    num_households_present = sum(hh_present == "y", na.rm = TRUE),  # Households present
    num_households_absent = sum(hh_present == "n", na.rm = TRUE),   # Households absent
    num_refusals = sum(consent == "n", na.rm = TRUE),  # Refusals
    num_households_interviewed = sum(consent == "y", na.rm = TRUE),  # Households interviewed
    .groups = "drop"
  )

# Reshape the data to have indicators as rows and rounds as columns
progression_table_long <- progression_table %>%
  pivot_longer(
    cols = starts_with("num_"),  # Select all columns starting with "num_"
    names_to = "indicator",  # The column names will be the indicators
    values_to = "count"  # The values will be the counts
  ) %>%
  mutate(
    indicator = case_when(
      indicator == "num_households_visited" ~ "HH visited",
      indicator == "num_households_present" ~ "HH present",
      indicator == "num_households_absent" ~ "HH absent",
      indicator == "num_refusals" ~ "Refusals",
      indicator == "num_households_interviewed" ~ "HH interviewed",
      TRUE ~ indicator  # Keep original name if no match
    )
  )

# Reshape further to have the round as columns
progression_table_wide <- progression_table_long %>%
  pivot_wider(
    names_from = round_num,
    values_from = count
  )

# Create the flextable
overview_per_round <- flextable(progression_table_wide) %>%
  set_header_labels(indicator = "") %>%  # Remove first column title
  width(j = 1, width = 2) %>%  # Make first column wider
  width(j = 2:ncol(progression_table_wide), width = 1.5) %>%  # Set width for other columns
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header")  %>% # Change font size for the header row
  set_table_properties(layout = "autofit")  # Ensure table adapts to content

# Display the flextable
overview_per_round

#-------------------------------
# Removing interviews that did not consent for rest of report
#-------------------------------

# Filter the dataset for households that have consented to the survey
data_edit_current <- data_edit_current %>%
  filter(consent == "y")

data_edit <- data_edit %>%
  filter(consent == "y")

#==============================================================================
# End of code
#==============================================================================


