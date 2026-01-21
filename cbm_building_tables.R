#==============================================================================
# 
# Purpose: Building tables for QC report
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Table 1. Overview of target and coverage, per CHW 
#-------------------------------

# Define target households from previous round for each community 
round1_target <- community_list %>%
  mutate(community = community_clean) %>%
  select(-community_clean)

# Overall table with number of interviews done by CHW, now including target and progress
QC_summary_table <- data_edit_current %>%
  group_by(CHW, community) %>%
  summarise(
    num_households_visited = n_distinct(id),  # Count distinct hh_id (households visited)
    num_households_present = sum(hh_present == "y", na.rm = TRUE),  # Households present (hh_present = y)
    num_households_absent = sum(hh_present == "n", na.rm = TRUE),   # Households absent (hh_present = n)
    num_refusals = sum(consent == "n", na.rm = TRUE),  # Refusals (consent = n)
    num_households_interviewed = sum(consent == "y", na.rm = TRUE),  # Households interviewed (consent = y)
    .groups = "drop"
  ) %>%
  left_join(round1_target, by = "community") %>%  # Join with the round 1 target data to get household counts from round 1
  mutate(
    progress_towards_target = as.integer((num_households_visited / hh_count_round1) * 100),  # Convert to integer to remove decimals
    hh_count_round1 = as.integer(hh_count_round1)  # Convert to integer to remove decimals
  )

# Create a flextable from the summary table
QC_summary_table_flex <- flextable(QC_summary_table) %>%
  set_header_labels(
    CHW = "CHW",
    community = "Community",
    num_households_visited = "HH visited",
    num_households_present = "HH present",
    num_households_absent = "HH absent",
    num_refusals = "Refusals",
    num_households_interviewed = "HH interviewed",
    hh_count_round1 = "HH Count (Round 1)",
    progress_towards_target = "Progress (%)"
  ) %>%
  colformat_int(  # Use integer formatting to ensure no decimals
    j = c("hh_count_round1", "progress_towards_target", "num_households_visited", 
          "num_households_present", "num_households_absent", 
          "num_refusals", "num_households_interviewed"),
    na_str = "NA"
  )

QC_summary_table_flex

#-------------------------------
# Table 2. Interview duration, per CHW
#-------------------------------

# Create the summary table
QC_data_summary <- data_edit_current %>%
  filter(consent == "y", hh_present == "y") %>%
  group_by(CHW) %>%
  summarise(
    `Number of interviews` = n(),
    `Short interviews (<5 mins)` = sum(interview_duration < 5, na.rm = TRUE),
    `Median duration` = round(median(interview_duration, na.rm = TRUE), 1),
    `Mean duration` = round(mean(interview_duration, na.rm = TRUE), 1),
    `Std. dev.` = round(sd(interview_duration, na.rm = TRUE), 1)
  )

# Create a flextable
QC_interview_length_table <- flextable(QC_data_summary) %>%
  add_footer_lines("Note: Very short interviews (<5 mins) could indicate issues in data collection.") %>%
  fontsize(part = "footer", size = 8) %>%
  color(part = "footer", color = "red")

#-------------------------------
# Table 3. Interval between interviews, per CHW (previous 7 days)
#-------------------------------

# Calculate the interval between interviews, but only for CHWs with multiple interviews per day
QC_interval_summary <- data_edit_current %>%
  filter(consent == "y", hh_present == "y") %>%
  mutate(
    interview_date = as.Date(start)  # Extract the date part from the start time
  ) %>%
  filter(interview_date >= Sys.Date() - 7) %>%  # Include only the previous seven days
  group_by(CHW, interview_date) %>%  # Group by CHW and interview date
  filter(n() > 1) %>%  # Keep only days where there are multiple interviews for the same CHW
  arrange(CHW, interview_date, start) %>%  # Sort by CHW, interview date, and start time
  mutate(
    interval_between_interviews = as.numeric(difftime(start, lag(start), units = "mins"))  # Calculate interval in minutes
  ) %>%
  summarise(
    mean_interval = mean(interval_between_interviews, na.rm = TRUE),  # Mean interval
    median_interval = median(interval_between_interviews, na.rm = TRUE)  # Median interval
  )

# # Create a flextable from the interval_summary
QC_interval_table_flex <- flextable(QC_interval_summary) %>%
  set_header_labels(
    CHW = "CHW",
    interview_date = "Interview date",
    mean_interval = "Mean interval (mins)",
    median_interval = "Median interval (mins)"
  ) %>%
  compose(
    j = c("mean_interval"),  # Center-align the numeric columns
    value = as_paragraph(as_chunk(.data$mean_interval, align = "center"))
  ) %>%
  compose(
    j = c("median_interval"),  # Center-align the numeric columns
    value = as_paragraph(as_chunk(.data$median_interval, align = "center"))
  ) %>%
  compose(
    j = "interview_date",  # Left-align the date column
    value = as_paragraph(as_chunk(.data$interview_date, align = "left"))
  ) %>%
  autofit() %>%
  add_footer_lines(
    values = "Note: Very short intervals between interviews (<5 mins) could indicate issues in data collection."
  ) %>%
  color(part = "footer", color = "red") %>%
  fontsize(part = "footer", size = 8)

 #-------------------------------
 # Table 4. Proportion of ill or injured individuals, per CHW
 #-------------------------------
 
# Convert `health_ill_num` and `hh_size` to numeric and group by CHW
sick_percentage_summary <- data_edit_current %>%
  mutate(
    health_ill_num = as.numeric(health_ill_num),  # Convert to numeric
    hh_size = as.numeric(hh_size)                # Convert to numeric
  ) %>%
  group_by(CHW) %>%
  summarise(
    total_health_ill = sum(health_ill_num, na.rm = TRUE),  # Total number of sick individuals
    total_household_size = sum(hh_size, na.rm = TRUE),     # Total household size
    sick_percentage = (total_health_ill / total_household_size) * 100  # Sick percentage
  )

# Correct the values to ensure no decimals for specific columns
sick_percentage_summary <- sick_percentage_summary %>%
  mutate(
    total_health_ill = round(total_health_ill, 0),  # Ensure no decimals for sick individuals
    total_household_size = round(total_household_size, 0),  # Ensure no decimals for household size
    sick_percentage = round(sick_percentage, 2)  # Limit percentage to 2 decimal places
  )

# Create a flextable with left alignment
sick_percentage_table_flex <- flextable(sick_percentage_summary) %>%
  set_header_labels(
    CHW = "CHW",
    total_health_ill = "Ill or injured individuals",
    total_household_size = "Total individuals",
    sick_percentage = "Proportion ill or injured (%)"
  ) %>%
  autofit() %>%
  align(align = "center", part = "all") %>%  # Align content within cells
  set_table_properties(width = 0.8, align = "left") %>% # Align the table to the left of the page
  add_footer_lines(
    values = "Note: A low proportion of ill or injured individuals (<10%) could indicate issues in data collection."
  ) %>%
  color(part = "footer", color = "red") %>%
  fontsize(part = "footer", size = 8)

#-------------------------------
# Table 5. Proportion of children with MUAC measurement, per CHW
#-------------------------------

# Remove variables starting with the specified prefix
data_edit_muac <- data_muac %>%
  rename_with(~ gsub("^(interview_info/full_survey/children_nutrition/)", "", .x), everything())

# Creating subset with current round only
# Convert date to proper date-time variables
data_edit_muac$doi <- ymd(data_edit_muac$doi)

# Add the CHW column based on the community variable
data_edit_muac <- data_edit_muac %>%
  mutate(
    CHW = case_when(
      community == "Al Jawaya" ~ "CHW 5",
      community == "Al Majaz" ~ "CHW 6",
      community == "Halaweh" ~ "CHW 8",
      community == "Imnezil" ~ "CHW 4",
      community == "Jinba" ~ "CHW 8",
      community == "Mirkez" ~ "CHW 8",
      community == "Susya" ~ "CHW 3",
      community == "Umm Qussa" ~ "CHW 7",
      community == "Wad Rahim" ~ "CHW 3",
      community == "Khirbet Tabban" ~ "CHW 6",
      TRUE ~ NA_character_  # Assign NA if no match is found
    )
  )

# Assign round number based on parameters in input file
data_edit_muac <- data_edit_muac %>%
  rowwise() %>%  # Ensures operation is row-wise
  mutate(round_num = round_dates$round_num[which(doi >= round_dates$start_date & doi <= round_dates$end_date)[1]]) %>%
  ungroup()

# # Adding month-year to data frame
# data_edit_muac <- data_edit_muac %>%
#   left_join(round_dates %>% dplyr::select(round_num, month_year), by = "round_num")

# Create a subset of the dataframe with the current round only
data_edit_muac_current <- data_edit_muac %>%
  filter(round_num == most_recent_round)

# Summarize the data based on the 'CHW' variable
QC_summary_data <- data_edit_muac_current %>%
  group_by(CHW) %>%
  summarise(
    `Number of children (all)` = n(),  # Count total number of rows for each CHW
    `Number of MUAC measured` = sum(child_present == "y", na.rm = TRUE),  # Count where child_present == 'y'
    `Proportion measured (%)` = sum(child_present == "y", na.rm = TRUE) / n() * 100  # Proportion as percentage
  )

# Create the flextable
QC_ft_muac <- flextable(QC_summary_data)

# Round the proportion to 1 decimal places
QC_ft_muac <- QC_ft_muac %>%
  compose(j = "Proportion measured (%)", value = as_paragraph(round(QC_summary_data$`Proportion measured (%)`, 1)))

#==============================================================================
# End of code
#==============================================================================




