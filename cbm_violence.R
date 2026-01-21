#==============================================================================
# 
# Purpose: Analyze data on experiences of violence
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Clean data
#-------------------------------

# Filter the dataset for households that have consented to the survey
surveyed_households <- data_edit_current %>%
  filter(consent == "y")

#-------------------------------
# Proportion having experienced violence
#-------------------------------

# Filter the dataset to exclude 'prefer_not_to_answer'
filtered_responses <- data_edit_current %>%
  filter(violence_witness_yesno != "prefer_not_to_answer")

# Calculate the total number of filtered responses
total_filtered_responses <- nrow(filtered_responses)

# Filter the dataset those who answered 'witnessed', 'experienced', and 'witnessed_and_experienced')
valid_responses <- filtered_responses %>%
  filter(violence_witness_yesno %in% c("witnessed", "experienced", "witnessed_and_experienced"))

# Calculate the number of valid responses
valid_responses_count <- nrow(valid_responses)

# Calculate the percentage of valid responses over the total number of filtered responses
valid_responses_percentage <- round((valid_responses_count / total_filtered_responses) * 100, 1)

# Calculate the number and proportion of people who experienced violence
experienced_responses <- filtered_responses %>%
  filter(violence_witness_yesno %in% c("experienced", "witnessed_and_experienced"))
experienced_count <- nrow(experienced_responses)
experienced_percentage <- round((experienced_count / total_filtered_responses) * 100, 1)

# Calculate the number and proportion of people who witnessed violence
witnessed_responses <- filtered_responses %>%
  filter(violence_witness_yesno %in% c("witnessed", "witnessed_and_experienced"))
witnessed_count <- nrow(witnessed_responses)
witnessed_percentage <- round((witnessed_count / total_filtered_responses) * 100, 1)

#-------------------------------
# Calculate the number of violent events per household
#-------------------------------

# Count occurrences of each type of violence experienced by each household
experienced_events_per_household <- data_edit_current %>%
  filter(!is.na(violence_violence_exp)) %>%
  separate_rows(violence_violence_exp, sep = " ") %>%  # Use space as the separator
  filter(violence_violence_exp != "prefer_not_answer") %>%
  group_by(id) %>%
  summarise(number_of_violent_events = n())

# Calculate the frequency and proportion of each number of violent events
frequency_of_events <- experienced_events_per_household %>%
  count(number_of_violent_events) %>%  # Count how many households fall into each category
  rename(number_of_people = n) %>%
  mutate(
    proportion = round((number_of_people / sum(number_of_people)) * 100, 2)  # Calculate proportion
  ) %>%
  arrange(number_of_violent_events)  # Sort by the number of violent events

# Merge the counts back with the original dataset (if needed)
merged_violence_data <- data_edit_current %>%
  left_join(experienced_events_per_household, by = "id")

#-------------------------------
# Types of violent events
#-------------------------------

# Create a named vector for clean labels
violence_labels <- c(
  threats = "Threats and/or intimidation",
  assault = "Physical assault (e.g. beating or stabbing)",
  violence_children = "Harassment or violence towards children",
  arrests = "Arrest or detention without warrant",
  destruction_property = "Destruction of personal property (e.g., vehicles, equipment)",
  theft = "Robbery/theft",
  shooting = "Shooting",
  home_demolished = "Home demolition",
  displacement = "Forced displacement or evacuation",
  incursion = "Incursion",
  checkpoint = "Checkpoint harassment or delay",
  livestock = "Attacks on agriculture or livestock",
  sexual_violence = "Sexual violence",
  killing = "Killing",
  other = "Other",
  prefer_not_answer = "Prefer not to answer"
)

# Calculate the total number of valid responses (excluding 'prefer_not_to_answer')
total_responses <- data_edit_current %>%
  filter(violence_witness_yesno != "prefer_not_to_answer") %>%
  nrow()

# Count occurrences of each type of violence experienced, excluding 'prefer_not_answer'
violence_exp_summary <- data_edit_current %>%
  filter(!is.na(violence_violence_exp)) %>%
  separate_rows(violence_violence_exp, sep = " ") %>%  # Use space as the separator
  filter(violence_violence_exp != "prefer_not_answer") %>%
  count(violence_violence_exp) %>%
  mutate(
    proportion = round((n / total_responses) * 100, 2),
    clean_labels = violence_labels[violence_violence_exp],
    type = "Experienced"
  )

# Count occurrences of each type of violence witnessed, excluding 'prefer_not_answer'
violence_witnessed_summary <- data_edit_current %>%
  filter(!is.na(violence_witnessed)) %>%
  separate_rows(violence_witnessed, sep = " ") %>%  # Use space as the separator
  filter(violence_witnessed != "prefer_not_answer") %>%
  count(violence_witnessed) %>%
  mutate(
    proportion = round((n / total_responses) * 100, 2),
    clean_labels = violence_labels[violence_witnessed],
    type = "Witnessed"
  )

# Combine the two summaries into one dataframe
violence_summary <- bind_rows(violence_exp_summary, violence_witnessed_summary)

# Combine experienced and witnessed into one column
violence_combined <- data_edit_current %>%
  mutate(
    all_violence = paste(violence_violence_exp, violence_witnessed, sep = " ")
  ) %>%
  filter(!is.na(all_violence)) %>%
  separate_rows(all_violence, sep = " ") %>%  # Separate all violence events
  filter(all_violence != "prefer_not_answer") %>%
  distinct(id, all_violence)  # Ensure each individual (unique id) is counted only once per type of violence

# Count occurrences of each type of violence, ensuring no double-counting
violence_summary2 <- violence_combined %>%
  count(all_violence) %>%
  mutate(
    proportion = round((n / total_responses) * 100, 2),
    clean_labels = violence_labels[all_violence],
    type = "Experienced or Witnessed"
  )

# Create a horizontal bar chart with light blue bars for experienced and dodgerblue4 bars for witnessed
violence_horizontal_bar_chart <- ggplot(violence_summary, aes(x = reorder(clean_labels, proportion), y = proportion, fill = type)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge(width = 0.7)) +  # Bars next to each other with dodge position
  labs(y = "Proportion of Respondents (%)", x = NULL) +  # Axis labels
  scale_fill_manual(values = c("Experienced" = "dodgerblue4", "Witnessed" = "lightblue")) +  # Custom fill colors
  scale_y_continuous(expand = c(0, 0)) +  # Remove extra space around bars
  theme_minimal() +  # Clean theme
  theme(axis.text.y = element_text(size = 12),  # Reduce y-axis text size
        axis.text.x = element_text(size = 12),  # Reduce x-axis text size
        legend.position = c(0.85, 0.1),  # Position legend inside the plot at bottom right
        legend.title = element_blank(),  # Remove legend title
        plot.margin = margin(5, 5, 5, 5)) +  # Reduce margins
  coord_flip()  # Flip for horizontal bars

#-------------------------------
# Perpetrators of violent events
#-------------------------------

# # List of variables to analyze
# variables <- c(
#   "violence_threats_wit_who", "violence_assault_wit_who", "violence_violence_child_wit_who",
#   "violence_arrest_wit_who", "violence_destruction_wit_who", "violence_theft_wit_who",
#   "violence_shooting_wit_who", "violence_home_wit_who", "violence_displacement_wit_who",
#   "violence_incursion_wit_who", "violence_checkpoint_wit_who", "violence_livestock_wit_who",
#   "violence_sexual_violence_wit_who", "violence_killing_wit_who",
#   "violence_threats_exp_who", "violence_assault_exp_who", "violence_violence_child_exp_who",
#   "violence_arrest_exp_who", "violence_destruction_exp_who", "violence_theft_exp_who",
#   "violence_shooting_exp_who", "violence_home_exp_who", "violence_displacement_exp_who",
#   "violence_incursion_exp_who", "violence_checkpoint_exp_who", "violence_livestock_exp_who",
#   "violence_sexual_violence_exp_who", "violence_killing_exp_who")

# List of variables to analyze
# Removed violence_violence_home_wit_who, violence_violence_home_exp_who, violence_displacement_exp_who, violence_displacement_wit_who, 
# violence_incursion_wit_who, violence_incursion_exp_who, violence_sexual_violence_exp_who, violence_sexual_violence_wit_who,
# violence_killing_exp_who, violence_killing_wit_who
variables <- c(
  "violence_threats_wit_who", "violence_assault_wit_who", "violence_violence_child_wit_who",
  "violence_arrest_wit_who", "violence_destruction_wit_who", "violence_theft_wit_who",
  "violence_shooting_wit_who", "violence_checkpoint_wit_who", "violence_livestock_wit_who",
  "violence_threats_exp_who", "violence_assault_exp_who", "violence_violence_child_exp_who",
  "violence_arrest_exp_who", "violence_destruction_exp_who", "violence_theft_exp_who",
  "violence_shooting_exp_who", "violence_checkpoint_exp_who", "violence_livestock_exp_who"
)

# Answer options
answer_options <- c("soldier", "settler_civilian", "settler_military", "unsure", "other")

# Combine all specified variables into a single long-format dataframe
long_data <- data_edit_current %>%
  dplyr::select(all_of(variables)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response")

# Remove NA values
long_data <- long_data %>%
  filter(!is.na(response))

# Calculate the overall count and proportion for each category
proportion_df <- long_data %>%
  group_by(response) %>%
  summarise(count = n()) %>%
  mutate(proportion = round((count / sum(count)) * 100, 1))

# Create values for R Markdown for each count and proportion
soldier_count <- proportion_df %>% filter(response == "soldier") %>% pull(count)
soldier_proportion <- proportion_df %>% filter(response == "soldier") %>% pull(proportion)

settler_civilian_count <- proportion_df %>% filter(response == "settler_civilian") %>% pull(count)
settler_civilian_proportion <- proportion_df %>% filter(response == "settler_civilian") %>% pull(proportion)

settler_military_count <- proportion_df %>% filter(response == "settler_military") %>% pull(count)
settler_military_proportion <- proportion_df %>% filter(response == "settler_military") %>% pull(proportion)

settler_soldier_count <- proportion_df %>% filter(response == "settler_soldier") %>% pull(count)
settler_soldier_proportion <- proportion_df %>% filter(response == "settler_soldier") %>% pull(proportion)
                                                                                               
unsure_count <- proportion_df %>% filter(response == "unsure") %>% pull(count)
unsure_proportion <- proportion_df %>% filter(response == "unsure") %>% pull(proportion)


#-------------------------------
# Building Table: Experiences with Violent Events per Round
#-------------------------------

# Filter for valid responses (exclude 'prefer_not_to_answer') and summarize by round_num
violence_table <- data_edit %>%
  filter(violence_witness_yesno != "prefer_not_to_answer") %>%
  group_by(round_num) %>%
  summarise(
    total_respondents = n(),  # Total number of valid respondents
    witnessed_percent = round(
      sum(violence_witness_yesno %in% c("witnessed", "witnessed_and_experienced"), na.rm = TRUE) / total_respondents * 100, 1
    ),  # Proportion who witnessed events
    experienced_percent = round(
      sum(violence_witness_yesno %in% c("experienced", "witnessed_and_experienced"), na.rm = TRUE) / total_respondents * 100, 1
    ),  # Proportion who experienced events
    witnessed_or_experienced_percent = round(
      sum(violence_witness_yesno %in% c("witnessed", "experienced", "witnessed_and_experienced"), na.rm = TRUE) / total_respondents * 100, 1
    ),  # Proportion who either witnessed or experienced events
    .groups = "drop"
  )

# Reshape the data for better table structure
violence_table_long <- violence_table %>%
  pivot_longer(
    cols = -round_num,  # All columns except "round_num"
    names_to = "indicator",  # Column names become indicators
    values_to = "value"  # Values populate the table
  )

# Reshape further to have round numbers as columns
violence_table_wide <- violence_table_long %>%
  pivot_wider(
    names_from = round_num,
    values_from = value
  )

# Clean up indicator names
violence_table_wide <- violence_table_wide %>%
  mutate(
    indicator = case_when(
      indicator == "total_respondents" ~ "Number of households",
      indicator == "witnessed_percent" ~ "Witnessed violent events (%)",
      indicator == "experienced_percent" ~ "Experienced violent events (%)",
      indicator == "witnessed_or_experienced_percent" ~ "Witnessed or experienced violent events (%)",
      TRUE ~ indicator  # Keep original name if no match
    )
  )

# Create the flextable
violence_flextable <- flextable(violence_table_wide) %>%
  set_header_labels(indicator = "") %>%  # Remove the first column title
  width(j = 1, width = 2) %>%  # Make the first column wider
  width(j = 2:ncol(violence_table_wide), width = 1.5) %>%  # Set width for other columns
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header") %>%  # Change font size for the header row
  set_table_properties(layout = "autofit")  # Ensure the table adapts to content

# Print the table
violence_flextable



#==============================================================================
# End of code
#==============================================================================