#==============================================================================
# 
# Purpose: Analyzing health seeking behaviours data
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Cleaning data frame
#-------------------------------

# Remove variables starting with the specified prefix
data_edit_health <- data_health %>%
  rename_with(~ gsub("^(interview_info/full_survey/health_info/)", "", .x), everything())

#-------------------------------
# Creating subset with current round only
#-------------------------------

# Convert date to proper date-time variables
data_edit_health$doi <- ymd(data_edit_health$doi)

# Assign round number based on parameters in input file
data_edit_health <- data_edit_health %>%
  rowwise() %>%  # Ensures operation is row-wise
  mutate(round_num = round_dates$round_num[which(doi >= round_dates$start_date & doi <= round_dates$end_date)[1]]) %>%
  ungroup()

# Adding month-year to data frame
# data_edit_health <- data_edit_health %>%
#   left_join(round_dates %>% dplyr::select(round_num, month_year), by = "round_num")

# Create a subset of the dataframe with the current round only
data_edit_health_current <- data_edit_health %>%
  filter(round_num == most_recent_round)

#-------------------------------
# Creating variables
#-------------------------------

# Generating the number of ill individuals
num_ill_or_both <- data_edit_health_current %>%
  filter(ill_or_injured %in% c("ill", "both_ill_and_injured")) %>%
  nrow()

# Generating the number of injured individuals
num_injured_or_both <- data_edit_health_current %>%
  filter(ill_or_injured %in% c("injured", "both_ill_and_injured")) %>%
  nrow()

# Counting how many needed care 
num_care_required <- data_edit_health_current %>%
  filter(care_required == "yes__medical_care_was_needed") %>%
  nrow()

# Count the total number of non-missing responses for care_required
total_responses <- data_edit_health_current %>%
  filter(!is.na(care_required)) %>%
  nrow()

# Calculate the proportion and round it to one decimal place
proportion_care_required <- round((num_care_required / total_responses) * 100, 1)

# Print the result
proportion_care_required

# Filter the data where care_required equals "yes__medical_care_was_needed"
care_needed_data <- data_edit_health_current %>%
  filter(care_required == "yes__medical_care_was_needed")

# Count the number of rows where medical_care equals "y"
num_medical_care_y <- care_needed_data %>%
  filter(medical_care == "y") %>%
  nrow()

# Calculate the total number of rows where care_required equals "yes__medical_care_was_needed"
total_care_needed <- nrow(care_needed_data)

# Calculate the proportion and round it to one decimal place
proportion_medical_care_y <- round((num_medical_care_y / total_care_needed) * 100, 1)

# Print the result
proportion_medical_care_y
num_medical_care_y

# Count the number of rows where medical_care equals "n"
num_medical_care_n <- care_needed_data %>%
  filter(medical_care == "n") %>%
  nrow()

# Calculate the proportion and round it to one decimal place
proportion_medical_care_n <- round((num_medical_care_n / total_care_needed) * 100, 1)

# Print the result
proportion_medical_care_n
num_medical_care_n

# Convert age_ill_years to numeric 
data_edit_health_current$age_ill_years <- as.numeric(data_edit_health_current$age_ill_years)

# Filter the data for children under 5 and where care_required equals "yes__medical_care_was_needed"
care_needed_under_5 <- data_edit_health_current %>%
  filter(age_ill_years < 5, care_required == "yes__medical_care_was_needed")

# Count the number of rows where medical_care equals "y" for children under 5
num_medical_care_y_under_5 <- care_needed_under_5 %>%
  filter(medical_care == "y") %>%
  nrow()

# Calculate the total number of rows where care_required equals "yes__medical_care_was_needed" for children under 5
total_care_needed_under_5 <- nrow(care_needed_under_5)

# Calculate the proportion and round it to one decimal place
proportion_medical_care_y_under_5 <- round((num_medical_care_y_under_5 / total_care_needed_under_5) * 100, 1)

# Print the result
proportion_medical_care_y_under_5
num_medical_care_y_under_5

# Filter the data for those who couldn't access care (medical_care == "n") but needed it 
care_not_accessed <- data_edit_health_current %>%
  filter(medical_care == "n" & care_required=="yes__medical_care_was_needed")

# Calculate the total population needing care per community
total_population_per_community <- data_edit_health_current %>%
  filter(care_required=="yes__medical_care_was_needed") %>%
  group_by(community) %>%
  summarise(total_population = n(), .groups = "drop")

# Summarize the number and proportion of individuals who couldn't access care per community
proportion_care_not_accessed <- care_not_accessed %>%
  group_by(community) %>%
  summarise(
    num_care_not_accessed = n(),  # Count of individuals who couldn't access care
    .groups = "drop"
  ) %>%
  left_join(total_population_per_community, by = "community") %>%  # Merge with total population
  mutate(proportion_care_not_accessed = round((num_care_not_accessed / total_population) * 100, 1))  # Compute proportion

# Find the community with the highest proportion of care not accessed
community_highest_proportion <- proportion_care_not_accessed %>%
  arrange(desc(proportion_care_not_accessed)) %>%  # Sort by highest proportion
  slice(1)  # Get the first row (the community with the highest proportion)

# Extract values for number and proportion of people unable to access care
num_people_unable_to_access_care <- community_highest_proportion$num_care_not_accessed
proportion_unable_to_access_care <- community_highest_proportion$proportion_care_not_accessed
highest_community <- community_highest_proportion$community

# Filter the data for those who couldn't access care (medical_care == "n")
care_not_accessed <- data_edit_health_current %>%
  filter(medical_care == "n")

# Compute total population by community first
total_population_by_community <- data_edit_health_current %>%
  group_by(community) %>%
  summarise(total_population = n(), .groups = "drop")

# Summarize the data by community
proportion_care_not_accessed <- care_not_accessed %>%
  group_by(community) %>%
  summarise(
    num_care_not_accessed = n(),  # Count of individuals who couldn't access care
    .groups = "drop"
  ) %>%
  left_join(total_population_by_community, by = "community") %>%  # Merge total population data
  mutate(
    proportion_care_not_accessed = round((num_care_not_accessed / total_population) * 100, 1)  # Calculate proportion
  )

# Find the community with the highest proportion of care not accessed
community_highest_proportion <- proportion_care_not_accessed %>%
  arrange(desc(proportion_care_not_accessed)) %>%
  slice(1)  # Get the first row (the community with the highest proportion)

# Extract values
community_lower_care_access <- community_highest_proportion$community
num_people_unable_to_access_care <- community_highest_proportion$num_care_not_accessed
proportion_unable_to_access_care <- community_highest_proportion$proportion_care_not_accessed

community_lower_care_access
num_people_unable_to_access_care
proportion_unable_to_access_care

### Car confiscations ###

# Calculate the count and proportion of individuals who answered "yes" to car_confiscation
confiscation_count <- sum(data_edit_health_current$car_confiscation == "yes", na.rm = TRUE)
confiscation_prop <- round((confiscation_count / sum(!is.na(data_edit_health_current$car_confiscation))) * 100, 1)

# Display results
confiscation_count
confiscation_prop

# Calculate the count and proportion of individuals who answered "no" to car_confiscation
fear_confiscation_count <- sum(data_edit_health_current$car_confiscation == "no", na.rm = TRUE)
fear_confiscation_prop <- round((fear_confiscation_count / sum(!is.na(data_edit_health_current$car_confiscation))) * 100, 1)

# Display results
fear_confiscation_count
fear_confiscation_prop

### Coping mecanisms ###

# Separate multiple coping mechanisms into individual rows
coping_mechanisms_long <- care_not_accessed %>%
  separate_rows(med_no_measures, sep = " ") %>%  # Adjust separator if needed (e.g., comma or semicolon)
  count(med_no_measures) %>%  # Count occurrences of each coping mechanism
  mutate(proportion = round(n / nrow(care_not_accessed) * 100, 1))  # Calculate percentage

# Create a mapping of short codes to full descriptions for coping mechanisms
coping_mechanisms_long <- coping_mechanisms_long %>%
  mutate(med_no_measures = case_when(
    med_no_measures == "home_remedies" ~ "Used home remedies",
    med_no_measures == "meds_other" ~ "Used medication meant for another purpose or another person",
    med_no_measures == "meds_spaced_out" ~ "Spaced out medication dosages",
    med_no_measures == "trad_healer" ~ "Consulted a traditional healer",
    med_no_measures == "comm_support" ~ "Sought support from community health workers or community organizations",
    med_no_measures == "friends_fam_support" ~ "Asked friends or family for assistance",
    med_no_measures == "online_info" ~ "Accessed online health information or resources",
    med_no_measures == "no_measures" ~ "I did not take any special measure",
    med_no_measures == "other" ~ "Other",
    TRUE ~ med_no_measures  # Keep original value if not in the list
  ))

# View the resulting table
coping_mechanisms_long

# Sort the mechanisms by frequency and select the top three
top_coping_mechanisms <- coping_mechanisms_long %>%
  arrange(desc(n)) %>%  # Sort by count in descending order
  slice_head(n = 3)  # Select the top 3

# Extract the top three coping mechanisms, their counts, and proportions
most_common_coping <- top_coping_mechanisms[1, ]
second_most_common_coping <- top_coping_mechanisms[2, ]
third_most_common_coping <- top_coping_mechanisms[3, ]

# Store the count and proportion for each of the top three coping mechanisms
most_common_coping_count <- most_common_coping$n
most_common_coping_proportion <- most_common_coping$proportion

second_most_common_coping_count <- second_most_common_coping$n
second_most_common_coping_proportion <- second_most_common_coping$proportion

third_most_common_coping_count <- third_most_common_coping$n
third_most_common_coping_proportion <- third_most_common_coping$proportion

# Print the results
most_common_coping_count
most_common_coping_proportion

second_most_common_coping_count
second_most_common_coping_proportion

third_most_common_coping_count
third_most_common_coping_proportion

# Extract the names of the coping mechanisms
most_common_coping_name <- most_common_coping$med_no_measures
second_most_common_coping_name <- second_most_common_coping$med_no_measures
third_most_common_coping_name <- third_most_common_coping$med_no_measures


#-------------------------------
# Figure 1. Proportion unable to access care when needed, per community 
#-------------------------------

care_access_graph <- ggplot(proportion_care_not_accessed, aes(x = reorder(community, -proportion_care_not_accessed), 
                                                              y = proportion_care_not_accessed, fill = community)) +
  geom_bar(stat = "identity") +  # Use identity to plot actual proportions
  labs(x = "Community", y = "Proportion (%)") +  # No title
  scale_fill_brewer(palette = "Blues") +  # Muted blue color palette
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove legend


#-------------------------------
# Figure 2. Reasons for being unable to access care when needed 
#-------------------------------

# Filter only individuals who didn't access care
care_not_accessed <- data_edit_health_current %>%
  filter(medical_care == "n")

# Separate multiple reasons into individual rows
reasons_long <- care_not_accessed %>%
  separate_rows(med_care_no, sep = " ") %>%  # Adjust separator if needed (e.g., comma or semicolon)
  count(med_care_no) %>%  # Count occurrences of each reason
  mutate(proportion = round(n / nrow(care_not_accessed) * 100, 1))  # Calculate percentage

# Create a mapping of short codes to full descriptions
reasons_long <- reasons_long %>%
  mutate(med_care_no = case_when(
    med_care_no == "time" ~ "Long wait times at the healthcare facility",
    med_care_no == "distrust" ~ "Distrust in healthcare facility or staff",
    med_care_no == "closures" ~ "Closures or reduction in medical services",
    med_care_no == "financial" ~ "Financial constraints (e.g. cost of healthcare, transportation, no medical insurance, etc.)",
    med_care_no == "distance" ~ "Long distance to healthcare facility",
    med_care_no == "access" ~ "No access to transportation",
    med_care_no == "checkpoints" ~ "Checkpoints, blockades or physical obstacles preventing access",
    med_care_no == "curfew" ~ "Curfew",
    med_care_no == "violence_fear" ~ "Fear of violence (e.g. by settlers or military) en route to healthcare facility",
    med_care_no == "other_obligations" ~ "Required to prioritize other activities (e.g. work obligations, no childcare available, etc.)",
    med_care_no == "disability" ~ "Barrier due to disability (e.g. mobility, vision, hearing, or cognitive challenges)",
    med_care_no == "pharmacy" ~ "Preference for going to a pharmacy instead",
    med_care_no == "other" ~ "Other",
    TRUE ~ med_care_no  # Keep original value if not in the list
  ))

# Create compact horizontal bar plot with reduced spacing
reasons_no_care <- ggplot(reasons_long, aes(x = reorder(med_care_no, proportion), y = proportion, fill = med_care_no)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge(width = 0)) +  # No spacing between bars
  labs(y = "Proportion (%)", x = "Reason") +  
  scale_fill_brewer(palette = "Blues") +  
  scale_y_continuous(expand = c(0, 0)) +  # Remove extra space around bars
  theme_minimal() +  
  theme(axis.text.y = element_text(size = 12),  # Reduce y-axis text size
        axis.text.x = element_text(size =12),  # Reduce x-axis text size
        legend.position = "none",
        plot.margin = margin(5, 5, 5, 5)) +  # Reduce margins
  coord_flip()  # Flip for horizontal bars

# Display the plot
reasons_no_care <- reasons_no_care + geom_col(width = 0.95)
reasons_no_care

# Create a table with the number and proportion for each reason
reasons_table <- reasons_long %>%
  dplyr::select(Reason = med_care_no, Count = n, Proportion = proportion)

# Print the table using kable
#kable(reasons_table, format = "markdown", caption = "Number and Proportion of Reasons for Being Unable to Access Care")

#-------------------------------
# Table 3. Evolution of health seeking behavior, per round 
#-------------------------------

# Summarizing the data by month_year
summary_by_round_num <- data_edit_health %>%
  filter(ill_or_injured %in% c("ill", "injured", "both_ill_and_injured")) %>%
  group_by(round_num) %>%
  summarise(
    num_ill_or_injured = n(),  # Total ill or injured individuals
    num_ill_or_injured_needed_care = sum(care_required == "yes__medical_care_was_needed"),  # Ill/injured who needed care
    num_ill_or_injured_got_care = sum(medical_care == "y" & care_required == "yes__medical_care_was_needed"),  # Ill/injured who got care
    .groups = "drop"
  )

# Rename the columns to make them more readable
summary_by_round_num <- summary_by_round_num %>%
  rename(
    "Round" = round_num,
    "Number of ill/injured individuals" = num_ill_or_injured,
    "Number of ill/injured who needed care" = num_ill_or_injured_needed_care,
    "Number of ill/injured who got care when needed" = num_ill_or_injured_got_care
  ) %>%
  mutate(
    # Convert numbers to characters (to prevent type mismatch in pivoting)
    `Number of ill/injured individuals` = as.character(`Number of ill/injured individuals`),
    `Number of ill/injured who needed care` = as.character(`Number of ill/injured who needed care`),
    `Number of ill/injured who got care when needed` = as.character(`Number of ill/injured who got care when needed`),
    # Format proportions with percentage sign
    `Proportion of ill/injured who needed care` = paste0(round((as.numeric(`Number of ill/injured who needed care`) / as.numeric(`Number of ill/injured individuals`)) * 100, 1), "%"),
    `Proportion of ill/injured who got care when needed` = paste0(round((as.numeric(`Number of ill/injured who got care when needed`) / as.numeric(`Number of ill/injured who needed care`)) * 100, 1), "%")
  )

# Reshape the data so that rounds become columns
summary_wide <- summary_by_round_num %>%
  pivot_longer(cols = c("Number of ill/injured individuals", 
                        "Number of ill/injured who needed care", 
                        "Proportion of ill/injured who needed care", 
                        "Number of ill/injured who got care when needed", 
                        "Proportion of ill/injured who got care when needed"), 
               names_to = "Indicator", 
               values_to = "Count") %>%
  pivot_wider(names_from = Round, values_from = Count)

# Enforce correct row order
indicator_order <- c(
  "Number of ill/injured individuals",
  "Number of ill/injured who needed care",
  "Proportion of ill/injured who needed care", 
  "Number of ill/injured who got care when needed", 
  "Proportion of ill/injured who got care when needed"
)

summary_wide <- summary_wide %>%
  mutate(Indicator = factor(Indicator, levels = indicator_order)) %>%
  arrange(Indicator)

# Create the flextable
ft_summary <- flextable(summary_wide)

# Apply autofit to automatically adjust column widths and left-align text
ft_summary <- ft_summary %>%
  set_header_labels(Indicator = "") %>%  # Remove the header for the first column
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header")  %>% # Change font size for the header row
  autofit() %>%  # Automatically adjust column widths
  align(align = "left", part = "all")  # Left-align text in the table

# Print the table
ft_summary

#==============================================================================
# End of code
#==============================================================================