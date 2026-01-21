#==============================================================================
# 
# Purpose: Analyze data on access to water
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
# Main source of drinking water
#-------------------------------
# 
# # Create a named vector for clean names
# clean_names <- c(
#   piped_water_into_dwelling = "Piped water into dwelling",
#   piped_water_to_yard_plot = "Piped water to yard/plot",
#   spring = "Spring water",
#   well = "Well water",
#   rainwater_collection = "Rainwater collection",
#   water_trucking = "Water trucking (stored in water tanks or cisterns)",
#   bottled_water = "Bottled water",
#   other = "Other"
# )
# 
# # Calculate the proportion of households relying on each water source and clean the names
# water_source_summary <- surveyed_households %>%
#   group_by(water_source_drinking) %>%
#   summarise(count = n()) %>%
#   mutate(
#     percentage = round((count / sum(count)) * 100, 1),
#     clean_names = clean_names[water_source_drinking]
#   ) %>%
#   dplyr::select(clean_names, count, percentage)%>%
#   na.omit() %>%
#   arrange(desc(count))  # Order by count in descending order
# 
# # Create a flextable
# water_source_flextable <- flextable(water_source_summary) %>%
#   set_header_labels(
#     clean_names = "Main source of drinking water",
#     count = "Number of households",
#     percentage = "Proportion of households (%)"
#   )  %>%
#   fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
#   fontsize(size = 10, part = "header")  %>% # Change font size for the header row 
#   width(j = 1:3, width = 1.2) %>% # Adjust column width
#   height(height = 1)            # Adjust row height
# 
# # Print the flextable
# print(water_source_flextable)
# 
# # Extract the top three sources
# top_1_source <- water_source_summary[1, ]
# top_2_source <- water_source_summary[2, ]
# top_3_source <- water_source_summary[3, ]
# 
# # Assign variables for the R Markdown line
# majority_source <- top_1_source$clean_names
# majority_count <- top_1_source$count
# majority_percentage <- top_1_source$percentage
# 
# second_source <- top_2_source$clean_names
# second_count <- top_2_source$count
# second_percentage <- top_2_source$percentage
# 
# third_source <- top_3_source$clean_names
# third_count <- top_3_source$count
# third_percentage <- top_3_source$percentage

#-------------------------------
# Number of days without access to clean water 
#-------------------------------

# Map the days categories to readable labels
days_labels <- c(
  zero_days = "0 days",
  one_five_days = "1-5 days",
  six_ten_days = "6-10 days",
  eleven_fifteen_days = "11-15 days",
  more_than_fifteen = "15+ days"
)

# Calculate the total number of households surveyed
total_households <- nrow(surveyed_households)

# Calculate the proportion of households facing difficulties accessing clean water
difficulties_summary <- surveyed_households %>%
  filter(!is.na(water_no_water_days) & water_no_water_days != "pna") %>%
  summarise(
    total_households = n(),
    difficulties_count = sum(water_no_water_days != "zero_days"),
    difficulties_percentage = round((difficulties_count / total_households) * 100, 2)
  )

# Assign variables for the R Markdown line
total_households_surveyed <- total_households
difficulties_percentage <- difficulties_summary$difficulties_percentage
difficulties_count <- difficulties_summary$difficulties_count

# Calculate the most frequent category under water_no_water_days
most_frequent_category <- data_edit_current %>%
  filter(!is.na(water_no_water_days) & water_no_water_days != "pna" & water_no_water_days != "zero_days") %>%
  count(water_no_water_days) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(water_no_water_days)

# Get the clean label for the most frequent category
most_frequent_label <- days_labels[most_frequent_category]

### Create bar chart ###
# Summarize the number of households for each category of days without water
days_summary <- surveyed_households %>%
  group_by(water_no_water_days) %>%
  summarise(count = n())

# Update the water_no_water_days variable to use readable labels
days_summary$water_no_water_days <- factor(days_summary$water_no_water_days, 
                                           levels = names(days_labels), 
                                           labels = days_labels)

# Create a bar chart
water_days_graph <- ggplot(days_summary, aes(x = water_no_water_days, 
                                             y = count, fill = water_no_water_days)) +
  geom_bar(stat = "identity") +  # Use identity to plot actual counts
  labs(x = NULL, y = "Number of Households") +  # Axis labels
  scale_fill_brewer(palette = "Blues") +  # Muted blue color palette
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove legend

#-------------------------------
# Difficulties accessing clean water 
#-------------------------------

# Create a named vector for clean labels
difficulty_labels <- c(
  financial = "Financial barriers (e.g., high cost of water or transportation)",
  physical = "Physical barriers (e.g., blocked access points)",
  threats = "Intimidation or threats from settlers",
  restrictions = "Restrictions imposed by authorities",
  damage = "Damage to water infrastructure (e.g. broken pipes or wells)",
  supply = "Limited supply during peak demand periods",
  technical = "Technical issues in the water network (e.g. high areas with low water pressure in the network)",
  other = "Other"
)

# Count occurrences of each difficulty in accessing clean water
water_difficulties_summary <- data_edit_current %>%
  filter(!is.na(water_difficulties)) %>%
  separate_rows(water_difficulties, sep = " ") %>%
  count(water_difficulties) %>%
  arrange(desc(n)) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1),
clean_labels = difficulty_labels[water_difficulties]
)

# Get the top three difficulties
top_1_difficulty <- water_difficulties_summary[1, ]
top_2_difficulty <- water_difficulties_summary[2, ]
top_3_difficulty <- water_difficulties_summary[3, ]

# Assign variables for the R Markdown line
difficulty_1 <- top_1_difficulty$clean_labels
difficulty_1_count <- top_1_difficulty$n
difficulty_1_percentage <- top_1_difficulty$percentage

difficulty_2 <- top_2_difficulty$clean_labels
difficulty_2_count <- top_2_difficulty$n
difficulty_2_percentage <- top_2_difficulty$percentage

difficulty_3 <- top_3_difficulty$clean_labels
difficulty_3_count <- top_3_difficulty$n
difficulty_3_percentage <- top_3_difficulty$percentage

#-------------------------------
# Impact on daily life 
#-------------------------------

# Create a named vector for clean labels
impact_labels <- c(
  water_intake = "Reduced drinking water intake",
  hygiene = "Affected personal hygiene",
  chores = "Restricted daily chores and household tasks",
  food_prep = "Limited food preparation",
  stress = "Increased stress or anxiety related to water access",
  livestock = "Reduced drinking water for livestock",
  children_act = "Limited children's participation in schooling and/or recreational activities",
  adult_act = "Limited adults' participation in community activities",
  displacement = "Forced displacement",
  no_impact = "No significant impact",
  other = "Other"
)

# Count occurrences of each impact on limited water access
water_impact_summary <- data_edit_current %>%
  filter(!is.na(water_water_impact)) %>%
  separate_rows(water_water_impact, sep = " ") %>%
  count(water_water_impact) %>%
  arrange(desc(n)) %>%
  mutate(
    percentage = round((n / sum(n)) * 100, 1),
    clean_labels = impact_labels[water_water_impact]
  )

# Get the top three impacts
top_1_impact <- water_impact_summary[1, ]
top_2_impact <- water_impact_summary[2, ]
top_3_impact <- water_impact_summary[3, ]

# Assign variables for the R Markdown line
impact_1 <- top_1_impact$clean_labels
impact_1_count <- top_1_impact$n
impact_1_percentage <- top_1_impact$percentage

impact_2 <- top_2_impact$clean_labels
impact_2_count <- top_2_impact$n
impact_2_percentage <- top_2_impact$percentage

impact_3 <- top_3_impact$clean_labels
impact_3_count <- top_3_impact$n
impact_3_percentage <- top_3_impact$percentage

#-------------------------------
# Proportion of households lacking water per community
#-------------------------------
# 
# # Create a categorical variable for water_no_water_days
# surveyed_households <- surveyed_households %>%
#   mutate(water_no_water_days = factor(water_no_water_days, levels = names(days_labels), labels = days_labels))
# 
# # Calculate the number and proportion of households per community
# water_access_summary <- surveyed_households %>%
#   group_by(community) %>%
#   summarise(
#     total_households = n(),
#     number_lacking_access = sum(water_no_water_days != "0 days"),
#     proportion_lacking_access = round((number_lacking_access / total_households) * 100, 2)
#   )
# 
# # Create a flextable
# water_access_flextable <- flextable(water_access_summary) %>%
#   set_header_labels(
#     community = "Community",
#     total_households = "Total Households",
#     number_lacking_access = "Households Lacking Access",
#     proportion_lacking_access = "Proportion of Households (%)"
#   ) %>%
#   autofit()


### 15 days or more only ###
# Create a categorical variable for water_no_water_days
surveyed_households <- surveyed_households %>%
  mutate(water_no_water_days = factor(water_no_water_days, levels = names(days_labels), labels = days_labels))

# Calculate the number and proportion of households per community
water_access_summary <- surveyed_households %>%
  group_by(community) %>%
  summarise(
    total_households = n(),
    number_lacking_access = sum(water_no_water_days == "15+ days"),
    proportion_lacking_access = round((number_lacking_access / total_households) * 100, 2)
  )

# Create a flextable
water_access_flextable <- flextable(water_access_summary) %>%
  set_header_labels(
    community = "Community",
    total_households = "Total Households",
    number_lacking_access = "Households Lacking Access",
    proportion_lacking_access = "Proportion of Households (%)"
  ) %>%
  autofit()

water_access_flextable

#-------------------------------
# Building Table: Clean Water Access per Round
#-------------------------------

# Filter for valid entries and summarize data by round_num
clean_water_table <- data_edit %>%
  filter(!is.na(water_no_water_days) & water_no_water_days != "pna") %>%
  group_by(round_num) %>%
  summarise(
    households_surveyed = n(),  # Total number of surveyed households
    no_clean_water_percent = round(
      sum(water_no_water_days != "zero_days", na.rm = TRUE) / households_surveyed * 100, 1
    ),  # Proportion of households with at least 1 day without clean water
    no_clean_water_15plus_percent = round(
      sum(water_no_water_days == "more_than_fifteen", na.rm = TRUE) / households_surveyed * 100, 1
    ),  # Proportion of households without clean water for 15+ days
    .groups = "drop"
  )

# Reshape the data for better table structure
clean_water_table_long <- clean_water_table %>%
  pivot_longer(
    cols = -round_num,  # All columns except "round_num"
    names_to = "indicator",  # Column names become indicators
    values_to = "value"  # Values populate the table
  )

# Reshape further to have round numbers as columns
clean_water_table_wide <- clean_water_table_long %>%
  pivot_wider(
    names_from = round_num,
    values_from = value
  )

# Clean up indicator names
clean_water_table_wide <- clean_water_table_wide %>%
  mutate(
    indicator = case_when(
      indicator == "households_surveyed" ~ "Households surveyed",
      indicator == "no_clean_water_percent" ~ "≥1 day without clean water (%)",
      indicator == "no_clean_water_15plus_percent" ~ "15+ days without clean water (%)",
      TRUE ~ indicator  # Keep original name if no match
    )
  )

# Create the flextable
clean_water_flextable <- flextable(clean_water_table_wide) %>%
  set_header_labels(indicator = "") %>%  # Remove the first column title
  width(j = 1, width = 2) %>%  # Make the first column wider
  width(j = 2:ncol(clean_water_table_wide), width = 1.5) %>%  # Set width for other columns
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header") %>%  # Change font size for the header row
  set_table_properties(layout = "autofit")  # Ensure the table adapts to content

# Print the table
clean_water_flextable




#==============================================================================
# End of code
#==============================================================================
