#==============================================================================
# 
# Purpose: Analyze financial hardship
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Creating values for report
#-------------------------------

# Filter out NA and "Prefer not to answer" responses
filtered_data <- data_edit_current %>%
  filter(!is.na(finances_finances_change) & finances_finances_change != "pnta")

# Define clean labels for the categories
clean_labels <- c(
  "improved_plus" = "Improved significantly",
  "improved" = "Improved somewhat",
  "same" = "Remained about the same",
  "worsened" = "Worsened somewhat",
  "worsened_plus" = "Worsened significantly"
)

# Apply clean labels to the data
filtered_data <- filtered_data %>%
  mutate(finances_finances_change = clean_labels[finances_finances_change])

# Calculate the count and proportion of each category
category_summary <- filtered_data %>%
  group_by(finances_finances_change) %>%
  summarise(
    count = n(),
    proportion = count / nrow(filtered_data) * 100
  ) %>%
  arrange(desc(count))

# Find the two most frequently cited categories
top_two_categories <- category_summary %>%
  slice_max(count, n = 2)

# Fill in the R Markdown code with the actual values
top_category_1 <- top_two_categories$finances_finances_change[1]
top_category_1_count <- top_two_categories$count[1]
top_category_1_proportion <- round(top_two_categories$proportion[1], 1)

top_category_2 <- top_two_categories$finances_finances_change[2]
top_category_2_count <- top_two_categories$count[2]
top_category_2_proportion <- round(top_two_categories$proportion[2], 1)

#-------------------------------
# Figure 6. Change in household's financial situation since October 2023
#-------------------------------

# Calculate the count and proportion of each category
category_summary <- filtered_data %>%
  group_by(finances_finances_change) %>%
  summarise(
    count = n(),
    proportion = count / nrow(filtered_data) * 100
  ) %>%
  arrange(desc(count))


# Wrap text labels to a specified width (adjust as needed)
category_summary$finances_finances_change <- str_wrap(category_summary$finances_finances_change, width = 10)

# Create a bar graph using ggplot2
bar_graph_finances <- ggplot(category_summary, aes(x = reorder(finances_finances_change, -proportion), y = proportion, fill = finances_finances_change)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 0.5, vjust = 0.5),  # Center-align labels
    legend.position = "none",
    axis.title.x = element_blank()
  ) +
  labs(y = "Proportion (%)")

# Display the bar graph
print(bar_graph_finances)

#-------------------------------
# Table 7. Contributing factors to worsening of financial situation 
#-------------------------------

# Define clean labels for the categories
clean_labels <- c(
  "job_less" = "Job loss",
  "work_permit" = "Cancellation of work permit",
  "reduced_work" = "Reduced work hours, reduction in pay, or delay in pay",
  "attack_livelihood" = "Attacks on or obstructions to livelihood",
  "mvt_restrictions" = "Movement restrictions",
  "destruction" = "Damage or destruction of homes or business due to violence",
  "displacement" = "Forced displacement",
  "med_expenses" = "Unexpected medical expenses",
  "cost_living" = "Increased cost of living",
  "expenses" = "Increase in family-related expenses",
  "circumstances" = "Changes in family circumstances",
  "other" = "Other"
)

# Filter out NAs
filtered_data_finances <- data_edit_current %>%
  filter(!is.na(finances_factors))

# Reshape the data to have one row per respondent per factor
long_data <- filtered_data_finances %>%
  separate_rows(finances_factors, sep = " ")

# Calculate the count and proportion of each factor
factor_summary <- long_data %>%
  group_by(finances_factors) %>%
  summarise(
    count = n(),
    proportion = round(count / nrow(filtered_data_finances) * 100, 1) 
  ) %>%
  filter(finances_factors != "pnta") %>%  # Remove "Prefer not to answer" if present
  arrange(desc(count))

filtered_data_finances1 <- nrow(filtered_data_finances)

# Apply clean labels
factor_summary$finances_factors <- clean_labels[factor_summary$finances_factors]

# Find the top three most common factors
top_three_factors <- factor_summary %>%
  slice_max(count, n = 3)

# Extract details for the top three factors
factor_1 <- top_three_factors$finances_factors[1]
count_1 <- top_three_factors$count[1]
proportion_1 <- round(top_three_factors$proportion[1], 1)

factor_2 <- top_three_factors$finances_factors[2]
count_2 <- top_three_factors$count[2]
proportion_2 <- round(top_three_factors$proportion[2], 1)

factor_3 <- top_three_factors$finances_factors[3]
count_3 <- top_three_factors$count[3]
proportion_3 <- round(top_three_factors$proportion[3], 1)

# Rename columns for the flextable
factor_summary <- factor_summary %>%
  rename(
    "Contributing factor" = finances_factors,
    "Number of households" = count,
    "Proportion of households (%)" = proportion
  )

# Create the flextable with adjusted column widths and set text wrapping
contributing_factors_table <- flextable(factor_summary) %>%
  set_header_labels(
    `Contributing factor` = "Factor contributing to economic worsening",
    `Number of households` = "Number of households",
    `Proportion of households (%)` = "Proportion of households (%)"
  ) %>%
  width(j = 1, width = 2) %>%
  width(j = 2, width = 1) %>%
  width(j = 3, width = 1) %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 10, part = "header") 

# Display the flextable
contributing_factors_table



#-------------------------------
# Proportion per community 
#-------------------------------

# Filter the data for households with 'worsened' or 'worsened_plus' financial situations
worsened_finances <- filtered_data %>%
  filter(finances_finances_change %in% c("Worsened somewhat", "Worsened significantly"))

# Calculate the total number of households by community
total_households_by_community <- filtered_data %>%
  group_by(community) %>%
  summarise(total_households = n())

# Calculate the number of households with 'worsened' or 'worsened_plus' financial situations by community
worsened_finances_by_community <- worsened_finances %>%
  group_by(community) %>%
  summarise(worsened_households = n())

# Merge the two dataframes to get the proportion
proportion_worsened_finances <- total_households_by_community %>%
  left_join(worsened_finances_by_community, by = "community") %>%
  mutate(
    worsened_households = ifelse(is.na(worsened_households), 0, worsened_households),
    proportion_worsened = round((worsened_households / total_households) * 100, 2)
  )

proportion_worsened_finances 

#==============================================================================
# End of code
#==============================================================================
