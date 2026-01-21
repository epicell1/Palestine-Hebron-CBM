#==============================================================================
# 
# Purpose: Analyze food security and nutrition data
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Cleaning data frame
#-------------------------------

# Remove variables starting with the specified prefix
data_edit_muac <- data_muac %>%
  rename_with(~ gsub("^(interview_info/full_survey/children_nutrition/)", "", .x), everything())

#-------------------------------
# Creating subset with current round only
#-------------------------------

# Convert date to proper date-time variables
data_edit_muac$doi <- ymd(data_edit_muac$doi)

# Assign round number based on parameters in input file
data_edit_muac <- data_edit_muac %>%
  rowwise() %>%  # Ensures operation is row-wise
  mutate(round_num = round_dates$round_num[which(doi >= round_dates$start_date & doi <= round_dates$end_date)[1]]) %>%
  ungroup()

# Create a subset of the dataframe with the current round only
data_edit_muac_current <- data_edit_muac %>%
  filter(round_num == most_recent_round)

#-------------------------------
# Creating variables - Food security
#-------------------------------

# Percentage of households reporting food insecurity (food_security_food_enough == "n")
households_insecure <- data_edit_current %>%
  filter(food_security_food_enough == "n") %>%
  nrow()

total_households <- nrow(data_edit_current)
percent_insecure <- round((households_insecure / total_households) * 100, 1)

# Households that reported food insecurity and had to limit portion sizes
households_limit <- data_edit_current %>%
  filter(food_security_food_enough == "n", !is.na(food_security_food_limit), food_security_food_limit > 0) %>%
  nrow()

percent_limit <- round((households_limit / households_insecure) * 100, 1)

# Households that reported food insecurity and had to restrict food consumption by adults
households_restrict <- data_edit_current %>%
  filter(food_security_food_enough == "n", !is.na(food_security_food_restrict), food_security_food_restrict > 0) %>%
  nrow()

percent_restrict <- round((households_restrict / households_insecure) * 100, 1)

# Households that reported food insecurity and had to reduce the number of meals
households_reduce <- data_edit_current %>%
  filter(food_security_food_enough == "n", !is.na(food_security_food_reduce), food_security_food_reduce > 0) %>%
  nrow()

percent_reduce <- round((households_reduce / households_insecure) * 100, 1)

### Build table per community ##

# Step 1: Calculate number of food insecure households by community
food_insecure_by_community <- data_edit_current %>%
  filter(food_security_food_enough == "n") %>%
  group_by(community) %>%
  summarise(
    num_insecure_households = n(),
    .groups = "drop"
  )

# Step 2: Calculate the total number of households by community
total_households_by_community <- data_edit_current %>%
  group_by(community) %>%
  summarise(
    total_households = n(),
    .groups = "drop"
  )

# Step 3: Merge the total households with food insecure data (using full join to keep all communities)
food_insecure_by_community <- total_households_by_community %>%
  left_join(food_insecure_by_community, by = "community") %>%
  mutate(
    num_insecure_households = ifelse(is.na(num_insecure_households), 0, num_insecure_households),
    proportion_insecure = round((num_insecure_households / total_households) * 100, 1)
  )

# Step 4: Create the flextable
food_insecure_flextable <- food_insecure_by_community %>%
  flextable() %>%
  set_header_labels(
    community = "Community",
    total_households = "Total number of households",
    num_insecure_households = "Number of food insecure households",
    proportion_insecure = "Proportion of food insecure households (%)"
  ) %>%
  # Apply text wrapping to the header
  compose(j = "community", value = as_paragraph(as_chunk(community, width = 20))) %>%
  compose(j = "total_households", value = as_paragraph(as_chunk(num_insecure_households, width = 20))) %>%
  compose(j = "num_insecure_households", value = as_paragraph(as_chunk(num_insecure_households, width = 20))) %>%
  compose(j = "proportion_insecure", value = as_paragraph(as_chunk(proportion_insecure, width = 20))) %>%
  # Format number columns to remove decimals
  colformat_num(j = "num_insecure_households", digits = 0) %>%
  colformat_num(j = "total_households", digits = 0) %>%
  # Set table properties for fitting in Word
  set_table_properties(layout = "autofit") %>%
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header")  %>% # Change font size for the header row
  autofit()

# View the flextable
food_insecure_flextable

### Build table per round ##

# Summarizing the data by round_num
food_insecurity_summary <- data_edit %>%
  filter(!is.na(food_security_food_enough)) %>%  # Filter out NAs
  group_by(round_num) %>%
  summarise(
    num_food_insecure_households = sum(food_security_food_enough == "n"),  # Total food insecure households
    num_households = n(),  # Total number of households
    .groups = "drop"
  ) %>%
  mutate(
    # Calculate proportions for food insecure households
    proportion_food_insecure = round((num_food_insecure_households / num_households) * 100, 1)
  )

# Rename the columns to make them more readable
food_insecurity_summary <- food_insecurity_summary %>%
  rename(
    "Round" = round_num,
    "Number of food insecure households" = num_food_insecure_households,
    "Total number of households" = num_households,
    "Proportion of food insecure households" = proportion_food_insecure
  )

# Reshape the data so that rounds become columns
food_insecurity_wide <- food_insecurity_summary %>%
  pivot_longer(cols = c("Number of food insecure households", 
                        "Total number of households", 
                        "Proportion of food insecure households"), 
               names_to = "Indicator", 
               values_to = "Count") %>%
  pivot_wider(names_from = Round, values_from = Count)

# Enforce correct row order
indicator_order <- c(
  "Number of food insecure households",
  "Total number of households",
  "Proportion of food insecure households"
)

food_insecurity_wide <- food_insecurity_wide %>%
  mutate(Indicator = factor(Indicator, levels = indicator_order)) %>%
  arrange(Indicator)

# Create the flextable
ft_food_insecurity <- flextable(food_insecurity_wide)

# Apply autofit to automatically adjust column widths and left-align text
ft_food_insecurity <- ft_food_insecurity %>%
  set_header_labels(Indicator = "") %>%  # Remove the header for the first column
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header")  %>% # Change font size for the header row
  autofit() %>%  # Automatically adjust column widths
  align(align = "left", part = "all")  # Left-align text in the table

# Print the table
ft_food_insecurity

#-------------------------------
# Creating variables - MUAC
#-------------------------------

# Total number of children aged 6-59 months
total_children <- nrow(data_edit_muac_current)

# Number of children present at home and available during the interview (child_present == "y")
children_present <- data_edit_muac_current %>%
  filter(child_present == "y") %>%
  nrow()

# Percentage of children present at home and available during the interview (rounded to 1 decimal)
percent_present <- round((children_present / total_children) * 100, 1)

# Number of children with MUAC measurements (MUAC_score not "unable")
children_muac_measured <- data_edit_muac_current %>%
  filter(MUAC_score != "unable") %>%
  nrow()

# Percentage of children with MUAC measurements (rounded to 1 decimal)
percent_muac_measured <- round((children_muac_measured / children_present) * 100, 1)

# Number of children with normal nutritional status (MUAC_score == "green")
children_normal <- data_edit_muac_current %>%
  filter(MUAC_score == "green") %>%
  nrow()

# Number of children with moderate acute malnutrition (MUAC_score == "yellow")
children_moderate_malnutrition <- data_edit_muac_current %>%
  filter(MUAC_score == "yellow") %>%
  nrow()

# Number of children with severe acute malnutrition (MUAC_score == "red")
children_severe_malnutrition <- data_edit_muac_current %>%
  filter(MUAC_score == "red") %>%
  nrow()

# Calculate percentages for each category (rounded to 1 decimal)
percent_normal <- round((children_normal / children_muac_measured) * 100, 1)
percent_moderate_malnutrition <- round((children_moderate_malnutrition / children_muac_measured) * 100, 1)
percent_severe_malnutrition <- round((children_severe_malnutrition / children_muac_measured) * 100, 1)


#-------------------------------
# Building Table: MUAC Proportions per Round
#-------------------------------

# Summarize the data by round_num
muac_table <- data_edit_muac %>%
  group_by(round_num) %>%
  summarise(
    children_muac_measured = sum(MUAC_score != "unable", na.rm = TRUE),  # Number of children with MUAC measured
    percent_green = round(
      sum(MUAC_score == "green", na.rm = TRUE) / children_muac_measured * 100, 1
    ),  # Proportion of children with green results
    percent_yellow = round(
      sum(MUAC_score == "yellow", na.rm = TRUE) / children_muac_measured * 100, 1
    ),  # Proportion of children with yellow results
    percent_red = round(
      sum(MUAC_score == "red", na.rm = TRUE) / children_muac_measured * 100, 1
    ),  # Proportion of children with red results
    .groups = "drop"
  )

# Reshape the data for better table structure
muac_table_long <- muac_table %>%
  pivot_longer(
    cols = -round_num,  # All columns except "round_num"
    names_to = "indicator",  # Column names become indicators
    values_to = "value"  # Values populate the table
  )

# Reshape further to have rounds as columns
muac_table_wide <- muac_table_long %>%
  pivot_wider(
    names_from = round_num,
    values_from = value
  )

# Clean up indicator names
muac_table_wide <- muac_table_wide %>%
  mutate(
    indicator = case_when(
      indicator == "children_muac_measured" ~ "Number of children with MUAC measured",
      indicator == "percent_green" ~ "Green MUAC (Normal) (%)",
      indicator == "percent_yellow" ~ "Yellow MUAC (Moderate malnutrition) (%)",
      indicator == "percent_red" ~ "Red MUAC (Severe malnutrition) (%)",
      TRUE ~ indicator  # Keep original name if no match
    )
  )

# Create the flextable
muac_flextable <- flextable(muac_table_wide) %>%
  set_header_labels(indicator = "") %>%  # Remove the first column title
  width(j = 1, width = 2) %>%  # Make the first column wider
  width(j = 2:ncol(muac_table_wide), width = 1.5) %>%  # Set width for other columns
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header") %>%  # Change font size for the header row
  set_table_properties(layout = "autofit")  # Ensure the table adapts to content

# Print the table
muac_flextable





#==============================================================================
# End of code
#==============================================================================

