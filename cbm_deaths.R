#==============================================================================
# 
# Purpose: Analyzing death data
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Cleaning data frame
#-------------------------------

# Remove variables starting with the specified prefix
data_edit_deaths <- data_deaths %>%
  rename_with(~ gsub("^(interview_info/full_survey/death_info/)", "", .x), everything())

#-------------------------------
# Creating subset with current round only
#-------------------------------

# Convert date to proper date-time variables
data_edit_deaths$doi <- ymd(data_edit_deaths$doi)

# Assign round number based on parameters in input file
data_edit_deaths <- data_edit_deaths %>%
  rowwise() %>%  # Ensures operation is row-wise
  mutate(round = round_dates$round_num[which(doi >= round_dates$start_date & doi <= round_dates$end_date)[1]]) %>%
  ungroup()

# Create a subset of the dataframe with the current round only
data_edit_deaths_current <- data_edit_deaths %>%
  filter(round == most_recent_round)

#-------------------------------
# Creating variables
#-------------------------------

# Convert the mortality_death_num variable to numeric
data_edit_deaths_current <- data_edit_deaths_current %>%
  mutate(mortality_death_num = as.numeric(mortality_death_num))
                                          
# Calculate the number of deaths
num_deaths <- sum(data_edit_deaths_current$mortality_death_num, na.rm = TRUE)

# Calculate the crude mortality rate (CMR)
crude_mortality_rate <- round((num_deaths / total_individuals) * 1000, 1)

#-------------------------------
# Building table
#-------------------------------

# Clean the values for sex_dead and mortality_under5
data_edit_deaths_current <- data_edit_deaths_current %>%
  mutate(
    sex_dead = case_when(
      sex_dead == "male" ~ "Male",
      sex_dead == "female" ~ "Female",
      TRUE ~ sex_dead
    ),
    mortality_under5 = case_when(
      mortality_under5 == "infection" ~ "Infection",
      mortality_under5 == "chronic_disease" ~ "Exacerbation of chronic disease (e.g. diabetes, hypertension, etc.)",
      mortality_under5 == "surgery" ~ "Surgical outcome or medical intervention",
      mortality_under5 == "trauma" ~ "Trauma (non-violence related, e.g. motor-vehicle accident)",
      mortality_under5 == "violence" ~ "Violence-related (e.g. gunshot, explosives, homicide, etc.)",
      mortality_under5 == "natural" ~ "Natural causes (e.g. old age)",
      mortality_under5 == "environment" ~ "Environmental (e.g. floods, earthquakes)",
      mortality_under5 == "pregnancy" ~ "Pregnancy-related (e.g. maternal mortality during childbirth)",
      mortality_under5 == "other" ~ "Other",
      mortality_under5 == "unknown" ~ "Unknown",
      TRUE ~ mortality_under5
    ),
    age_category = case_when(
      death_age_years < 5 ~ "<5 years old",
      death_age_years >= 5 & death_age_years <= 17 ~ "5-17 years old",
      death_age_years >= 18 & death_age_years <= 29 ~ "18-29 years old",
      death_age_years >= 30 & death_age_years <= 39 ~ "30-39 years old",
      death_age_years >= 40 & death_age_years <= 49 ~ "40-49 years old",
      death_age_years >= 50 & death_age_years <= 59 ~ "50-59 years old",
      death_age_years >= 60 & death_age_years <= 69 ~ "60-69 years old",
      death_age_years >= 70 ~ "70+ years old",
      TRUE ~ "Unknown"
    )
  )

# Select the specified columns with age categories and rename them
selected_data <- data_edit_deaths_current %>%
  select(Sex = sex_dead, `Age category` = age_category, `Cause of death` = mortality_under5)

# Create a flextable
deaths_flextable <- flextable(selected_data) %>%
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header")  %>% # Change font size for the header row 
  autofit()


#-------------------------------
# Data Preparation
#-------------------------------

# Ensure mortality_death_num is numeric
data_edit_deaths <- data_edit_deaths %>%
  mutate(mortality_death_num = as.numeric(mortality_death_num))  # Convert to numeric

# Handle warnings caused by non-numeric entries
data_edit_deaths <- data_edit_deaths %>%
  mutate(mortality_death_num = ifelse(is.na(mortality_death_num), 0, mortality_death_num))  # Replace NA with 0

#-------------------------------
# Building Table: Mortality Statistics per Time Period
#-------------------------------

# Summarize the data by round_num
mortality_table <- data_edit_deaths %>%
  group_by(round) %>%
  summarise(
    num_deaths = sum(as.numeric(mortality_death_num), na.rm = TRUE),  # Total number of deaths
    crude_mortality_rate = round((num_deaths / total_individuals) * 1000, 1),  # CMR per 1000 individuals
    .groups = "drop"
  )

# Convert the number of deaths to integer and remove decimals
mortality_table <- mortality_table %>%
  mutate(num_deaths = as.integer(num_deaths))  # Ensure no decimals for "Number of deaths"

# Reshape the data for better table structure
mortality_table_long <- mortality_table %>%
  pivot_longer(
    cols = -round,  # All columns except "round_num"
    names_to = "indicator",  # Column names become indicators
    values_to = "value"  # Values populate the table
  )

# Reshape further to have months as columns
mortality_table_wide <- mortality_table_long %>%
  pivot_wider(
    names_from = round,
    values_from = value
  )

# Clean up indicator names
mortality_table_wide <- mortality_table_wide %>%
  mutate(
    indicator = case_when(
      indicator == "num_deaths" ~ "Number of deaths",
      indicator == "crude_mortality_rate" ~ "Crude Mortality Rate (CMR)",
      TRUE ~ indicator  # Keep original name if no match
    )
  )

# Create the flextable
mortality_flextable <- flextable(mortality_table_wide) %>%
  set_header_labels(indicator = "") %>%  # Remove the first column title
  width(j = 1, width = 2) %>%  # Make the first column wider
  width(j = 2:ncol(mortality_table_wide), width = 1.5) %>%  # Set width for other columns
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header") %>%  # Change font size for the header row
  set_table_properties(layout = "autofit")  # Ensure the table adapts to content

# Print the table
mortality_flextable



#==============================================================================
# End of code
#==============================================================================


