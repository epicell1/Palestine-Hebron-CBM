#==============================================================================
# 
# Purpose: Analyze mental health data
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Clean data frame
#-------------------------------

# Remove variables starting with the specified prefix
data_edit_mh <- data_mh %>%
  rename_with(~ gsub("^(interview_info/full_survey/module_mh/)", "", .x), everything())

#-------------------------------
# Creating subset with current round only
#-------------------------------

# Convert date to proper date-time variables
data_edit_mh$doi <- ymd(data_edit_mh$doi)

# Assign round number based on parameters in input file
data_edit_mh <- data_edit_mh %>%
  rowwise() %>%  # Ensures operation is row-wise
  mutate(round_num = round_dates$round_num[which(doi >= round_dates$start_date & doi <= round_dates$end_date)[1]]) %>%
  ungroup()

# Create a subset of the dataframe with the current round only
data_edit_mh_current <- data_edit_mh %>%
  filter(round_num == most_recent_round)

#-------------------------------
# Generating refusal rates
#-------------------------------

# Convert relevant columns to numeric
data_edit_current <- data_edit_current %>%
  mutate(
    hh_adult_male = as.numeric(hh_adult_male),
    hh_adult_female = as.numeric(hh_adult_female),
    men_at_home = as.numeric(men_at_home),
    women_at_home = as.numeric(women_at_home)
    )
    
# Calculate the total number of adults in the surveyed households
total_adults <- data_edit_current %>%
  summarise(total_adults = sum(hh_adult_male, hh_adult_female, na.rm = TRUE)) %>%
  pull(total_adults)

# Calculate the number of adults present and available to answer individual questions
num_present <- data_edit_current %>%
  summarise(num_present = sum(men_at_home, women_at_home, na.rm = TRUE)) %>%
  pull(num_present)

# Calculate the proportion of adults present and available
proportion_present <- round((num_present / total_adults) * 100, 1)

# Calculate the number and proportion of women present and available
num_women_present <- data_edit_current %>%
  summarise(num_women_present = sum(women_at_home, na.rm = TRUE)) %>%
  pull(num_women_present)
proportion_women_present <- round((num_women_present / num_present) * 100, 1)

# Calculate the number and proportion of men present and available
num_men_present <- data_edit_current %>%
  summarise(num_men_present = sum(men_at_home, na.rm = TRUE)) %>%
  pull(num_men_present)
proportion_men_present <- round((num_men_present / num_present) * 100, 1)


# Calculate the number of respondents who consented to answer mental health questions
num_consented <- data_edit_mh_current %>%
  filter(mh_consent == "y") %>%
  nrow()
proportion_consented <- round((num_consented / num_present) * 100, 1)

# Calculate the refusal rate for men and women
# For those who consented
men_consented <- data_edit_mh_current %>%
  filter(mh_consent == "y" & sex_mh_answer == "male") %>%
  nrow()
women_consented <- data_edit_mh_current %>%
  filter(mh_consent == "y" & sex_mh_answer == "female") %>%
  nrow()

# For those who did not consent
men_refused <- data_edit_mh_current %>%
  filter(mh_consent == "n" & sex_mh_no_answer == "male") %>%
  nrow()
women_refused <- data_edit_mh_current %>%
  filter(mh_consent == "n" & sex_mh_no_answer == "female") %>%
  nrow()

# Calculate total men and women
total_men <- men_consented + men_refused
total_women <- women_consented + women_refused

# Calculate refusal rates
refusal_rate_men <- round((men_refused / total_men) * 100, 1)
refusal_rate_women <- round((women_refused / total_women) * 100, 1)

#-------------------------------
# Generating mental health scores
#-------------------------------

# Create score for PHQ4 rating
data_edit_mh_current <- data_edit_mh_current %>% 
  mutate(
    nervous_score = case_when(
      mh_nervous == "none" ~ 0,
      mh_nervous == "several_days" ~ 1,
      mh_nervous == "half_days" ~ 2,
      mh_nervous == "all_days" ~ 3,
      mh_nervous == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    worry_score = case_when(
      mh_worry == "none" ~ 0,
      mh_worry == "several_days" ~ 1,
      mh_worry == "half_days" ~ 2,
      mh_worry == "all_days" ~ 3,
      mh_worry == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    depressed_score = case_when(
      mh_depressed == "none" ~ 0,
      mh_depressed == "several_days" ~ 1,
      mh_depressed == "half_days" ~ 2,
      mh_depressed == "all_days" ~ 3,
      mh_depressed == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    interest_score = case_when(
      mh_interest == "none" ~ 0,
      mh_interest == "several_days" ~ 1,
      mh_interest == "half_days" ~ 2,
      mh_interest == "all_days" ~ 3,
      mh_interest == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  # Check if any response is 'decline' and create a flag
  mutate(
    any_decline = ifelse(
      mh_nervous == "decline" | 
        mh_worry == "decline" | 
        mh_depressed == "decline" | 
        mh_interest == "decline", 
      TRUE, FALSE
    )
  ) %>% 
  # Replace scores with NA if any_decline is TRUE
  mutate(
    nervous_score = ifelse(any_decline, NA_real_, nervous_score),
    worry_score = ifelse(any_decline, NA_real_, worry_score),
    depressed_score = ifelse(any_decline, NA_real_, depressed_score),
    interest_score = ifelse(any_decline, NA_real_, interest_score)
  ) %>% 
  # Calculate average score for non-missing values
  mutate(
    avg_score = rowMeans(select(., nervous_score, worry_score, depressed_score, interest_score), na.rm = TRUE),
    nervous_score = ifelse(is.na(nervous_score), avg_score, nervous_score),
    worry_score = ifelse(is.na(worry_score), avg_score, worry_score),
    depressed_score = ifelse(is.na(depressed_score), avg_score, depressed_score),
    interest_score = ifelse(is.na(interest_score), avg_score, interest_score)
  ) %>% 
  # Calculate total scores
  mutate(
    phq4_score = ifelse(any_decline, NA_real_, rowSums(select(., nervous_score, worry_score, depressed_score, interest_score), na.rm = TRUE)),
    sx_dep_score = ifelse(any_decline, NA_real_, rowSums(select(., depressed_score, interest_score), na.rm = TRUE)),
    sx_anx_score = ifelse(any_decline, NA_real_, rowSums(select(., nervous_score, worry_score), na.rm = TRUE))
  ) %>% 
  # Rate the PHQ4 score
  mutate(
    phq4_rate = case_when(
      is.na(phq4_score) ~ NA_character_,
      phq4_score >= 0 & phq4_score <= 2 ~ "Normal",
      phq4_score >= 3 & phq4_score <= 5 ~ "Mild",
      phq4_score >= 6 & phq4_score <= 8 ~ "Moderate",
      phq4_score >= 9 & phq4_score <= 12 ~ "Severe",
      TRUE ~ NA_character_
    ),
    phq4_rate = factor(phq4_rate, levels = c("Normal", "Mild", "Moderate", "Severe"))
  )

# Count and proportion for each category of phq4_rate
total_count <- nrow(data_edit_mh_current)
phq4_summary <- data_edit_mh_current %>% 
  group_by(phq4_rate) %>% 
  summarise(count = n(), proportion = round((count / total_count) * 100, 1))

#-------------------------------
# Table 8. Severity of mental health symptoms, overall and by gender 
#-------------------------------

# Filter data for respondents who consented
data_mh_consent <- data_edit_mh_current %>% filter(mh_consent == "y")

# Count total respondents (excluding NAs in key variables)
total_mh_respondents <- nrow(data_mh_consent %>% filter(!is.na(phq4_rate) & !is.na(sex_mh_answer)))

# Count by gender (excluding NAs)
total_mh_women <- sum(data_mh_consent$sex_mh_answer == "female" & !is.na(data_mh_consent$sex_mh_answer))
total_mh_men <- sum(data_mh_consent$sex_mh_answer == "male" & !is.na(data_mh_consent$sex_mh_answer))

# Create summary table with proportions of women and men for each answer option (excluding NAs)
phq4_summary <- data_mh_consent %>%
  filter(!is.na(phq4_rate) & !is.na(sex_mh_answer)) %>%
  group_by(phq4_rate) %>%
  summarise(
    total_count = n(),
    overall_proportion = round((total_count / total_mh_respondents) * 100, 1),
    women_count_in_group = sum(sex_mh_answer == "female", na.rm = TRUE),
    women_proportion = round((women_count_in_group / total_mh_women) * 100, 1),
    men_count_in_group = sum(sex_mh_answer == "male", na.rm = TRUE),
    men_proportion = round((men_count_in_group / total_mh_men) * 100, 1)
  )

# Create flextable with multi-level header
phq4_table <- flextable(phq4_summary) %>%
  add_header_row(values = c("Severity", "Overall", "Women", "Men"), colwidths = c(1, 2, 2, 2)) %>%
  set_header_labels(
    phq4_rate = " ",
    total_count = "Count",
    overall_proportion = "Proportion (%)",
    women_count_in_group = "Count",
    women_proportion = "Proportion (%)",
    men_count_in_group = "Count",
    men_proportion = "Proportion (%)"
  ) %>%
  merge_v(j = 1) %>%
  merge_h(part = "header") %>%
  #  theme_vanilla() %>%
  width(j = 1, width = 1) %>%
  width(j = 2:7, width = 0.8) %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 10, part = "header") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:7, align = "center", part = "all")#  %>%
#  autofit()

# Print flextable
phq4_table


#-------------------------------
# Proportion with symptoms
#-------------------------------

# Filter data for respondents who consented and have non-missing phq4_score
data_mh_consent <- data_edit_mh_current %>%
  filter(mh_consent == "y" & !is.na(phq4_score))

# Count non-missing entries for table header
non_missing_phq4_count <- sum(!is.na(data_mh_consent$phq4_score))

# Count total respondents excluding NAs in key variables
total_mh_respondents <- nrow(data_mh_consent)

# Count by gender excluding NAs
total_mh_women <- sum(data_mh_consent$sex_mh_answer == "female" & !is.na(data_mh_consent$sex_mh_answer))
total_mh_men <- sum(data_mh_consent$sex_mh_answer == "male" & !is.na(data_mh_consent$sex_mh_answer))

# Calculate the count and proportion of people with a PHQ-4 score of 3 or higher
phq4_high_severity <- data_mh_consent %>%
  filter(phq4_score >= 3) %>%
  summarise(
    count = n(),
    proportion = round((count / total_mh_respondents) * 100, 1)
  )

# Extract the count and proportion as separate values
phq4_high_severity_count <- phq4_high_severity$count
phq4_high_severity_proportion <- phq4_high_severity$proportion

# Print the count and proportion
phq4_high_severity_count
phq4_high_severity_proportion



# Group data by community and calculate the proportion of people with a PHQ-4 score of 3 or higher
phq4_high_severity_per_community <- data_mh_consent %>%
  group_by(community) %>%
  summarise(
    `Number of respondents` = n(),
    `Number of respondents with symptoms` = sum(phq4_score >= 3),
    Proportion = round((`Number of respondents with symptoms` / `Number of respondents`) * 100, 1)
  )

# Create a flextable from the results
phq4_table_comm <- flextable(phq4_high_severity_per_community)

# Rename columns
phq4_table_comm <- set_header_labels(phq4_table_comm,
                                     community = "Community",
                                     `Number of respondents` = "Number of respondents",
                                     `Number of respondents with symptoms` = "Number of respondents with symptoms",
                                     Proportion = "Proportion (%)")



# Group data by community and calculate the proportion of people with a PHQ-4 score of 9 or higher
phq4_9plus_severity_per_community <- data_mh_consent %>%
  group_by(community) %>%
  summarise(
    `Number of respondents` = n(),
    `Number of respondents with symptoms` = sum(phq4_score >= 9),
    Proportion = round((`Number of respondents with symptoms` / `Number of respondents`) * 100, 1)
  )

# Create a flextable from the results
phq4_9plus_table <- flextable(phq4_9plus_severity_per_community)

# Rename columns
phq4_9plus_table <- set_header_labels(phq4_9plus_table,
                                      community = "Community",
                                      `Number of respondents` = "Number of respondents",
                                      `Number of respondents with symptoms` = "Number of respondents with severe symptoms",
                                      Proportion = "Proportion (%)")

# Print the flextable
print(phq4_9plus_table)

#-------------------------------
# Gender differences 
#-------------------------------

### Score of 9 or more ###

# Create a binary variable indicating if the PHQ-4 score is 3 or higher
data_mh_consent <- data_mh_consent %>%
  mutate(phq4_high = ifelse(phq4_score >= 3, "Yes", "No"))

### Score of 6 or more ###

# Create a binary variable indicating if the PHQ-4 score is 6 or higher
data_mh_consent <- data_mh_consent %>%
  mutate(phq4_high = ifelse(phq4_score >= 6, "Yes", "No"))

### Score of 9 or more ###

# Create a binary variable indicating if the PHQ-4 score is 9 or higher
data_mh_consent <- data_mh_consent %>%
  mutate(phq4_high = ifelse(phq4_score >= 9, "Yes", "No"))

## Types of symptoms (anxiety vs depression)

# Create a categorical variable indicating if the symptom is anxiety, depression, both, or none
data_mh_consent <- data_mh_consent %>%
  mutate(
    symptom_type = case_when(
      sx_anx_score >= 3 & sx_dep_score < 3 ~ "Anxiety",
      sx_dep_score >= 3 & sx_anx_score < 3 ~ "Depression",
      sx_anx_score >= 3 & sx_dep_score >= 3 ~ "Both",
      TRUE ~ "None"
    )
  )

# Filter out rows where neither anxiety nor depression symptoms are present
data_mh_consent_filtered <- data_mh_consent %>%
  filter(symptom_type %in% c("Anxiety", "Depression", "Both"))

#-------------------------------
# Per age group
#-------------------------------

# Define age groups
data_mh_consent <- data_mh_consent %>%
  mutate(
    age_group = case_when(
      respondent_age < 30 ~ "<30",
      respondent_age >= 30 & respondent_age <= 44 ~ "30-44",
      respondent_age >= 45 & respondent_age <= 60 ~ "45-59",
      respondent_age >= 60 ~ "60+",
      TRUE ~ NA_character_
    )
  )

# Calculate proportions for depression and anxiety symptoms by age group
age_group_summary <- data_mh_consent %>%
  group_by(age_group) %>%
  summarise(
    total_respondents = n(),
    depression_count = sum(sx_dep_score >= 3, na.rm = TRUE),
    anxiety_count = sum(sx_anx_score >= 3, na.rm = TRUE),
    depression_proportion = round((depression_count / total_respondents) * 100, 1),
    anxiety_proportion = round((anxiety_count / total_respondents) * 100, 1)
  ) %>%
  gather(key = "symptom_type", value = "proportion", depression_proportion, anxiety_proportion) %>%
  mutate(symptom_type = recode(symptom_type, "depression_proportion" = "Depression", "anxiety_proportion" = "Anxiety"))

# Create the plot
mh_age_graph <- ggplot(age_group_summary, aes(x = age_group, y = proportion, fill = symptom_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Proportion (%)", fill = "Symptom Type") +
  scale_fill_manual(values = c("Depression" = "dodgerblue4", "Anxiety" = "lightblue")) +
  scale_y_continuous(limits = c(0, 100)) +  # Set y-axis limit to 100
  theme_minimal()

mh_age_graph

### Looking into age differences ###

## Severity ##

# Create a binary variable indicating if the PHQ-4 score is 6 or higher
data_mh_consent <- data_mh_consent %>%
  mutate(phq4_high = ifelse(phq4_score >= 9, "Yes", "No"))

# Create a contingency table for the chi-square test
age_contingency_table <- table(data_mh_consent$age_group, data_mh_consent$phq4_high)

# # Perform the chi-square test
# age_chi_square_test <- chisq.test(age_contingency_table)
# 
# # Print the contingency table and test results
# age_contingency_table
# age_chi_square_test

## Symptom types ##

# Create a contingency table for the chi-square test
symptom_age_contingency_table <- table(data_mh_consent$age_group, data_mh_consent$symptom_type)
# 
# # Perform the chi-square test
# symptom_age_chi_square_test <- chisq.test(symptom_age_contingency_table)
# 
# # Print the contingency table and test results
# symptom_age_contingency_table
# symptom_age_chi_square_test

#-------------------------------
# Generating mental health scores for full dataset 
#-------------------------------

# Create score for PHQ4 rating
data_edit_mh <- data_edit_mh %>% 
  mutate(
    nervous_score = case_when(
      mh_nervous == "none" ~ 0,
      mh_nervous == "several_days" ~ 1,
      mh_nervous == "half_days" ~ 2,
      mh_nervous == "all_days" ~ 3,
      mh_nervous == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    worry_score = case_when(
      mh_worry == "none" ~ 0,
      mh_worry == "several_days" ~ 1,
      mh_worry == "half_days" ~ 2,
      mh_worry == "all_days" ~ 3,
      mh_worry == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    depressed_score = case_when(
      mh_depressed == "none" ~ 0,
      mh_depressed == "several_days" ~ 1,
      mh_depressed == "half_days" ~ 2,
      mh_depressed == "all_days" ~ 3,
      mh_depressed == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    interest_score = case_when(
      mh_interest == "none" ~ 0,
      mh_interest == "several_days" ~ 1,
      mh_interest == "half_days" ~ 2,
      mh_interest == "all_days" ~ 3,
      mh_interest == "decline" ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  # Check if any response is 'decline' and create a flag
  mutate(
    any_decline = ifelse(
      mh_nervous == "decline" | 
        mh_worry == "decline" | 
        mh_depressed == "decline" | 
        mh_interest == "decline", 
      TRUE, FALSE
    )
  ) %>% 
  # Replace scores with NA if any_decline is TRUE
  mutate(
    nervous_score = ifelse(any_decline, NA_real_, nervous_score),
    worry_score = ifelse(any_decline, NA_real_, worry_score),
    depressed_score = ifelse(any_decline, NA_real_, depressed_score),
    interest_score = ifelse(any_decline, NA_real_, interest_score)
  ) %>% 
  # Calculate average score for non-missing values
  mutate(
    avg_score = rowMeans(select(., nervous_score, worry_score, depressed_score, interest_score), na.rm = TRUE),
    nervous_score = ifelse(is.na(nervous_score), avg_score, nervous_score),
    worry_score = ifelse(is.na(worry_score), avg_score, worry_score),
    depressed_score = ifelse(is.na(depressed_score), avg_score, depressed_score),
    interest_score = ifelse(is.na(interest_score), avg_score, interest_score)
  ) %>% 
  # Calculate total scores
  mutate(
    phq4_score_full = ifelse(any_decline, NA_real_, rowSums(select(., nervous_score, worry_score, depressed_score, interest_score), na.rm = TRUE)),
    sx_dep_score_full = ifelse(any_decline, NA_real_, rowSums(select(., depressed_score, interest_score), na.rm = TRUE)),
    sx_anx_score_full = ifelse(any_decline, NA_real_, rowSums(select(., nervous_score, worry_score), na.rm = TRUE))
  ) %>% 
  # Rate the PHQ4 score
  mutate(
    phq4_rate_full = case_when(
      is.na(phq4_score_full) ~ NA_character_,
      phq4_score_full >= 0 & phq4_score_full <= 2 ~ "Normal",
      phq4_score_full >= 3 & phq4_score_full <= 5 ~ "Mild",
      phq4_score_full >= 6 & phq4_score_full <= 8 ~ "Moderate",
      phq4_score_full >= 9 & phq4_score_full <= 12 ~ "Severe",
      TRUE ~ NA_character_
    ),
    phq4_rate_full = factor(phq4_rate_full, levels = c("Normal", "Mild", "Moderate", "Severe"))
  )

# Count and proportion for each category of phq4_rate
total_count <- nrow(data_edit_mh)
phq4_summary <- data_edit_mh %>% 
  group_by(phq4_rate_full) %>% 
  summarise(count = n(), proportion = round((count / total_count) * 100, 1))


#-------------------------------
# Building Table: Mental Health Symptoms per Round
#-------------------------------

# Filter for respondents who consented and have non-missing PHQ-4 scores
data_mh_consent <- data_edit_mh %>%
  filter(mh_consent == "y" & !is.na(phq4_score_full))

# Summarize data by round_num
mental_health_table <- data_mh_consent %>%
  group_by(round_num) %>%
  summarise(
    valid_respondents = n(),  # Total number of valid respondents per round
    anxiety_symptoms_percent = round(
      sum(sx_anx_score_full >= 3, na.rm = TRUE) / valid_respondents * 100, 1
    ),  # Proportion with symptoms of anxiety
    depression_symptoms_percent = round(
      sum(sx_dep_score_full >= 3, na.rm = TRUE) / valid_respondents * 100, 1
    ),  # Proportion with symptoms of depression
    any_symptoms_percent = round(
      sum(phq4_score_full >= 3, na.rm = TRUE) / valid_respondents * 100, 1
    ),  # Proportion with any symptoms (PHQ-4 score of 3 or higher)
    .groups = "drop"
  )

# Reshape the data for a better table structure
mental_health_table_long <- mental_health_table %>%
  pivot_longer(
    cols = -round_num,  # All columns except "round_num"
    names_to = "indicator",  # Column names become indicators
    values_to = "value"  # Values populate the table
  )

# Reshape further to have round numbers as columns
mental_health_table_wide <- mental_health_table_long %>%
  pivot_wider(
    names_from = round_num,
    values_from = value
  )

# Clean up indicator names
mental_health_table_wide <- mental_health_table_wide %>%
  mutate(
    indicator = case_when(
      indicator == "valid_respondents" ~ "Number of valid respondents",
      indicator == "anxiety_symptoms_percent" ~ "Symptoms of anxiety (%)",
      indicator == "depression_symptoms_percent" ~ "Symptoms of depression (%)",
      indicator == "any_symptoms_percent" ~ "Any symptoms (PHQ-4 score ≥3) (%)",
      TRUE ~ indicator  # Keep original name if no match
    )
  )

# Create the flextable
mental_health_flextable <- flextable(mental_health_table_wide) %>%
  set_header_labels(indicator = "") %>%  # Remove the first column title
  width(j = 1, width = 2) %>%  # Make the first column wider
  width(j = 2:ncol(mental_health_table_wide), width = 1.5) %>%  # Set width for other columns
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header") %>%  # Change font size for the header row
  set_table_properties(layout = "autofit")  # Ensure the table adapts to content

# Print the table
mental_health_flextable

#==============================================================================
# End of code
#==============================================================================