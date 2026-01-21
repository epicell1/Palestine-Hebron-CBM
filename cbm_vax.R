#==============================================================================
# 
# Purpose: Analyzing vaccination data
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Cleaning data frame
#-------------------------------

# Remove variables starting with the specified prefix
data_edit_vax <- data_vax %>%
  rename_with(~ gsub("^(interview_info/full_survey/module_vax/)", "", .x), everything()) 
  
# Removing children aged younger than 12 months since shouldn't be vaccinated yet 
data_edit_vax$age_child_vax <- as.numeric(as.character(data_edit_vax$age_child_vax))

data_edit_vax <- data_edit_vax %>%
  filter(age_child_vax >= 12) 

# Convert date to proper date-time variables
data_edit_vax$doi <- ymd(data_edit_vax$doi)
data_edit_vax$vax_dose1_date <- ymd(data_edit_vax$vax_dose1_date)
data_edit_vax$vax_dose2_date <- ymd(data_edit_vax$vax_dose2_date)

#-------------------------------
# Creating subset with current round only
#-------------------------------

# Assign round number based on parameters in input file
data_edit_vax <- data_edit_vax %>%
  rowwise() %>%  # Ensures operation is row-wise
  mutate(round_num = round_dates$round_num[which(doi >= round_dates$start_date & doi <= round_dates$end_date)[1]]) %>%
  ungroup()

# Create a subset of the dataframe with the current round only
data_edit_vax_current <- data_edit_vax %>%
  filter(round_num == most_recent_round)

#-------------------------------
# Vaccination cards
#-------------------------------

# Counting number of children aged 12-59
num_children_12_59 <- nrow(subset(data_edit_vax_current, age_child_vax >= 12))
num_children_12_59

# Couting number of children with vaccination card 
num_vax_card <- data_edit_vax_current %>%
  filter(vax_card %in% c("yes")) %>%
  nrow()

# Calculate the proportion
prop_vax_card <- round((num_vax_card / nrow(data_edit_vax_current)) * 100, 1)

# Print the results
num_vax_card
prop_vax_card

# Counting those for whom it is elsewhere
num_vax_card_else <- data_edit_vax_current %>%
  filter(vax_card %in% c("elsewhere")) %>%
  nrow()

# Calculate the percentage and round to one decimal place
prop_vax_card_else <- round((num_vax_card_else / nrow(data_edit_vax_current)) * 100, 1)

# Print the results
num_vax_card_else
prop_vax_card_else

# No vaccination card
num_vax_card_no <- data_edit_vax_current %>%
  filter(vax_card %in% c("n", "no")) %>%
  nrow()

# Calculate the percentage and round to one decimal place
prop_vax_card_no <- round((num_vax_card_no / nrow(data_edit_vax_current)) * 100, 1)

# Print the results
num_vax_card_no
prop_vax_card_no

#-------------------------------
# Two dose coverage
#-------------------------------

# Count number of children with vax_card= yes who had vax_dose1 = y and vax_dose2 = y among those aged 18 months and older
num_fully_vax_card <- data_edit_vax_current %>%
  filter(age_child_vax >= 18, vax_card == "yes", vax_dose1 == "y", vax_dose2 == "y") %>%
  nrow()

# Calculate the proportion of fully vaccinated children among those with a vaccination card
denom_vax_card <- data_edit_vax_current %>%
  filter(age_child_vax >= 18, vax_card == "yes") %>%
  nrow()

prop_fully_vax_card <- round((num_fully_vax_card / denom_vax_card) * 100, 1)

# Print the results
num_fully_vax_card
prop_fully_vax_card


# Count children aged 18 months and older with self-reported vaccinations (2 or more doses)
num_fully_vax_self <- data_edit_vax_current %>%
  filter(age_child_vax >= 18, vax_self_report_num >= 2) %>%
  nrow()

# Count total number of age-eligible children (18 months and older)
num_children_18_59 <- data_edit_vax_current %>%
  filter(age_child_vax >= 18) %>%
  nrow()

# Calculate total number of fully vaccinated children (card or self-report)
num_fully_vax_total <- num_fully_vax_card + num_fully_vax_self

# Calculate the proportion of fully vaccinated children and round to one decimal place
prop_fully_vax_total <- round((num_fully_vax_total / num_children_18_59) * 100, 1)

# Print the result
num_fully_vax_self
num_fully_vax_total
prop_fully_vax_total

#-------------------------------
# At least one dose coverage (ever vaccinated)
#-------------------------------

# Count the number of children with at least one dose (vax_dose1 = "y") and vax_card = "yes"
num_ever_vax_card <- data_edit_vax_current %>%
  filter(vax_card == "yes", vax_dose1 == "y") %>%
  nrow()

# Calculate the proportion of ever vaccinated children among those with a vaccination card
prop_ever_vax_card <- round((num_ever_vax_card / num_vax_card) * 100, 1)

# Print the result
num_ever_vax_card
prop_ever_vax_card

# Count children with vax_self_report_num >= 1 (self-reported at least one dose)
num_ever_vax_self <- data_edit_vax_current %>%
  filter(vax_self_report_num >= 1) %>%
  nrow()

# Print the result
num_ever_vax_self

# Calculate total number of children with at least one dose (card or self-report)
num_ever_vax_total <- num_ever_vax_card + num_ever_vax_self

# Print the total count
num_ever_vax_total

# Calculate the proportion of children with at least one dose and round to one decimal place
prop_ever_vax_total <- round((num_ever_vax_total / num_children_12_59) * 100, 1)

# Print the proportion
prop_ever_vax_total

#-------------------------------
# Bar graph
#-------------------------------

# Create a data frame (proportions already in percentage format)
data_edit_vax_current_long <- data.frame(
  Dose = rep(c("At least 1 dose", "2+ doses"), each = 2),
  Proportion = c(prop_ever_vax_total, prop_ever_vax_card,
                 prop_fully_vax_total, prop_fully_vax_card),
  Type = rep(c("Self-reported", "Recorded"), times = 2)
)

# Reorder the "Dose" factor to have "At least 1 dose" first
data_edit_vax_current_long$Dose <- factor(data_edit_vax_current_long$Dose, levels = c("At least 1 dose", "2+ doses"))

# Select two shades of blue from the "Blues" palette
blue_colors <- brewer.pal(3, "Blues")[c(2, 3)]  # Select two shades of blue

# Create the side-by-side bar plot with y-axis scaled to 100%
vax_plot <- ggplot(data_edit_vax_current_long, aes(x = Dose, y = Proportion, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
  labs(x = NULL, y = "Proportion (%)") +  # Updated y-axis label
  scale_fill_manual(values = blue_colors) +  # Apply Blues palette colors
  scale_y_continuous(limits = c(0, 100)) +  # Ensure the y-axis ranges from 0 to 100
  theme_minimal()


#-------------------------------
# Summarize Indicators by Round
#-------------------------------

# Grouping indicators by round_num
data_edit_vax_grouped <- data_edit_vax %>%
  group_by(round_num) %>%
  summarise(
    num_children_surveyed = sum(age_child_vax >= 12, na.rm = TRUE),
    prop_fully_vax_card = round((sum(age_child_vax >= 18 & vax_card == "yes" & vax_dose1 == "y" & vax_dose2 == "y", na.rm = TRUE) / sum(age_child_vax >= 18 & vax_card == "yes", na.rm = TRUE)) * 100, 1),
    prop_fully_vax_total = round(((sum(age_child_vax >= 18 & vax_card == "yes" & vax_dose1 == "y" & vax_dose2 == "y", na.rm = TRUE) + sum(age_child_vax >= 18 & vax_self_report_num >= 2, na.rm = TRUE)) / sum(age_child_vax >= 18, na.rm = TRUE)) * 100, 1),
    prop_ever_vax_card = round((sum(vax_card == "yes" & vax_dose1 == "y", na.rm = TRUE) / sum(vax_card == "yes", na.rm = TRUE)) * 100, 1),
    prop_ever_vax_total = round(((sum(vax_card == "yes" & vax_dose1 == "y", na.rm = TRUE) + sum(vax_self_report_num >= 1, na.rm = TRUE)) / sum(age_child_vax >= 12, na.rm = TRUE)) * 100, 1)
  )

# Reshape the data for better table structure
data_edit_vax_table_long <- data_edit_vax_grouped %>%
  pivot_longer(
    cols = -round_num,  # All columns except "round_num"
    names_to = "indicator",  # Column names become indicators
    values_to = "value"  # Values populate the table
  )

# Reshape further to have rounds as columns
data_edit_vax_table_wide <- data_edit_vax_table_long %>%
  pivot_wider(
    names_from = round_num,
    values_from = value
  )

# Clean up indicator names
data_edit_vax_table_wide <- data_edit_vax_table_wide %>%
  mutate(
    indicator = case_when(
      indicator == "num_children_surveyed" ~ "Number of children surveyed",
      indicator == "prop_fully_vax_card" ~ "Two Doses (card available) (%)",
      indicator == "prop_fully_vax_total" ~ "Two Doses (incl. self-report) (%)",
      indicator == "prop_ever_vax_card" ~ "At least one dose (card available) (%)",
      indicator == "prop_ever_vax_total" ~ "At least one dose (incl. self-report) (%)",
      TRUE ~ indicator  # Keep original name if no match
    )
  )

# Create the flextable
vaccination_flextable <- flextable(data_edit_vax_table_wide) %>%
  set_header_labels(indicator = "") %>%  # Remove the first column title
  width(j = 1, width = 2) %>%  # Make the first column wider
  width(j = 2:ncol(data_edit_vax_table_wide), width = 1.5) %>%  # Set width for other columns
  fontsize(size = 10, part = "all") %>%  # Change font size for the entire table
  fontsize(size = 10, part = "header") %>%  # Change font size for the header row
  set_table_properties(layout = "autofit")  # Ensure the table adapts to content

# Print the table
vaccination_flextable


#==============================================================================
# End of code
#==============================================================================
