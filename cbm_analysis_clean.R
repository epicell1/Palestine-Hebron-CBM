#==============================================================================
# 
# Purpose: Look into factors associated with poor mental health
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
library(pacman)

# Install and load required packages to run script
pacman::p_load(caret,
               MASS,
               broom,
               lme4,
               broom.mixed, 
               car,
               sandwich,
               lmtest,
               ResourceSelection,
               pROC)

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

# Merge the counts back with the original dataset
merged_violence_data <- data_edit_current %>%
  left_join(experienced_events_per_household, by = "id")

#-------------------------------
# Merging data frames
#-------------------------------

# Keep only the variables 'id' and 'experienced_events_per_household' in merged_violence_data
merged_violence_data <- merged_violence_data %>%
  select(id, number_of_violent_events)

# Merge the data frames based on the variable 'id'
merged_data <- data_edit_mh %>%
  inner_join(data_edit_current, by = "id")

# Merge the data frames based on the variable 'id'
merged_data <- merged_data %>%
  inner_join(merged_violence_data, by = "id")

# Replace NA values with 0 in the 'experienced_events_per_household' column
merged_data <- merged_data %>%
  mutate(number_of_violent_events = ifelse(is.na(number_of_violent_events), 0, number_of_violent_events))

#-------------------------------
# Creating variables 
#-------------------------------

# Create a binary variable for severe mental health symptoms
merged_data <- merged_data %>%
  mutate(severe_mh = ifelse(phq4_rate == "Severe", 1, 0))

# Create a binary variable for moderate or severe mental health symptoms
merged_data <- merged_data %>%
  mutate(moderate_severe_mh = ifelse(phq4_rate %in% c("Moderate", "Severe"), 1, 0))

# Creating a unique household ID by combining community and hh_id
merged_data <- merged_data %>%
  mutate(household_id = paste(community.y, hh_id, sep = "_"))

# Ensure respondent_age is treated as a numeric variable
merged_data <- merged_data %>%
  mutate(respondent_age = as.numeric(respondent_age))

# Create a binary variable for worsened financial situation
merged_data <- merged_data %>%
  mutate(finances_worsened = ifelse(finances_finances_change %in% c("worsened_plus"), 1, 0))

# Create a binary variable for experiencing violence
merged_data <- merged_data %>%
  mutate(experienced_violence = ifelse(violence_witness_yesno %in% c("witnessed_and_experienced", "experience"), 1, 0))

# Create a binary variable for having witnessed violence only
merged_data <- merged_data %>%
  mutate(witnessed_violence = ifelse(violence_witness_yesno %in% c("witnessed"), 1, 0))

# Create a binary variable for lacking water ten days or more 
merged_data <- merged_data %>%
  mutate(lacked_water = ifelse(water_no_water_days %in% c("eleven_fifteen_days", "more_than_fifteen"), 1, 0))

# Create a binary variable for lacking water at least one day
merged_data <- merged_data %>%
  mutate(lacked_water_any = ifelse(water_no_water_days %in% c("one_five_days", "six_ten_days", "eleven_fifteen_days", "more_than_fifteen"), 1, 0))

# Remove rows with missing values in relevant columns
merged_data <- merged_data %>%
  filter(!is.na(severe_mh))

#-------------------------------
# Mental health and financial worsening
#-------------------------------

### Logistic regression with clustered standard error, controlling for age and sex ### 

# Logistic regression model without clustering
logistic_model <- glm(severe_mh ~ finances_worsened + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix <- vcovCL(logistic_model, cluster = ~household_id)

# Extract clustered standard errors
clustered_se <- sqrt(diag(clustered_se_matrix))

# Extract coefficients
coef_logistic <- coef(logistic_model)

# Calculate odds ratios
odds_ratios <- exp(coef_logistic)

# Calculate 95% confidence intervals for the odds ratios
ci_lower <- exp(coef_logistic - 1.96 * clustered_se)
ci_upper <- exp(coef_logistic + 1.96 * clustered_se)

# Combine results into a data frame
results_clustered <- data.frame(
  Variable = names(coef_logistic),
  Odds_Ratio = odds_ratios,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# Display the results
print(results_clustered)

### Testing model assumptions ###

# Calculate VIF for the model (colinearity())
vif_values <- vif(logistic_model)

# Display the VIF values
print(vif_values)

# Hosmer-Lemeshow test for goodness of fit 
hosmer_lemeshow <- hoslem.test(merged_data$severe_mh, fitted(logistic_model))
print(hosmer_lemeshow)

# ROC curve and AUC
roc_curve <- roc(merged_data$severe_mh, fitted(logistic_model))
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)

#-------------------------------
# Mental health and experiences of violence
#-------------------------------

### Logistic regression with clustered standard errors, controlling for age and sex ###

# Ensure respondent_age is treated as a numeric variable
merged_data <- merged_data %>%
  mutate(respondent_age = as.numeric(respondent_age))

# Logistic regression to examine the association between experiencing violence and severe mental health symptoms, controlling for age and sex
logistic_model <- glm(severe_mh ~ experienced_violence + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix <- vcovCL(logistic_model, cluster = ~household_id)

# Extract clustered standard errors
clustered_se <- sqrt(diag(clustered_se_matrix))

# Extract coefficients
coef_logistic <- coef(logistic_model)

# Calculate odds ratios
odds_ratios <- exp(coef_logistic)

# Calculate 95% confidence intervals for the odds ratios
ci_lower <- exp(coef_logistic - 1.96 * clustered_se)
ci_upper <- exp(coef_logistic + 1.96 * clustered_se)

# Combine results into a data frame
results_clustered <- data.frame(
  Variable = names(coef_logistic),
  Odds_Ratio = odds_ratios,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# Display the results
print(results_clustered)

### Testing model assumptions ###

# Calculate VIF for the model (colinearity)
vif_values <- vif(logistic_model)
print(vif_values)

# Hosmer-Lemeshow test for goodness of fit 
hosmer_lemeshow <- hoslem.test(merged_data$severe_mh, fitted(logistic_model))
print(hosmer_lemeshow)

# ROC curve and AUC
roc_curve <- roc(merged_data$severe_mh, fitted(logistic_model))
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)

# AIC and BIC
aic_value <- AIC(logistic_model)
aic_value
bic_value <- BIC(logistic_model)
bic_value

#-------------------------------
# Mental health and access to water 
#-------------------------------

### Logistic regression with clustered standard errors, controlling for age and sex ###

# Ensure respondent_age is treated as a numeric variable
merged_data <- merged_data %>%
  mutate(respondent_age = as.numeric(respondent_age))

# Logistic regression to examine the association between lacking water and severe mental health symptoms, controlling for age and sex
logistic_model_water <- glm(severe_mh ~ lacked_water + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix <- vcovCL(logistic_model_water, cluster = ~household_id)

# Extract clustered standard errors
clustered_se <- sqrt(diag(clustered_se_matrix))

# Extract coefficients
coef_logistic_water <- coef(logistic_model_water)

# Calculate odds ratios
odds_ratios_water <- exp(coef_logistic_water)

# Calculate 95% confidence intervals for the odds ratios
ci_lower_water <- exp(coef_logistic_water - 1.96 * clustered_se)
ci_upper_water <- exp(coef_logistic_water + 1.96 * clustered_se)

# Combine results into a data frame
results_clustered_water <- data.frame(
  Variable = names(coef_logistic_water),
  Odds_Ratio = odds_ratios_water,
  CI_Lower = ci_lower_water,
  CI_Upper = ci_upper_water
)

# Display the results
print(results_clustered_water)

### Testing model assumptions ###

# Calculate VIF for the model (colinearity)
vif_values <- vif(logistic_model_water)
print(vif_values)

# Hosmer-Lemeshow test for goodness of fit 
hosmer_lemeshow <- hoslem.test(merged_data$severe_mh, fitted(logistic_model_water))
print(hosmer_lemeshow)

# ROC curve and AUC
roc_curve <- roc(merged_data$severe_mh, fitted(logistic_model_water))
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)

# AIC and BIC
aic_value <- AIC(logistic_model_water)
aic_value
bic_value <- BIC(logistic_model_water)
bic_value

#-------------------------------
# All independent variables included 
#-------------------------------

# Logistic regression to examine the association between all independent variables and severe mental health symptoms, controlling for age and sex
logistic_model_all <- glm(severe_mh ~ lacked_water + finances_worsened + experienced_violence + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix_all <- vcovCL(logistic_model_all, cluster = ~household_id)

# Extract clustered standard errors
clustered_se_all <- sqrt(diag(clustered_se_matrix_all))

# Extract coefficients
coef_logistic_all <- coef(logistic_model_all)

# Calculate odds ratios
odds_ratios_all <- exp(coef_logistic_all)

# Calculate 95% confidence intervals for the odds ratios
ci_lower_all <- exp(coef_logistic_all - 1.96 * clustered_se_all)
ci_upper_all <- exp(coef_logistic_all + 1.96 * clustered_se_all)

# Combine results into a data frame
results_clustered_all <- data.frame(
  Variable = names(coef_logistic_all),
  Odds_Ratio = odds_ratios_all,
  CI_Lower = ci_lower_all,
  CI_Upper = ci_upper_all
)

# Display the results
print(results_clustered_all)

### Testing model assumptions ###

# Calculate VIF for the model (colinearity)
vif_values <- vif(logistic_model_all)
print(vif_values)

# Hosmer-Lemeshow test for goodness of fit 
hosmer_lemeshow <- hoslem.test(merged_data$severe_mh, fitted(logistic_model_all))
print(hosmer_lemeshow)

# ROC curve and AUC
roc_curve <- roc(merged_data$severe_mh, fitted(logistic_model_all))
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)

# AIC and BIC
aic_value <- AIC(logistic_model_all)
aic_value
bic_value <- BIC(logistic_model_all)
bic_value

#-------------------------------
# Having one, two or three adversities 
#-------------------------------

### Linear regression ###

# Create a new variable to count the number of negative factors
merged_data <- merged_data %>%
  mutate(
    negative_factors = as.numeric(lacked_water) + as.numeric(finances_worsened) + as.numeric(experienced_violence)
  )

# Fit a linear regression model to examine the impact of the cumulative negative factors on mental health severity
linear_model <- lm(phq4_score ~ negative_factors + respondent_age + sex_mh_answer, data = merged_data)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix <- vcovCL(linear_model, cluster = ~household_id)

# Extract clustered standard errors
clustered_se <- sqrt(diag(clustered_se_matrix))

# Extract coefficients
coef_linear <- coef(linear_model)

# Calculate 95% confidence intervals for the coefficients
ci_lower <- coef_linear - 1.96 * clustered_se
ci_upper <- coef_linear + 1.96 * clustered_se

# Combine results into a data frame
results_linear <- data.frame(
  Variable = names(coef_linear),
  Coefficient = coef_linear,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# Display the results
print(results_linear)

### Logistic regression ###

# Create a binary variable for mental health severity (e.g., 1 if phq4_score >= 9, else 0)
merged_data <- merged_data %>%
  mutate(
    negative_factors = as.numeric(lacked_water) + as.numeric(finances_worsened) + as.numeric(experienced_violence)
  )

# Fit a logistic regression model
logistic_model_binary <- glm(severe_mh ~ negative_factors + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix_binary <- vcovCL(logistic_model_binary, cluster = ~household_id)

# Extract clustered standard errors
clustered_se_binary <- sqrt(diag(clustered_se_matrix_binary))

# Extract coefficients
coef_logistic_binary <- coef(logistic_model_binary)

# Calculate odds ratios
odds_ratios_binary <- exp(coef_logistic_binary)

# Calculate 95% confidence intervals for the odds ratios
ci_lower_binary <- exp(coef_logistic_binary - 1.96 * clustered_se_binary)
ci_upper_binary <- exp(coef_logistic_binary + 1.96 * clustered_se_binary)

# Combine results into a data frame
results_clustered_binary <- data.frame(
  Variable = names(coef_logistic_binary),
  Odds_Ratio = odds_ratios_binary,
  CI_Lower = ci_lower_binary,
  CI_Upper = ci_upper_binary
)

# Display the results
print(results_clustered_binary)

### Testing model assumptions ###

# Calculate VIF for the model (colinearity)
vif_values <- vif(logistic_model_binary)
print(vif_values)

# Hosmer-Lemeshow test for goodness of fit 
hosmer_lemeshow <- hoslem.test(merged_data$severe_mh, fitted(logistic_model_binary))
print(hosmer_lemeshow)

# ROC curve and AUC
roc_curve <- roc(merged_data$severe_mh, fitted(logistic_model_binary))
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)

#-------------------------------
# Having witnessed, but not experienced violence  
#-------------------------------

# Calculate the frequencies of witnessed_violence
frequencies <- merged_data %>%
  count(witnessed_violence) %>%
  mutate(percentage = n / sum(n) * 100)

# Display the frequencies
print(frequencies)

# Fit a logistic regression model using witnessed_violence
logistic_model_violence <- glm(severe_mh ~ witnessed_violence + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix_violence <- vcovCL(logistic_model_violence, cluster = ~household_id)

# Extract clustered standard errors
clustered_se_violence <- sqrt(diag(clustered_se_matrix_violence))

# Extract coefficients
coef_logistic_violence <- coef(logistic_model_violence)

# Calculate odds ratios
odds_ratios_violence <- exp(coef_logistic_violence)

# Calculate 95% confidence intervals for the odds ratios
ci_lower_violence <- exp(coef_logistic_violence - 1.96 * clustered_se_violence)
ci_upper_violence <- exp(coef_logistic_violence + 1.96 * clustered_se_violence)

# Combine results into a data frame
results_clustered_violence <- data.frame(
  Variable = names(coef_logistic_violence),
  Odds_Ratio = odds_ratios_violence,
  CI_Lower = ci_lower_violence,
  CI_Upper = ci_upper_violence
)

# Display the results
print(results_clustered_violence)

#-------------------------------
#  Number of violent events experienced  
#-------------------------------

# Fit a logistic regression model
logistic_model_binary <- glm(severe_mh ~ number_of_violent_events + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix_binary <- vcovCL(logistic_model_binary, cluster = ~household_id)

# Extract clustered standard errors
clustered_se_binary <- sqrt(diag(clustered_se_matrix_binary))

# Extract coefficients
coef_logistic_binary <- coef(logistic_model_binary)

# Calculate odds ratios
odds_ratios_binary <- exp(coef_logistic_binary)

# Calculate 95% confidence intervals for the odds ratios
ci_lower_binary <- exp(coef_logistic_binary - 1.96 * clustered_se_binary)
ci_upper_binary <- exp(coef_logistic_binary + 1.96 * clustered_se_binary)

# Combine results into a data frame
results_clustered_binary <- data.frame(
  Variable = names(coef_logistic_binary),
  Odds_Ratio = odds_ratios_binary,
  CI_Lower = ci_lower_binary,
  CI_Upper = ci_upper_binary
)

# Display the results
print(results_clustered_binary)


#### Trying with number of violent events as a categorical variable ###

# Categorize the number of violent events
merged_data <- merged_data %>%
  mutate(number_of_violent_events_cat = case_when(
    number_of_violent_events == 0 ~ "0 events",
    number_of_violent_events == 1 ~ "1 event",
    number_of_violent_events >= 2 ~ "2 or more events",
    TRUE ~ NA_character_
  ))

# Fit a logistic regression model
logistic_model_binary <- glm(severe_mh ~ number_of_violent_events_cat + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix_binary <- vcovCL(logistic_model_binary, cluster = ~household_id)

# Extract clustered standard errors
clustered_se_binary <- sqrt(diag(clustered_se_matrix_binary))

# Extract coefficients
coef_logistic_binary <- coef(logistic_model_binary)

# Calculate odds ratios
odds_ratios_binary <- exp(coef_logistic_binary)

# Calculate 95% confidence intervals for the odds ratios
ci_lower_binary <- exp(coef_logistic_binary - 1.96 * clustered_se_binary)
ci_upper_binary <- exp(coef_logistic_binary + 1.96 * clustered_se_binary)

# Combine results into a data frame
results_clustered_binary <- data.frame(
  Variable = names(coef_logistic_binary),
  Odds_Ratio = odds_ratios_binary,
  CI_Lower = ci_lower_binary,
  CI_Upper = ci_upper_binary
)

# Display the results
print(results_clustered_binary)




### Linear regression ###

# Fit a linear regression model to examine the impact of the number of violent events on mental health severity
linear_model <- lm(phq4_score ~ number_of_violent_events + respondent_age + sex_mh_answer, data = merged_data)

# Calculate clustered standard errors using vcovCL
clustered_se_matrix <- vcovCL(linear_model, cluster = ~household_id)

# Extract clustered standard errors
clustered_se <- sqrt(diag(clustered_se_matrix))

# Extract coefficients
coef_linear <- coef(linear_model)

# Calculate 95% confidence intervals for the coefficients
ci_lower <- coef_linear - 1.96 * clustered_se
ci_upper <- coef_linear + 1.96 * clustered_se

# Combine results into a data frame
results_linear <- data.frame(
  Variable = names(coef_linear),
  Coefficient = coef_linear,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# Display the results
print(results_linear)

### Testing model assumptions ###

# Calculate VIF for the model (colinearity)
vif_values <- vif(linear_model)
print(vif_values)

# Hosmer-Lemeshow test for goodness of fit 
hosmer_lemeshow <- hoslem.test(merged_data$phq4_score, fitted(linear_model))
print(hosmer_lemeshow)

# ROC curve and AUC
roc_curve <- roc(merged_data$phq4_score, fitted(linear_model))
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)


#-------------------------------
# Export new df to Excel 
#-------------------------------

# Create the file path with the date appended
file_path <- paste0("C:/Users/msfe-jerusalem-epi/OneDrive - MSF/1_Epidemiology/3_Projects/2_DMC_2024/2_CBS/1_Data/Analysis/merged_data_", current_date, ".xlsx")

# Export to Excel
write_xlsx(merged_data, file_path)

#==============================================================================
# End of code
#==============================================================================
