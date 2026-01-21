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
               lmtest)

#-------------------------------
# Merging data frames
#-------------------------------

# Merge the data frames based on the variable 'id'
merged_data <- data_edit_mh %>%
  inner_join(data_edit_current, by = "id")

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

         
#-------------------------------
# Mental health and financial worsening
#-------------------------------

# Logistic regression to examine the association between worsened financial situation and severe mental health symptoms
logistic_model_finances <- glm(severe_mh ~ finances_worsened, data = merged_data, family = binomial)

# Summarize the logistic regression model
summary(logistic_model_finances)

# Extract coefficients
coef_logistic_finances <- tidy(logistic_model_finances)

# Calculate odds ratio and confidence interval
odds_ratio_logistic_finances <- exp(coef_logistic_finances$estimate)
conf_interval_logistic_finances <- exp(confint(logistic_model_finances))

# Combine results into a data frame
results_logistic_finances <- data.frame(
  Variable = coef_logistic_finances$term,
  Odds_Ratio = odds_ratio_logistic_finances,
  CI_Lower = conf_interval_logistic_finances[, 1],
  CI_Upper = conf_interval_logistic_finances[, 2]
)

# Display the results
print(results_logistic_finances)

## Controlling for age and sex ##

# Create a binary variable for worsened financial situation
merged_data <- merged_data %>%
  mutate(finances_worsened = ifelse(finances_finances_change %in% c("worsened_plus"), 1, 0))

# Ensure respondent_age is treated as a numeric variable
merged_data <- merged_data %>%
  mutate(respondent_age = as.numeric(respondent_age))

# Logistic regression to examine the association between worsened financial situation and severe mental health symptoms, controlling for age and sex
logistic_model_finances <- glm(severe_mh ~ finances_worsened + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Summarize the logistic regression model
summary(logistic_model_finances)

# Extract coefficients
coef_logistic_finances <- tidy(logistic_model_finances)

# Calculate odds ratio and confidence interval
odds_ratio_logistic_finances <- exp(coef_logistic_finances$estimate)
conf_interval_logistic_finances <- exp(confint(logistic_model_finances))

# Combine results into a data frame
results_logistic_finances <- data.frame(
  Variable = coef_logistic_finances$term,
  Odds_Ratio = odds_ratio_logistic_finances,
  CI_Lower = conf_interval_logistic_finances[, 1],
  CI_Upper = conf_interval_logistic_finances[, 2]
)

# Display the results
print(results_logistic_finances)


##### Multilevel model to take into account clustering within households ####

# Ensure respondent_age is treated as a numeric variable
merged_data <- merged_data %>%
  mutate(respondent_age = as.numeric(respondent_age))

# Multilevel logistic regression to examine the association between worsened financial situation and severe mental health symptoms, controlling for age and sex, with random intercept for households
multilevel_model_finances <- glmer(severe_mh ~ finances_worsened + respondent_age + sex_mh_answer + (1 | household_id), data = merged_data, family = binomial)

# Summarize the multilevel logistic regression model
summary(multilevel_model_finances)

# Extract coefficients for fixed effects
coef_multilevel_finances <- tidy(multilevel_model_finances, effects = "fixed")

# Calculate odds ratio for fixed effects
odds_ratio_multilevel_finances <- exp(coef_multilevel_finances$estimate)

# Calculate confidence intervals for fixed effects using the Wald method
conf_interval_multilevel_finances <- exp(confint(multilevel_model_finances, method = "Wald", parm = "beta_"))

# Combine results into a data frame
results_multilevel_finances <- data.frame(
  Variable = coef_multilevel_finances$term,
  Odds_Ratio = odds_ratio_multilevel_finances,
  CI_Lower = conf_interval_multilevel_finances[, 1],
  CI_Upper = conf_interval_multilevel_finances[, 2]
)

# Display the results
print(results_multilevel_finances)

## VERY wide confidence intervals, likely because the financial worsening variable is the same for all households ... 

# Testing for colinearity 

# Calculate VIF for the model
vif_values <- vif(multilevel_model_finances)

# Display the VIF values
print(vif_values)


### Trying with Household ID as a fixed effect instead ###

# Logistic regression to examine the association between worsened financial situation and severe mental health symptoms, controlling for age, sex, and household as a fixed effect
logistic_model_finances <- glm(severe_mh ~ finances_worsened + respondent_age + sex_mh_answer + household_id, data = merged_data, family = binomial)

# Summarize the logistic regression model
summary(logistic_model_finances)

# Extract coefficients for fixed effects
coef_logistic_finances <- tidy(logistic_model_finances)

# Calculate odds ratio for fixed effects
odds_ratio_logistic_finances <- exp(coef_logistic_finances$estimate)

# Calculate confidence intervals for fixed effects
conf_interval_logistic_finances <- exp(confint(logistic_model_finances))

# Combine results into a data frame
results_logistic_finances <- data.frame(
  Variable = coef_logistic_finances$term,
  Odds_Ratio = odds_ratio_logistic_finances,
  CI_Lower = conf_interval_logistic_finances[, 1],
  CI_Upper = conf_interval_logistic_finances[, 2]
)

# Display the results
print(results_logistic_finances)

# Testing for colinearity 

# Calculate VIF for the model
vif_values <- vif(logistic_model_finances)

# Display the VIF values
print(vif_values)



### With clustered standard error ### 


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




#-------------------------------
# Mental health and experiences of violence
#-------------------------------

### Logistic regression ###
### Moderate or severe symptoms ###

# Create a binary variable for experiencing violence
merged_data <- merged_data %>%
  mutate(experienced_violence = ifelse(violence_witness_yesno %in% c("witnessed_and_experienced", "experience"), 1, 0))

# Logistic regression to examine the association between experiencing violence and moderate or severe mental health symptoms
logistic_model <- glm(moderate_severe_mh ~ experienced_violence, data = merged_data, family = binomial)

# Summarize the logistic regression model
summary(logistic_model)

# Extract coefficients
coef_logistic <- tidy(logistic_model)

# Calculate odds ratio and confidence interval
odds_ratio <- exp(coef_logistic$estimate)
conf_interval <- exp(confint(logistic_model))

# Combine results into a data frame
results <- data.frame(
  Variable = coef_logistic$term,
  Odds_Ratio = odds_ratio,
  CI_Lower = conf_interval[, 1],
  CI_Upper = conf_interval[, 2]
)

## Severe symptoms ##

# Create a binary variable for experiencing violence
merged_data <- merged_data %>%
  mutate(experienced_violence = ifelse(violence_witness_yesno %in% c("witnessed_and_experienced", "experience"), 1, 0))

# Logistic regression to examine the association between experiencing violence and severe mental health symptoms
logistic_model <- glm(severe_mh ~ experienced_violence, data = merged_data, family = binomial)

# Summarize the logistic regression model
summary(logistic_model)

# Extract coefficients
coef_logistic <- tidy(logistic_model)

# Calculate odds ratio and confidence interval
odds_ratio <- exp(coef_logistic$estimate)
conf_interval <- exp(confint(logistic_model))

# Combine results into a data frame
results <- data.frame(
  Variable = coef_logistic$term,
  Odds_Ratio = odds_ratio,
  CI_Lower = conf_interval[, 1],
  CI_Upper = conf_interval[, 2]
)

## Controlling for age and sex ##

# Ensure respondent_age is treated as a numeric variable
merged_data <- merged_data %>%
  mutate(respondent_age = as.numeric(respondent_age))

# Logistic regression to examine the association between experiencing violence and severe mental health symptoms, controlling for age and sex
logistic_model <- glm(severe_mh ~ experienced_violence + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Summarize the logistic regression model
summary(logistic_model)

# Extract coefficients
coef_logistic <- tidy(logistic_model)

# Calculate odds ratio and confidence interval
odds_ratio <- exp(coef_logistic$estimate)
conf_interval <- exp(confint(logistic_model))

# Combine results into a data frame
results <- data.frame(
  Variable = coef_logistic$term,
  Odds_Ratio = odds_ratio,
  CI_Lower = conf_interval[, 1],
  CI_Upper = conf_interval[, 2]
)

# Display the results
print(results)



### Multilevel logistic regression ###

# Multilevel logistic regression to examine the association between experiencing violence and severe mental health symptoms, controlling for age and sex, with random intercept for households
multilevel_model <- glmer(severe_mh ~ experienced_violence + respondent_age + sex_mh_answer + (1 | household_id), data = merged_data, family = binomial)

# Summarize the multilevel logistic regression model
summary(multilevel_model)

# Extract coefficients for fixed effects
coef_multilevel <- tidy(multilevel_model, effects = "fixed")

# Calculate odds ratio for fixed effects
odds_ratio_multilevel <- exp(coef_multilevel$estimate)

# Calculate confidence intervals for fixed effects using the Wald method
conf_interval_multilevel <- exp(confint(multilevel_model, method = "Wald", parm = "beta_"))

# Combine results into a data frame
results_multilevel <- data.frame(
  Variable = coef_multilevel$term,
  Odds_Ratio = odds_ratio_multilevel,
  CI_Lower = conf_interval_multilevel[, 1],
  CI_Upper = conf_interval_multilevel[, 2]
)

# Display the results
print(results_multilevel)



### With clustered standard errors ###

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



#-------------------------------
# Mental health and access to water 
#-------------------------------

# Create a binary variable for lacking water at least one day
merged_data <- merged_data %>%
  mutate(lacked_water = ifelse(water_no_water_days %in% c("eleven_fifteen_days", "more_than_fifteen"), 1, 0))

# Logistic regression to examine the association between lacking water and severe mental health symptoms
logistic_model_water <- glm(severe_mh ~ lacked_water, data = merged_data, family = binomial)

# Summarize the logistic regression model
summary(logistic_model_water)

# Extract coefficients
coef_logistic_water <- tidy(logistic_model_water)

# Calculate odds ratio and confidence interval
odds_ratio_logistic_water <- exp(coef_logistic_water$estimate)
conf_interval_logistic_water <- exp(confint(logistic_model_water))

# Combine results into a data frame
results_logistic_water <- data.frame(
  Variable = coef_logistic_water$term,
  Odds_Ratio = odds_ratio_logistic_water,
  CI_Lower = conf_interval_logistic_water[, 1],
  CI_Upper = conf_interval_logistic_water[, 2]
)

# Display the results
print(results_logistic_water)


## Controlling for age and sex ##

# Create a binary variable for lacking water at least one day
merged_data <- merged_data %>%
  mutate(lacked_water = ifelse(water_no_water_days %in% c("eleven_fifteen_days", "more_than_fifteen"), 1, 0))

# Ensure respondent_age is treated as a numeric variable
merged_data <- merged_data %>%
  mutate(respondent_age = as.numeric(respondent_age))

# Logistic regression to examine the association between lacking water and severe mental health symptoms, controlling for age and sex
logistic_model_water <- glm(severe_mh ~ lacked_water + respondent_age + sex_mh_answer, data = merged_data, family = binomial)

# Summarize the logistic regression model
summary(logistic_model_water)

# Extract coefficients
coef_logistic_water <- tidy(logistic_model_water)

# Calculate odds ratio and confidence interval
odds_ratio_logistic_water <- exp(coef_logistic_water$estimate)
conf_interval_logistic_water <- exp(confint(logistic_model_water))

# Combine results into a data frame
results_logistic_water <- data.frame(
  Variable = coef_logistic_water$term,
  Odds_Ratio = odds_ratio_logistic_water,
  CI_Lower = conf_interval_logistic_water[, 1],
  CI_Upper = conf_interval_logistic_water[, 2]
)

# Display the results
print(results_logistic_water)



## Multilevel regression ##

# Multilevel logistic regression to examine the association between lacking water and severe mental health symptoms, controlling for age and sex, with random intercept for households
multilevel_model_water <- glmer(severe_mh ~ lacked_water + respondent_age + sex_mh_answer + (1 | household_id), data = merged_data, family = binomial)

# Summarize the multilevel logistic regression model
summary(multilevel_model_water)

# Extract coefficients for fixed effects
coef_multilevel_water <- tidy(multilevel_model_water, effects = "fixed")

# Calculate odds ratio for fixed effects
odds_ratio_multilevel_water <- exp(coef_multilevel_water$estimate)

# Calculate confidence intervals for fixed effects using the Wald method
conf_interval_multilevel_water <- exp(confint(multilevel_model_water, method = "Wald", parm = "beta_"))

# Combine results into a data frame
results_multilevel_water <- data.frame(
  Variable = coef_multilevel_water$term,
  Odds_Ratio = odds_ratio_multilevel_water,
  CI_Lower = conf_interval_multilevel_water[, 1],
  CI_Upper = conf_interval_multilevel_water[, 2]
)

# Display the results
print(results_multilevel_water)

### With clustered standard errors ###


# Create a binary variable for lacking water at least one day
merged_data <- merged_data %>%
  mutate(lacked_water = ifelse(water_no_water_days %in% c("one_five_days", "six_ten_days", "eleven_fifteen_days", "more_than_fifteen"), 1, 0))

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

#-------------------------------
# All independent variables included 
#-------------------------------

# Ensure respondent_age is treated as a numeric variable
merged_data <- merged_data %>%
  mutate(respondent_age = as.numeric(respondent_age))

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


#-------------------------------
# Having one, two or three adversities 
#-------------------------------


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
