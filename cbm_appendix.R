#==============================================================================
# 
# Purpose: Build appendix tables
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

#-------------------------------
# Create acronym table
#-------------------------------

# Create a data frame with acronyms and definitions
acronyms <- data.frame(
  Acronym = c("ANC", "PNC", "CHW", "MUAC", "CBM", "DMC", "MOH", "PHQ-4", "MMR"),
  Definition = c(
    "Antenatal Care",
    "Postnatal Care",
    "Community Health Worker",
    "Mid-Upper Arm Circumference",
    "Community-Based Monitoring",
    "Decentralized Model of Care",
    "Ministry of Health (in this context it is used to refer exclusively to the Palestinian Ministry of Health)",
    "Patient Health Questionnaire 4",
    "Measles-Mumps-Rubella"
  )
)

# Create the table using kable
kable(acronyms, format = "markdown", caption = "Table of Acronyms and Definitions")
acronyms