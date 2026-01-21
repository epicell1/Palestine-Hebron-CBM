#==============================================================================
# 
# Purpose: Retrieve Data from Kobo
# Date created: February 2024
# Author: Laurence Campeau
#
#==============================================================================

# Specify the Kobo URL and credentials
kobo_url <- "https://kobo.msf.org"
username <- "msfe_jerusalem_epi"
password <- "45Pancakes##kobo"
form_id <- "a2tgnbHAbiXzW9qeCFfLBX"
api <- "e485119d17c7a2def540665a44be18e7ca3f9704"

# Define the data URL
data_url <- paste0(kobo_url, "/api/v2/assets/", form_id, "/data/")

# Authenticate and extract the data
response <- GET(
  data_url,
  authenticate(username, password),
  accept_json())

# Check for successful response (200 is a successful HTML request)
if (response$status_code == 200)
  # Parse the JSON content
  data <- content(response, "text", encoding = "UTF-8")
  data_json <- fromJSON(data, flatten = TRUE)

# Convert to a data frame
data_raw <- bind_rows(data_json$results, .id = "id")
  
#==============================================================================
# End of code
#==============================================================================

