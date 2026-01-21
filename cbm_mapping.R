#==============================================================================
# 
# Purpose: Produce maps
# Date created: February 2025
# Author: Laurence Campeau
#
#==============================================================================
  
#-------------------------------
# Import data
#-------------------------------

# Extract household mapping from Kobo and manually import here:
data_map <- read_excel("C:/Users/msfe-jerusalem-epi/OneDrive - MSF/1_Epidemiology/3_Projects/2_DMC_2024/3_Mapping/2_Data/CBM_Household Mapping_Jan 2025.xlsx")

#-------------------------------
# Clean data
#-------------------------------

# Change name of variables starting with the specified prefix
data_map_edit <- data_map %>%
  rename_with(~ gsub("^(_Record the location of the household_)", "", .x), everything()) %>%
  
# Rename variables
  rename(hh_ID = `Label the household with a Household ID`) %>%
  rename(index = "_index")  %>%

# Keeping only relevant variables
  select(start, end, username, Community, latitude, longitude, altitude, precision, hh_ID, index) %>%

# Change date format
  mutate(end = as.Date(start)) %>%
  mutate(end = as.Date(end)) %>%

# Keep current round only OR entries in Al Majaz from previous round
  filter(
    (end >= as.Date("2024-12-15") & end <= as.Date("2025-02-15")) | 
      (Community == "Isfey Al Majaz" & !is.na(precision)) 
    ) %>%

# Only keep entries with precision of 10 meters or less 
  filter(precision <= 10) %>%
  
# Allocate Community = Wad Jwaya to the entry with _index = 290
  mutate(Community = ifelse(index == 290, "Wad Jwaya", Community))

#-------------------------------
# Generate centroid location using sf package 
#-------------------------------

# Create a data frame with the relevant columns
data_map_edit <- data_map_edit %>%
  select(Community, latitude, longitude)

# Convert the data frame to an sf object
sf_data <- st_as_sf(data_map_edit, coords = c("longitude", "latitude"), crs = 4326)

# Calculate the centroid for each Community
centroids <- sf_data %>%
  group_by(Community) %>%
  summarise(geometry = st_centroid(st_union(geometry)))

# Extract centroid coordinates
centroid_coords <- centroids %>%
  mutate(latitude = st_coordinates(geometry)[, 2],
         longitude = st_coordinates(geometry)[, 1])

#-------------------------------
# Map the coordinates
#-------------------------------

# Create the leaflet map 
leaflet(data = centroid_coords) %>%
  addTiles() %>%  
  addMarkers(~longitude, ~latitude, label = ~Community) %>%
  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE))

# # Trying with labels (too much overlap)
# leaflet(data = centroid_coords) %>%
#   addTiles() %>%  # Add default OpenStreetMap tiles
#   addMarkers(~longitude, ~latitude) %>%  # Add markers
#   addLabelOnlyMarkers(
#     ~longitude, ~latitude, label = ~Community, 
#     labelOptions = labelOptions(
#       noHide = TRUE, 
#       direction = "top",
#       textOnly = TRUE,
#       offset = c(10, 10),  # Moves labels slightly to avoid overlap
#       style = list(
#         "font-weight" = "bold",
#         "font-size" = "14px",
#         "color" = "black",
#         "background-color" = "rgba(255, 255, 255, 0.7)",
#         "padding" = "4px",
#         "border-radius" = "5px"
#       )
#     )
#   )

#==============================================================================
# End of code
#==============================================================================
