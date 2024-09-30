library(dplyr)
library(readr)
library(stringr)
library(geosphere)  # For distance calculations

# Load the datasets using relative paths
file_path_1 <- 'denue_inegi_72_1.csv'
file_path_2 <- 'denue_inegi_72_2.csv'
file_path_3 <- 'denue_inegi_46321-46531_.csv'  # New database

# Try loading the files with 'latin1' encoding
df1 <- tryCatch(read_csv(file_path_1, locale = locale(encoding = 'latin1')), error = function(e) { message(paste("Error loading", file_path_1, ":", e)); return(NULL) })
df2 <- tryCatch(read_csv(file_path_2, locale = locale(encoding = 'latin1')), error = function(e) { message(paste("Error loading", file_path_2, ":", e)); return(NULL) })
df3 <- tryCatch(read_csv(file_path_3, locale = locale(encoding = 'latin1')), error = function(e) { message(paste("Error loading", file_path_3, ":", e)); return(NULL) })

# Ensure 'cod_postal', 'cve_ent', and 'telefono' are character type in all data frames
if (!is.null(df1)) {
  df1 <- df1 %>% mutate(cod_postal = as.character(cod_postal), cve_ent = as.character(cve_ent), telefono = as.character(telefono))
}
if (!is.null(df2)) {
  df2 <- df2 %>% mutate(cod_postal = as.character(cod_postal), cve_ent = as.character(cve_ent), telefono = as.character(telefono))
}
if (!is.null(df3)) {
  df3 <- df3 %>% mutate(cod_postal = as.character(cod_postal), cve_ent = as.character(cve_ent), telefono = as.character(telefono))
}

# Define a function to identify McDonald's name variations
is_mcdonalds <- function(name) {
  if (is.na(name)) return(FALSE)
  return(str_detect(name, regex('mc[\\s-]?donald\'?s?', ignore_case = TRUE)))
}

# Define a function to identify "farmacias"
is_farmacia <- function(name) {
  if (is.na(name)) return(FALSE)
  return(str_detect(name, regex('farmacia', ignore_case = TRUE)))
}

# Filter rows that potentially represent McDonald's locations
mcdonalds_df1 <- if (!is.null(df1)) filter(df1, sapply(nom_estab, is_mcdonalds)) else data.frame()
mcdonalds_df2 <- if (!is.null(df2)) filter(df2, sapply(nom_estab, is_mcdonalds)) else data.frame()

# Combine the McDonald's locations from both datasets
combined_mcdonalds_df <- bind_rows(mcdonalds_df1, mcdonalds_df2)

# Filter rows that potentially represent "farmacias"
farmacias_df <- if (!is.null(df3)) filter(df3, sapply(nom_estab, is_farmacia)) else data.frame()

# Number of McDonald's locations found
num_mcdonalds <- nrow(combined_mcdonalds_df)

# Define the bounding box for your map area
min_lat <- min(farmacias_df$latitud, na.rm = TRUE)
max_lat <- max(farmacias_df$latitud, na.rm = TRUE)
min_lon <- min(farmacias_df$longitud, na.rm = TRUE)
max_lon <- max(farmacias_df$longitud, na.rm = TRUE)

# Generate random points within the bounding box
set.seed(123)  # For reproducibility
random_points <- data.frame(
  latitud = runif(num_mcdonalds, min_lat, max_lat),
  longitud = runif(num_mcdonalds, min_lon, max_lon)
)

# Calculate the number of "farmacias" within a 2 km radius of each McDonald's location
farmacias_within_radius <- function(lat, lon, farmacias_df) {
  distances <- distHaversine(matrix(c(lon, lat), ncol = 2), farmacias_df[, c("longitud", "latitud")])
  return(sum(distances <= 2000))
}


# Initialize an empty dataframe to store McDonald's with calculated farmacias_count
mcdonalds_with_farmacias <- combined_mcdonalds_df %>%
  mutate(farmacias_count = NA)

# Iterate through McDonald's locations one by one and calculate farmacias_count
for (i in 1:nrow(combined_mcdonalds_df)) {
  lat <- combined_mcdonalds_df$latitud[i]
  lon <- combined_mcdonalds_df$longitud[i]
  
  # Calculate the number of farmacias within a 2 km radius
  farmacias_count <- farmacias_within_radius(lat, lon, farmacias_df)
  
  # Update the dataframe with the calculated count
  mcdonalds_with_farmacias$farmacias_count[i] <- farmacias_count
  
  # Optional: Print progress
  if (i %% 10 == 0) {
    cat("Processed", i, "of", nrow(combined_mcdonalds_df), "McDonald's locations\n")
  }
}

# Check the output dataframe
head(mcdonalds_with_farmacias)

# Select the 100 McDonald's locations with the fewest farmacias
mcdonalds_least_farmacias <- mcdonalds_with_farmacias %>%
  arrange(farmacias_count) %>%
  head(100) %>%
  mutate(farmacias_needed = 0)  # Initialize farmacias_needed to 0

# Check the output
head(mcdonalds_least_farmacias)

# Reset farmacias_needed to zero
mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
  mutate(farmacias_needed = 0)

# Initialize the total number of farmacias allocated
total_farmacias_needed <- 0

# Loop to allocate farmacias until we reach exactly 200
while (total_farmacias_needed < 200) {
  # Sort the dataframe by the current number of farmacias (to prioritize locations)
  mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
    arrange(farmacias_count)
  
  # Filter locations that can still receive farmacias (less than 3 already allocated)
  can_receive_farmacias <- mcdonalds_least_farmacias %>%
    filter(farmacias_needed < 3)
  
  # Calculate how many more farmacias are needed to reach exactly 200
  farmacias_remaining <- 200 - total_farmacias_needed
  
  # If there are no more locations that can receive farmacias or if we have enough, exit the loop
  if (nrow(can_receive_farmacias) == 0 || farmacias_remaining <= 0) {
    break
  }
  
  # Start allocating farmacias to eligible McDonald's locations
  for (i in 1:nrow(can_receive_farmacias)) {
    if (total_farmacias_needed >= 200) break  # Exit if we've allocated enough
    
    # Determine how many farmacias to add
    if (can_receive_farmacias$farmacias_needed[i] < 2 && farmacias_remaining >= 2) {
      # Add 2 farmacias if possible
      mcdonalds_least_farmacias$farmacias_count[i] <- mcdonalds_least_farmacias$farmacias_count[i] + 2
      mcdonalds_least_farmacias$farmacias_needed[i] <- mcdonalds_least_farmacias$farmacias_needed[i] + 2
      total_farmacias_needed <- total_farmacias_needed + 2
    } else if (can_receive_farmacias$farmacias_needed[i] < 3 && farmacias_remaining > 0) {
      # Otherwise, add 1 farmacia if less than 3 have been allocated
      mcdonalds_least_farmacias$farmacias_count[i] <- mcdonalds_least_farmacias$farmacias_count[i] + 1
      mcdonalds_least_farmacias$farmacias_needed[i] <- mcdonalds_least_farmacias$farmacias_needed[i] + 1
      total_farmacias_needed <- total_farmacias_needed + 1
    }
    
    # Print progress to track the number of farmacias allocated so far
    cat("Total farmacias allocated so far:", total_farmacias_needed, "\n")
    
    # Check the current sum of farmacias_needed
    current_sum_needed <- sum(mcdonalds_least_farmacias$farmacias_needed)
    cat("Current total sum of farmacias_needed:", current_sum_needed, "\n")
  }
}

# Final check to ensure 200 farmacias were allocated
final_total_needed <- sum(mcdonalds_least_farmacias$farmacias_needed)
cat("Final total farmacias allocated:", final_total_needed, "\n")

# Ensure no more than 200 farmacias were allocated
if (final_total_needed > 200) {
  stop("Error: More than 200 farmacias have been allocated!")
}


# Area of a circle with radius 2 km (4π square km)
area_2km_radius <- pi * (2^2)

# Calculate new farmacias count and density
mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
  mutate(new_farmacias_count = farmacias_count + farmacias_needed,
         density = new_farmacias_count / area_2km_radius)

# Output the result with relevant details
result <- mcdonalds_least_farmacias %>%
  select(nom_estab, latitud, longitud, farmacias_count, farmacias_needed, new_farmacias_count, density) %>%
  filter(farmacias_needed > 0)

# Print the results
print(result)
cat("Total farmacias needed:", sum(mcdonalds_least_farmacias$farmacias_needed), "\n")

library(leaflet)
library(htmlwidgets)
# Reset farmacias_needed and farmacias_count columns to zero
combined_mcdonalds_df$farmacias_needed <- 0
combined_mcdonalds_df$farmacias_count <- 0

# Recompute farmacias_count (if needed) using your original function
combined_mcdonalds_df <- combined_mcdonalds_df %>%
  rowwise() %>%
  mutate(farmacias_count = farmacias_within_radius(latitud, longitud, farmacias_df))

# Select the McDonald's locations that need the most and least farmacias
mcdonalds_least_farmacias <- combined_mcdonalds_df %>%
  arrange(farmacias_count) %>%
  head(100) %>%
  mutate(farmacias_needed = 0)  # Reset farmacias_needed to 0

# Global variable to track added points
global_added_points <- data.frame(lat = numeric(0), lng = numeric(0))

# Reset the map (if applicable) and clear markers
map <- leaflet() %>%
  addTiles()

# Adding 3 farmacias to those with the least, and 1 farmacia to those with the most
total_farmacias_needed <- 0

# Add 3 farmacias to McDonald's with the least farmacias
while (total_farmacias_needed < 200) {
  mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
    arrange(farmacias_count)
  
  # For the least 3 farmacias
  for (i in 1:nrow(mcdonalds_least_farmacias)) {
    if (total_farmacias_needed < 200 && mcdonalds_least_farmacias$farmacias_needed[i] < 3) {
      mcdonalds_least_farmacias$farmacias_count[i] <- mcdonalds_least_farmacias$farmacias_count[i] + 1
      mcdonalds_least_farmacias$farmacias_needed[i] <- mcdonalds_least_farmacias$farmacias_needed[i] + 1
      total_farmacias_needed <- total_farmacias_needed + 1
    }
  }
}

# Add McDonald's locations to the map (in red)
map <- map %>%
  addCircleMarkers(
    data = mcdonalds_least_farmacias,
    lat = ~latitud,
    lng = ~longitud,
    radius = 5,
    color = "red",
    fill = TRUE,
    fillColor = "red",
    fillOpacity = 0.7,
    popup = ~paste(nom_estab, "<br>Farmacias Count:", farmacias_count, "<br>Farmacias Needed:", farmacias_needed, "<br>New Farmacias Count:", farmacias_count)
  )

# Add new farmacias locations to the map (in green) for each McDonald's location
for (i in 1:nrow(mcdonalds_least_farmacias)) {
  added_points <- data.frame(lat = numeric(0), lng = numeric(0))
  farmacias_added <- 0
  
  while (farmacias_added < mcdonalds_least_farmacias$farmacias_needed[i]) {
    angle <- runif(1, 0, 2 * pi)
    distance <- runif(1, 0, 2)
    new_lat <- mcdonalds_least_farmacias$latitud[i] + (distance / 111) * cos(angle)
    new_lng <- mcdonalds_least_farmacias$longitud[i] + (distance / (111 * cos(mcdonalds_least_farmacias$latitud[i] * pi / 180))) * sin(angle)
    
    if (all(sqrt((global_added_points$lat - new_lat)^2 + (global_added_points$lng - new_lng)^2) > 0.01) &&
        all(sqrt((added_points$lat - new_lat)^2 + (added_points$lng - new_lng)^2) > 0.01)) {
      
      added_points <- rbind(added_points, data.frame(lat = new_lat, lng = new_lng))
      global_added_points <- rbind(global_added_points, data.frame(lat = new_lat, lng = new_lng))
      
      map <- map %>%
        addCircleMarkers(
          lat = new_lat,
          lng = new_lng,
          radius = 3,
          color = "blue",
          fill = TRUE,
          fillColor = "blue",
          fillOpacity = 0.5,
          popup = paste("New Farmacia near", mcdonalds_least_farmacias$nom_estab[i])
        )
      
      farmacias_added <- farmacias_added + 1
    }
  }
}

# Save the map as an HTML file on your desktop
saveWidget(map, "C:/Users/Ating/OneDrive/Desktop/mcdonalds_farmacias_mapbueno.html", selfcontained = TRUE)
