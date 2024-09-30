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

if (num_mcdonalds > 0) {
  combined_mcdonalds_df <- combined_mcdonalds_df %>%
    rowwise() %>%
    mutate(farmacias_count = farmacias_within_radius(latitud, longitud, farmacias_df))
  
  avg_farmacias_near_mcdonalds <- mean(combined_mcdonalds_df$farmacias_count, na.rm = TRUE)
  
  # Calculate the number of "farmacias" within a 2 km radius for random points
  random_farmacias_count <- sapply(1:nrow(random_points), function(i) {
    farmacias_within_radius(random_points$latitud[i], random_points$longitud[i], farmacias_df)
  })
  
  avg_farmacias_random <- mean(random_farmacias_count, na.rm = TRUE)
  
  # Select 100 random "farmacias"
  set.seed(123)  # For reproducibility
  random_farmacias <- sample_n(farmacias_df, 100)
  
  # Calculate the number of "farmacias" within a 2 km radius of each selected "farmacia"
  random_farmacias <- random_farmacias %>%
    rowwise() %>%
    mutate(farmacias_nearby = farmacias_within_radius(latitud, longitud, farmacias_df))
  
  avg_farmacias_near_farmacias <- mean(random_farmacias$farmacias_nearby, na.rm = TRUE)
  
  # Print the results
  message(paste("Average number of 'farmacias' within 2 km of McDonald's locations:", avg_farmacias_near_mcdonalds))
  message(paste("Average number of 'farmacias' within random 2 km radii:", avg_farmacias_random))
  message(paste("Average number of 'farmacias' within 2 km of other 'farmacias':", avg_farmacias_near_farmacias))
} else {
  message("No McDonald's locations found.")
}
library(dplyr)

# Assuming the previous code has been run and the following variables are available:
# combined_mcdonalds_df: DataFrame with McDonald's locations and their farmacias_count
# avg_farmacias_near_mcdonalds: Average number of farmacias within 2 km of McDonald's locations

# Find the McDonald's locations with the least number of farmacias surrounding them
mcdonalds_least_farmacias <- combined_mcdonalds_df %>%
  arrange(farmacias_count) %>%
  head(30) %>%
  mutate(farmacias_needed = 0)  # Reset farmacias_needed to 0

# Allocate exactly 200 farmacias in a round-robin fashion
set.seed(123)  # Set seed for reproducibility
total_farmacias_needed <- 0

while (total_farmacias_needed < 200) {
  # Sort the dataframe by the current number of farmacias
  mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
    arrange(farmacias_count)
  
  # Add one farmacia to the location with the least number of farmacias
  mcdonalds_least_farmacias$farmacias_count[1] <- mcdonalds_least_farmacias$farmacias_count[1] + 1
  mcdonalds_least_farmacias$farmacias_needed[1] <- mcdonalds_least_farmacias$farmacias_needed[1] + 1
  total_farmacias_needed <- total_farmacias_needed + 1
}

# Calculate the new density of farmacias after adding the required number
mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
  mutate(new_farmacias_count = farmacias_count + farmacias_needed)

# Calculate the average density of farmacias within a 2 km radius
# Area of a circle with radius 2 km is π * (2^2) = 4π square km
area_2km_radius <- pi * (2^2)

# Calculate the density
mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
  mutate(density = new_farmacias_count / area_2km_radius)

# Output the locations, the number of farmacias to be added, and the new density
result <- mcdonalds_least_farmacias %>%
  select(nom_estab, latitud, longitud, farmacias_count, farmacias_needed, new_farmacias_count, density) %>%
  filter(farmacias_needed > 0)  # Only include locations that need additional farmacias

# Print the results
print(result)
# Calculate and print the total number of farmacias needed
total_farmacias_needed <- sum(mcdonalds_least_farmacias$farmacias_needed)
cat("Total farmacias needed:", total_farmacias_needed, "\n")


if (!require(leaflet)) {
  install.packages("leaflet")
  library(leaflet)
}
library(htmlwidgets)
# Create a leaflet map
map <- leaflet() %>%
  addTiles()  # Add default OpenStreetMap map tiles

# Add McDonald's locations to the map
map <- map %>%
  addCircleMarkers(
    data = result,
    lat = ~latitud,
    lng = ~longitud,
    radius = 5,
    color = "red",
    fill = TRUE,
    fillColor = "red",
    fillOpacity = 0.7,
    popup = ~paste(nom_estab, "<br>Farmacias Count:", farmacias_count, "<br>Farmacias Needed:", farmacias_needed, "<br>New Farmacias Count:", new_farmacias_count, "<br>Density:", round(density, 2))
  )

# Add new farmacias locations to the map
for (i in 1:nrow(result)) {
  for (j in 1:result$farmacias_needed[i]) {
    # Generate random points within a 2 km radius of the McDonald's location
    angle <- runif(1, 0, 2 * pi)
    distance <- runif(1, 0, 2)  # Distance in km
    new_lat <- result$latitud[i] + (distance / 111) * cos(angle)
    new_lng <- result$longitud[i] + (distance / (111 * cos(result$latitud[i] * pi / 180))) * sin(angle)
    
    map <- map %>%
      addCircleMarkers(
        lat = new_lat,
        lng = new_lng,
        radius = 3,
        color = "blue",
        fill = TRUE,
        fillColor = "blue",
        fillOpacity = 0.5,
        popup = paste("New Farmacia near", result$nom_estab[i])
      )
  }
}

# Save the map as an HTML file
saveWidget(map, "C:/Users/Ating/OneDrive/Desktop/mcdonalds_farmacias_map.html", selfcontained = TRUE)

