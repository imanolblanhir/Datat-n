library(dplyr)
library(readr)
library(stringr)
library(geosphere)

file_path_1 <- 'denue_inegi_72_1.csv'
file_path_2 <- 'denue_inegi_72_2.csv'
file_path_3 <- 'denue_inegi_46321-46531_.csv'

df1 <- tryCatch(read_csv(file_path_1, locale = locale(encoding = 'latin1')), error = function(e) { message(paste("Error loading", file_path_1, ":", e)); return(NULL) })
df2 <- tryCatch(read_csv(file_path_2, locale = locale(encoding = 'latin1')), error = function(e) { message(paste("Error loading", file_path_2, ":", e)); return(NULL) })
df3 <- tryCatch(read_csv(file_path_3, locale = locale(encoding = 'latin1')), error = function(e) { message(paste("Error loading", file_path_3, ":", e)); return(NULL) })

if (!is.null(df1)) {
  df1 <- df1 %>% mutate(cod_postal = as.character(cod_postal), cve_ent = as.character(cve_ent), telefono = as.character(telefono))
}
if (!is.null(df2)) {
  df2 <- df2 %>% mutate(cod_postal = as.character(cod_postal), cve_ent = as.character(cve_ent), telefono = as.character(telefono))
}
if (!is.null(df3)) {
  df3 <- df3 %>% mutate(cod_postal = as.character(cod_postal), cve_ent = as.character(cve_ent), telefono = as.character(telefono))
}

is_mcdonalds <- function(name) {
  if (is.na(name)) return(FALSE)
  return(str_detect(name, regex('mc[\\s-]?donald\'?s?', ignore_case = TRUE)))
}

is_farmacia <- function(name) {
  if (is.na(name)) return(FALSE)
  return(str_detect(name, regex('farmacia', ignore_case = TRUE)))
}

mcdonalds_df1 <- if (!is.null(df1)) filter(df1, sapply(nom_estab, is_mcdonalds)) else data.frame()
mcdonalds_df2 <- if (!is.null(df2)) filter(df2, sapply(nom_estab, is_mcdonalds)) else data.frame()

combined_mcdonalds_df <- bind_rows(mcdonalds_df1, mcdonalds_df2)

farmacias_df <- if (!is.null(df3)) filter(df3, sapply(nom_estab, is_farmacia)) else data.frame()

num_mcdonalds <- nrow(combined_mcdonalds_df)

min_lat <- min(farmacias_df$latitud, na.rm = TRUE)
max_lat <- max(farmacias_df$latitud, na.rm = TRUE)
min_lon <- min(farmacias_df$longitud, na.rm = TRUE)
max_lon <- max(farmacias_df$longitud, na.rm = TRUE)

set.seed(123)
random_points <- data.frame(
  latitud = runif(num_mcdonalds, min_lat, max_lat),
  longitud = runif(num_mcdonalds, min_lon, max_lon)
)

farmacias_within_radius <- function(lat, lon, farmacias_df) {
  distances <- distHaversine(matrix(c(lon, lat), ncol = 2), farmacias_df[, c("longitud", "latitud")])
  return(sum(distances <= 2000))
}

mcdonalds_with_farmacias <- combined_mcdonalds_df %>%
  mutate(farmacias_count = NA)

for (i in 1:nrow(combined_mcdonalds_df)) {
  lat <- combined_mcdonalds_df$latitud[i]
  lon <- combined_mcdonalds_df$longitud[i]
  farmacias_count <- farmacias_within_radius(lat, lon, farmacias_df)
  mcdonalds_with_farmacias$farmacias_count[i] <- farmacias_count
  if (i %% 10 == 0) {
    cat("Processed", i, "of", nrow(combined_mcdonalds_df), "McDonald's locations\n")
  }
}

head(mcdonalds_with_farmacias)

mcdonalds_least_farmacias <- mcdonalds_with_farmacias %>%
  arrange(farmacias_count) %>%
  head(100) %>%
  mutate(farmacias_needed = 0)

head(mcdonalds_least_farmacias)

mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
  mutate(farmacias_needed = 0)

total_farmacias_needed <- 0

while (total_farmacias_needed < 200) {
  mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
    arrange(farmacias_count)
  
  can_receive_farmacias <- mcdonalds_least_farmacias %>%
    filter(farmacias_needed < 3)
  
  farmacias_remaining <- 200 - total_farmacias_needed
  
  if (nrow(can_receive_farmacias) == 0 || farmacias_remaining <= 0) {
    break
  }
  
  for (i in 1:nrow(can_receive_farmacias)) {
    if (total_farmacias_needed >= 200) break
    if (can_receive_farmacias$farmacias_needed[i] < 2 && farmacias_remaining >= 2) {
      mcdonalds_least_farmacias$farmacias_count[i] <- mcdonalds_least_farmacias$farmacias_count[i] + 2
      mcdonalds_least_farmacias$farmacias_needed[i] <- mcdonalds_least_farmacias$farmacias_needed[i] + 2
      total_farmacias_needed <- total_farmacias_needed + 2
    } else if (can_receive_farmacias$farmacias_needed[i] < 3 && farmacias_remaining > 0) {
      mcdonalds_least_farmacias$farmacias_count[i] <- mcdonalds_least_farmacias$farmacias_count[i] + 1
      mcdonalds_least_farmacias$farmacias_needed[i] <- mcdonalds_least_farmacias$farmacias_needed[i] + 1
      total_farmacias_needed <- total_farmacias_needed + 1
    }
    
    cat("Total farmacias allocated so far:", total_farmacias_needed, "\n")
    current_sum_needed <- sum(mcdonalds_least_farmacias$farmacias_needed)
    cat("Current total sum of farmacias_needed:", current_sum_needed, "\n")
  }
}

final_total_needed <- sum(mcdonalds_least_farmacias$farmacias_needed)
cat("Final total farmacias allocated:", final_total_needed, "\n")

if (final_total_needed > 200) {
  stop("Error: More than 200 farmacias have been allocated!")
}

area_2km_radius <- pi * (2^2)

mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
  mutate(new_farmacias_count = farmacias_count + farmacias_needed,
         density = new_farmacias_count / area_2km_radius)

result <- mcdonalds_least_farmacias %>%
  select(nom_estab, latitud, longitud, farmacias_count, farmacias_needed, new_farmacias_count, density) %>%
  filter(farmacias_needed > 0)

print(result)
cat("Total farmacias needed:", sum(mcdonalds_least_farmacias$farmacias_needed), "\n")

library(leaflet)
library(htmlwidgets)

combined_mcdonalds_df$farmacias_needed <- 0
combined_mcdonalds_df$farmacias_count <- 0

combined_mcdonalds_df <- combined_mcdonalds_df %>%
  rowwise() %>%
  mutate(farmacias_count = farmacias_within_radius(latitud, longitud, farmacias_df))

mcdonalds_least_farmacias <- combined_mcdonalds_df %>%
  arrange(farmacias_count) %>%
  head(100) %>%
  mutate(farmacias_needed = 0)

global_added_points <- data.frame(lat = numeric(0), lng = numeric(0))

map <- leaflet() %>%
  addTiles()

total_farmacias_needed <- 0

while (total_farmacias_needed < 200) {
  mcdonalds_least_farmacias <- mcdonalds_least_farmacias %>%
    arrange(farmacias_count)
  
  for (i in 1:nrow(mcdonalds_least_farmacias)) {
    if (total_farmacias_needed < 200 && mcdonalds_least_farmacias$farmacias_needed[i] < 3) {
      mcdonalds_least_farmacias$farmacias_count[i] <- mcdonalds_least_farmacias$farmacias_count[i] + 1
      mcdonalds_least_farmacias$farmacias_needed[i] <- mcdonalds
