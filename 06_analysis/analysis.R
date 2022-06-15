# Perform analysis

rm(list = ls())

source("../project_support.r")
library(nlme)

# Load data
data <- readRDS("./input/data_ecology_wide.rds")
analysis_questions <- fread("./input/analysis_questions.csv")
analysis_2_sample <- read_csv("./input/a_2_dict.csv")
analysis_3_sample <- read_csv("./input/a_3_dict.csv")
analysis_4_sample <- read_csv("./input/a_4_dict.csv")

# Calculate variability
data <- data %>%
  mutate(start_temp_var = start_temp_max - start_temp_min) %>%
  mutate(start_prep_var = start_prep_max - start_prep_min) 

# Convert to sf
data_sf <- st_as_sf(data)

# Calculate area
data_area <- data_sf %>%
  # Convert -Inf and NaN to NA
  mutate(across(oa_temp_avg:plants, ~ ifelse(is.nan(.), NA, .))) %>%
  mutate(across(oa_temp_avg:plants, ~ ifelse(is.infinite(.), NA, .))) %>%
  # Scale distance and numeric variables
  mutate(across(oa_temp_avg:plants, ~ as.numeric(scale(., center = TRUE)))) %>%
  # Calculate area
  mutate(area = as.numeric(st_area(.)/1000000)) %>%
  mutate(log10_area = log10(area)) %>%
  # Add ID
  mutate(id = paste0(`Entry ID`, "_", gsub(",", "", `Branching question`), "_", `Region ID`, "_", start_year, "_", end_year))

# Calculate centroids
data_centroid <- st_centroid(data_area)

# Extract latitude and longitude
lat_lon <- data_centroid %>%
  dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                latitude = sf::st_coordinates(.)[,2]) %>%
  as_tibble() %>%
  select(`Entry ID`, `Region ID`, longitude, latitude) %>%
  distinct()

# Prepare data for analysis
analysis_data <- data %>%
  # Calculate distance to freshwater
  group_by(`Entry ID`, `Entry name`, start_year, end_year, `Region ID`, `Region name`, `Branching question`) %>%
  mutate(dist_freshwater = min(dist_lakes, dist_rivers)) %>%
  ungroup() %>%
  # Convert -Inf and NaN to NA
  mutate(across(start_temp_avg:plants, ~ ifelse(is.nan(.), NA, .))) %>%
  mutate(across(start_temp_avg:plants, ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(start_prep_var = ifelse(is.infinite(start_prep_var), NA, start_prep_var)) %>%
  # Transform variables
  # Square root
  mutate(start_prep_avg = sqrt(start_prep_avg), plants = sqrt(plants), mammals = sqrt(mammals)) %>%
  # Squared
  mutate(start_temp_avg = start_temp_avg^2) %>%
  # Offset 0 values for logging
  mutate(start_prep_var = ifelse(start_prep_var == 0, 0.001, start_prep_var)) %>%
  # Convert -Inf and NaN to NA
  mutate(across(start_temp_avg:plants, ~ ifelse(is.nan(.), NA, .))) %>%
  mutate(across(start_temp_avg:plants, ~ ifelse(is.infinite(.), NA, .))) %>%
  # Logged
  mutate(start_temp_var = log(start_temp_var), start_prep_var = log(start_prep_var)) %>%
  # Scale distance and numeric variables
  mutate(across(start_temp_avg:plants, ~ as.numeric(scale(., center = TRUE, scale = TRUE)))) %>%
  # Join with latitude and longitude
  left_join(lat_lon, by = c("Entry ID", "Region ID")) %>% 
  # Remove duplicate rows
  distinct()

# Create output directory
make.dir("./output")

# Run model for each condition
for(i in 1:nrow(analysis_questions)) {
  var = analysis_questions$`Question ID`[i]
  run_gls(data = analysis_data, var = var)
}

# Find sample size for each variable/condition
sample_size_list <- list()
for(i in 1:nrow(analysis_questions)) {
  var = analysis_questions$`Question ID`[i]
  sample_size_list[[i]] <- get_sample_size(var)
}
sample_sizes <- bind_rows(sample_size_list)

# Join with metadata
sample_sizes <- sample_sizes %>%
  left_join(analysis_questions) %>%
  select(Question, everything())

# Save output
write_csv(sample_sizes, "./../results/sample_sizes.csv")

# Create tables of results
create_results_tables()
