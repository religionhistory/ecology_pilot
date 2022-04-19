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
  # Calculate distance to freshwater
  group_by(`Entry ID`, `Entry name`, start_year, end_year, `Region ID`, `Region name`, `Branching question`) %>%
  mutate(dist_freshwater = min(dist_lakes, dist_rivers)) %>%
  ungroup() %>%
  # Add ID
  mutate(id = paste0(`Entry ID`, "_", gsub(",", "", `Branching question`), "_", `Region ID`, "_", start_year, "_", end_year))

# Calculate centroids
data_centroid <- st_centroid(data_area)

# Extract latitude and longitude
lat_lon <- data_centroid %>%
  dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                latitude = sf::st_coordinates(.)[,2])
  
# Prepare data for PPCA
ppca_data <- data %>%
  select(start_temp_avg, start_temp_var, start_prep_avg, start_prep_var, elevation, mammals, plants) %>%
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
  mutate(across(start_temp_avg:plants, ~ as.numeric(scale(., center = TRUE, scale = TRUE)))) 

# Probabilistic PCA (PPCA) with 5 principal components
ppca_output <- pca(ppca_data, method="ppca", nPcs=3, center=FALSE)

# Extract PC scores
ppca_scores <- as_tibble(scores(ppca_output))

# Join with metadata
data_ppca <- lat_lon %>%
  bind_cols(ppca_scores)

# Extract at PC loadings
ppca_loadings <- loadings(ppca_output)
ppca_loadings <- round(ppca_loadings, 2)
ppca_loadings <- as.data.frame(ppca_loadings) %>%
  mutate(variable = row.names(ppca_loadings)) %>%
  select(variable, everything())

# Create output directory
make.dir("./output")

# Run model for each condition
for(i in 1:nrow(analysis_questions)) {
  var = analysis_questions$`Question ID`[i]
  run_gls(data = data_ppca, var = var)
}


# Create overall table of results
results <- create_results_table()

# Save output
write_csv(ppca_loadings, "./output/ppca_loadings.csv")
write_csv(results, "./output/results.csv")
