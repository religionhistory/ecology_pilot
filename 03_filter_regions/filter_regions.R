# Filter entries with regions < 500,000 km2 and by other exclusion criteria

rm(list = ls())

source("../project_support.r")

# Load data
regions <- readRDS("./input/drh_regions_clean.rds")
drh_long <- read_csv("./input/data_long.csv")
drh_wide <- read_csv("./input/data_wide.csv")
exclude <- read_csv("./input/excluded_entries.csv")

# Make valid
regions <- st_make_valid(regions)

# Filter regions by exclusion criteria
drh_regions_filt <- regions %>%
  anti_join(exclude)

# Filter question and answer data by region
drh_long_filt <- as_tibble(drh_regions_filt) %>%
  left_join(drh_long) %>%
  select(-geometry, -Name, -Description)
drh_wide_filt <- as_tibble(drh_regions_filt) %>%
  left_join(drh_wide) %>%
  select(-geometry, -Name, -Description)

# Create output directory
make.dir("./output")

# Save cleaned regions
write_csv(drh_wide_filt, "./output/data_wide_filt.csv")
write_csv(drh_long_filt, "./output/data_long_filt.csv")
saveRDS(drh_regions_filt, file = "./output/drh_regions_filt.rds")

rm(list = ls())