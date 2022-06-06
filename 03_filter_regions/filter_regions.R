# Filter entries by exclusion criteria

rm(list = ls())

source("../project_support.r")

# Load data
regions <- readRDS("./input/drh_regions_clean.rds")
drh_wide <- read_csv("./input/data_wide.csv")

# Make valid
regions <- st_make_valid(regions)

# Filter question and answer data by region
drh_wide_filt <- drh_wide %>%
  left_join(as_tibble(regions)) %>%
  select(-geometry, -Name, -Description)

# Create output directory
make.dir("./output")

# Save cleaned regions
write_csv(drh_wide_filt, "./output/data_wide_filt.csv")

rm(list = ls())