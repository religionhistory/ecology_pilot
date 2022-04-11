# Clean region data to remove areas not on land

rm(list = ls())

source("../project_support.r")

# Load data
regions <- readRDS("./input/drh_regions.rds")
land <- st_read("../data/landmass/ne_10m_land.shp")
minor_islands <- st_read("../data/landmass/ne_10m_minor_islands.shp")

# Convert to sf
regions <- st_as_sf(regions)

# Join land and minor islands polygons
all_land <- rbind(land, minor_islands)

# Standardize CRS
all_land <- st_set_crs(all_land, "WGS84")

# Make valid
regions <- st_make_valid(regions)
all_land <- st_make_valid(all_land)

# Split into valid and invalid regions
regions_valid <- valid_region(regions)
regions_invalid <- invalid_region(regions)

expect_equal(nrow(regions_valid) + nrow(regions_invalid), nrow(regions))

# Use s2
sf_use_s2(TRUE)

# Extract land only from regions
land_valid <- st_intersection(all_land, regions_valid)

# Make valid
land_valid <- st_make_valid(land_valid)

# Recombine polygons
land_valid <- land_valid %>%
  group_by(`Entry.ID`, `Entry.name`, `Branching.question`, `Region.ID`, `Region.name`, start_year, end_year) %>% 
  summarize(geometry = st_union(geometry), .groups = "keep") %>%
  distinct()

# For invalid regions use planar intersects 
sf_use_s2(FALSE)

# Make valid
regions_invalid <- st_make_valid(regions_invalid)

# Extract land only from regions
land_invalid <- st_intersection(all_land, regions_invalid)

# Recombine polygons
land_invalid <- land_invalid %>%
  group_by(`Entry.ID`, `Entry.name`, `Branching.question`, `Region.ID`, `Region.name`, start_year, end_year) %>% 
  summarize(geometry = st_union(geometry), .groups = "keep") %>%
  distinct()
  
# Use s2
sf_use_s2(TRUE)

# Recombine valid and invalid
regions_land <- bind_rows(land_valid, land_invalid) %>%
  # Rename variables to original
  rename(`Entry ID` = Entry.ID, `Entry name` = Entry.name, `Branching question` = Branching.question, `Region ID` = Region.ID, `Region name` = Region.name)

# Some very small regions (mostly island) are lost by filtering regions by land, so readd these regions
regions_land_no_geo <- as_tibble(regions_land) %>%
  select(-geometry)
regions_missing <- regions %>%
  anti_join(regions_land_no_geo)

# Recombine regions 
regions_land <- bind_rows(regions_land, regions_missing)

expect_equal(nrow(regions_land), nrow(regions))

# Create output directory
make.dir("./output")

# Save cleaned regions
saveRDS(regions_land, file = "./output/drh_regions_clean.rds")

