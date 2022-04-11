# Extract ecological data

source("../project_support.r")

# Load data
data <- readRDS("../03_filter_regions/output/drh_regions_filt.rds")

# Select unique regions and time periods
unique_regions <- data %>%
  ungroup() %>%
  select(`Entry ID`, start_year, end_year, `Region ID`, `Region name`, geometry) %>%
  distinct()

# Convert to sf
regions_sf <- st_as_sf(unique_regions)

# Find midpoint and nearest millennium 
regions_sf <- regions_sf %>%
  mutate(midpoint = start_year + (abs(start_year - end_year)/2)) %>%
  mutate(midpoint = round(midpoint)) %>%
  mutate(midpoint = ifelse(midpoint == 0, 1, midpoint)) %>%
  mutate(start_millennium = round(start_year/1000) * 1000) %>%
  mutate(end_millennium = round(end_year/1000) * 1000) 

# Load ecological data

# Temperature
# Krapp et al. (2021) for BCE
mean_temp <- brick("../data/ecology/temperature/bio01_800ka.nc")
max_temp <- brick("../data/ecology/temperature/bio05_800ka.nc")
min_temp <- brick("../data/ecology/temperature/bio06_800ka.nc")

# Steiger et al. (2018) for 1CE - 1900CE
# tas_mn = 2 m temperature (reconstruction mean)
temp <- brick("../data/ecology/hydroclimate/da_hydro_AprMar_r.1-2000_d.05-Jan-2018.nc", varname="tas_mn")

# CRU TS v. 4.05 (Harris et al., 2020) for 1901CE - 2020CE
temp_1901ce <- brick("../data/ecology/temperature/cru_ts4.05.1901.2020.tmp.dat.nc", varname="tmp")
temp_min_1901ce <- brick("../data/ecology/temperature/cru_ts4.05.1901.2020.tmn.dat.nc", varname="tmn")
temp_max_1901ce <- brick("../data/ecology/temperature/cru_ts4.05.1901.2020.tmx.dat.nc", varname="tmx")

# Precipitation
# Krapp et al. (2021) for pre-1900CE
mean_prep <- brick("../data/ecology/precipitation/bio12_800ka.nc")
max_prep <- brick("../data/ecology/precipitation/bio14_800ka.nc")
min_prep <- brick("../data/ecology/precipitation/bio13_800ka.nc")

# CRU TS v. 4.05 (Harris et al., 2020) for 1901CE - 2020CE
prep_1901ce <- brick("../data/ecology/precipitation/cru_ts4.05.1901.2020.pre.dat.nc", varname="pre")

# Hydroclimate 
# SPEI
# Steiger et al. (2018) for 1CE - 1900CE
# spei_mn = SPEI 12 months (land only) (reconstruction mean)
spei <- brick("../data/ecology/hydroclimate/da_hydro_AprMar_r.1-2000_d.05-Jan-2018.nc", varname="spei_mn")

# SPEIbase for 1901CE - 2018CE
spei_1901ce <- brick("../data/ecology/hydroclimate/spei12.nc")

# PDSI
# Steiger et al. (2018) for 1CE - 1900CE
# pdsi_mn = PDSI (land only) (reconstruction mean)
pdsi <- brick("../data/ecology/hydroclimate/da_hydro_AprMar_r.1-2000_d.05-Jan-2018.nc", varname="pdsi_mn")

# Dai et al. (2004) for 1870CE - 2005CE
pdsi_1870ce <- brick("../data/ecology/hydroclimate/pdsi.mon.mean.nc")

# Elevation
# GLDAS Elevation at 0.25 degree 
elev_data <- brick("../data/ecology/elevation/GLDASp5_elevation_025d.nc4", varname="GLDAS_elevation")

# Distance from water
# Coastline
coastline <- st_read("../data/ecology/distance_water/ne_10m_coastline.shp")
# Lakes
lakes <- st_read("../data/ecology/distance_water/ne_10m_lakes.shp")
#historic_lakes <- st_read("../data/ecology/distance_water/ne_10m_lakes_historic.shp")
# Rivers
rivers <- st_read("../data/ecology/distance_water/ne_10m_rivers_lake_centerlines_scale_rank.shp")

# Load biome data
biomes <- st_read("../data/ecology/biomes/Ecoregions2017.shp")

# Convert crs to match
coastline <- st_set_crs(coastline, "WGS84")
lakes <- st_set_crs(lakes, "WGS84")
rivers <- st_set_crs(rivers, "WGS84")

# Make lake and biome geometries valid
lakes <- st_make_valid(lakes)
biomes <- st_make_valid(biomes)

# Some of the names of the biome "Tundra" are missing, re-add them
biomes <- biomes %>%
  mutate(BIOME_NAME = ifelse(BIOME_NAME == "N/A" & BIOME_NUM == 11, "Tundra", BIOME_NAME))

# Mammals
# Global Mammal Richness Grids 
mammal_data <- raster(x = "../data/ecology/mammals/all_mammals.tif")

# Plants
# Ellis et al. (2012), using the variable Anthropocene species richness (ASR)
plant_data <- st_read("../data/ecology/plants/ellis_2012_l8_dataset_2012_01_17.shp")

# Add ecological data
# Add temperature data
data_temp <- get_temperature(regions_sf)

# Add precipitation data
data_prep <- get_precipitation(data_temp)

# Add SPEI 
data_spei <- get_spei(data_prep)

# Add PDSI
data_pdsi <- get_pdsi(data_spei)

# Add distance to water
data_dist_water <- get_distance_water(data_pdsi)

# Add elevation data
data_elev <- get_elevation(data_dist_water)

# Add mammal data
data_mammals <- get_mammals(data_elev)

# Add plant data
data_plants <- get_plants(data_mammals)

# Add biome data
data_biome <- get_biomes(data_plants)

# Save data
saveRDS(data_biome, file = "../data/drh_ecology.rds")

rm(list = ls())