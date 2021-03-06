# Join drh data with ecological data

rm(list = ls())

source("../project_support.r")

# Load data
data_wide <- read_csv("./input/data_wide_filt.csv")
ecology <- readRDS("./input/drh_ecology.rds")

# Join data
data_ecology_wide <- data_wide %>%
  left_join(ecology)

# Create output directory
make.dir("./output")

# Save data
saveRDS(data_ecology_wide, file = "./output/data_ecology_wide.rds")

