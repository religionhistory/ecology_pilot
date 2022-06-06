# Create dictionaries for filtering data for inclusion in each analysis sample

source("../project_support.r")

# Load data
data <- readRDS("./input/data_ecology_wide.rds")
sccs <- read_csv("./input/sccs_drh_dict.csv")
sccs_overlaps <- read_csv("./input/sccs_drh_overlaps.csv")

# Add ID 
data <- data %>%
  mutate(ID = paste0(`Entry ID`, "_", gsub(",", "", `Branching question`), "_", `Region ID`, "_", start_year, "_", end_year)) %>% 
  select(ID, `Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, start_year, end_year)

# Filter question and answer data by analysis criteria
# Analysis (1) All included entries - no filtering required
# Analysis (2) All entries with start dates between 4500 BCE to 1500 CE
analysis_2_dict <- data %>%
  filter(start_year > -4500 & start_year < 1500) 
# Analysis (3) SCCS data
analysis_3_dict <- data %>%
  filter(`Entry ID` %in% sccs$DRH_ID)
# Analysis (4) DRH data covering the temporal and geographical regions of the SCCS 
analysis_4_dict <- data %>%
  filter(`Entry ID` %in% sccs_overlaps$`Entry ID`)

# Create output directory
make.dir("./output")

# Save output
write_csv(analysis_2_dict, "./output/a_2_dict.csv")
write_csv(analysis_3_dict, "./output/a_3_dict.csv")
write_csv(analysis_4_dict, "./output/a_4_dict.csv")

# Find the sample size per analysis condition
sample_sizes <- analysis_sample_size()

# Save to results
write_csv(sample_sizes, "./../results/sample_sizes.csv")


