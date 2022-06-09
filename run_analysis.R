rm(list = ls())

# Load packages and functions
source("./project_support.r")

# Unzip data
unzip("./data/drh.csv.zip", exdir = "./data")

# Make figures and results folders
make.dir("figures")
make.dir("results")

# Transform data
make.dir("./01_transform_data/input")
files <- c("./data/drh.csv", "./data/question_dictionary.csv", "./data/drh_v6_poll.csv", "./data/analysis_questions.csv")
file.copy(files, "./01_transform_data/input", overwrite = TRUE)
setwd("./01_transform_data/")
source("transform_data.R")
setwd("..")

# Clean region data
make.dir("./02_clean_regions/input")
files <- c("./01_transform_data/output/drh_regions.rds", "./data/excluded_entries.csv")
file.copy(files, "./02_clean_regions/input", overwrite = TRUE)
setwd("./02_clean_regions/")
source("clean_regions.R")
setwd("..")

# Filter regions
make.dir("./03_filter_regions/input")
files <- c("./01_transform_data/output/data_wide.csv", "./02_clean_regions/output/drh_regions_clean.rds")
file.copy(files, "./03_filter_regions/input", overwrite = TRUE)
setwd("./03_filter_regions/")
source("filter_regions.R")
setwd("..")

# Join with ecological data
make.dir("./04_join_ecology/input")
files <- c("./03_filter_regions/output/data_wide_filt.csv", "./data/drh_ecology.rds")
file.copy(files, "./04_join_ecology/input", overwrite = TRUE)
setwd("./04_join_ecology/")
source("join_ecology.R")
setwd("..")

# Create inclusion criteria dictionaries per analyses
make.dir("./05_analysis_samples/input")
files <- c("./04_join_ecology/output/data_ecology_wide.rds", "./data/sccs_drh_dict.csv", "./data/sccs_drh_overlaps.csv")
file.copy(files, "./05_analysis_samples/input", overwrite = TRUE)
setwd("./05_analysis_samples/")
source("analysis_samples.R")
setwd("..")

# Run analyses
make.dir("./06_analysis/input")
files <- c("./04_join_ecology/output/data_ecology_wide.rds", "./data/analysis_questions.csv", "./05_analysis_samples/output/a_2_dict.csv", "./05_analysis_samples/output/a_3_dict.csv", "./05_analysis_samples/output/a_4_dict.csv")
file.copy(files, "./06_analysis/input", overwrite = TRUE)
setwd("./06_analysis/")
source("analysis.R")
setwd("..")

# Create figures
make.dir("./07_figures/input")
files <- c("./04_join_ecology/output/data_ecology_wide.rds", "./05_analysis_samples/output/a_2_dict.csv", "./05_analysis_samples/output/a_3_dict.csv", "./05_analysis_samples/output/a_4_dict.csv")
file.copy(files, "./07_figures/input", overwrite = TRUE)
setwd("./07_figures/")
source("figures.R")
setwd("..")

