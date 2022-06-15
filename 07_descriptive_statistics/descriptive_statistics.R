# Create tables of descriptive statistics

rm(list = ls())

source("../project_support.r")

# Load data
data <- readRDS("./input/data_ecology_wide.rds")
analysis_questions <- fread("./input/analysis_questions.csv")

# Calculate descriptive statistics for analysis questions
question_statistics <- data %>%
  select(all_of(analysis_questions$`Question ID`)) %>%
  pivot_longer(cols = everything(), names_to = "Question ID", values_to = "Answers") %>%
  filter(!is.na(Answers)) %>%
  group_by(`Question ID`, Answers) %>%
  tally(name = "Frequency") %>%
  group_by(`Question ID`) %>%
  mutate(Percent = Frequency/ sum(Frequency) * 100) %>%
  mutate(Percent = round(Percent, 2)) %>%
  mutate(Answers = case_when(Answers == 1 ~ "Yes", Answers == 0 ~ "No")) %>%
  pivot_wider(names_from = Answers, values_from = c(Frequency, Percent), names_sep = " ") %>%
  mutate(N = sum(`Frequency No`, `Frequency Yes`)) %>%
  ungroup() %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "-", .))) %>%
  left_join(analysis_questions, by = "Question ID") %>%
  select(Question, N, `Frequency Yes`, `Frequency No`, `Percent Yes`, `Percent No`) 

# Calculate descriptive statistics for ecological variables
ecology_statistics <- data %>%
  group_by(`Entry ID`, `Entry name`, start_year, end_year, `Region ID`, `Region name`, `Branching question`) %>%
  mutate(dist_freshwater = min(dist_lakes, dist_rivers)) %>%
  mutate(start_prep_var = start_prep_max - start_prep_min) %>%
  ungroup() %>%
  select(start_temp_avg, start_prep_avg, start_prep_var, dist_coastline, dist_freshwater, elevation, mammals, plants) %>%
  mutate(dist_coastline = as.numeric(dist_coastline)/1000) %>%
  mutate(dist_freshwater = as.numeric(dist_freshwater)/1000) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value) & !is.infinite(Value)) %>%
  group_by(Variable) %>%
  summarise(Mean = mean(Value), SD = sd(Value), Range = paste(round(min(Value), 2), " - ", round(max(Value), 2))) %>%
  mutate(across(Mean:SD, ~ round(., 2))) %>%
  mutate(Variable = case_when(Variable == "start_temp_avg" ~ "Average Temperature (°C)",
                              Variable == "start_prep_avg" ~ "Average Precipitation (mm)",
                              Variable == "start_prep_var" ~ "Precipitation Variation (mm)",
                              Variable == "elevation" ~ "Elevation (m)",
                              Variable == "mammals" ~ "Mammal Richness (N Species)",
                              Variable == "plants" ~ "Plant Biodiversity (N Species)",
                              Variable == "dist_coastline" ~ "Distance to Coast (km)",
                              Variable == "dist_freshwater" ~ "Distance to Freshwater (km)")) %>%
  mutate(row_n = case_when(Variable == "Average Temperature (°C)" ~ 1,
                           Variable == "Average Precipitation (mm)" ~ 2,
                           Variable == "Precipitation Variation (mm)" ~ 3,
                           Variable == "Elevation (m)" ~ 4,
                           Variable == "Mammal Richness (N Species)" ~ 5,
                           Variable == "Plant Biodiversity (N Species)" ~ 6,
                           Variable == "Distance to Coast (km)" ~ 7,
                           Variable == "Distance to Freshwater (km)" ~ 8)) %>%
  arrange(row_n) %>%
  select(-row_n)

# Save outputs
write_csv(question_statistics, "./../results/question_statistics.csv")
write_csv(ecology_statistics, "./../results/ecology_statistics.csv")


