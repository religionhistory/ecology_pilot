# Look at distribution of distance to freshwater and distance to coast between samples

rm(list = ls())

source("../project_support.r")
library(corrplot)
library(tmap)

# Load data
data <- readRDS("./input/data_ecology_wide.rds")
analysis_questions <- fread("./input/analysis_questions.csv")
analysis_2_sample <- read_csv("./input/a_2_dict.csv")
analysis_3_sample <- read_csv("./input/a_3_dict.csv")
analysis_4_sample <- read_csv("./input/a_4_dict.csv")

### Create corrplot
# Remove ID variables
data_ecology <- data %>%
  ungroup() %>%
  mutate(start_temp_var = start_temp_max - start_temp_min) %>%
  mutate(start_prep_var = start_prep_max - start_prep_min) %>%
  group_by(`Entry ID`, `Region ID`) %>%
  mutate(dist_freshwater = min(dist_lakes, dist_rivers)) %>%
  ungroup() %>%
  select(start_temp_avg, start_temp_var, start_prep_avg, start_prep_var, dist_coastline, dist_freshwater, elevation, mammals, plants) %>%
  mutate(dist_coastline = as.numeric(dist_coastline)) %>%
  mutate(dist_freshwater = as.numeric(dist_freshwater)) %>%
  distinct() %>%
  rename(`Average Temperature` = start_temp_avg, `Temperature Variation` = start_temp_var, `Average Precipitation` = start_prep_avg, `Precipitation Variation` = start_prep_var, `Distance to Coast` = dist_coastline, `Distance to Freshwater` = dist_freshwater, Elevation = elevation, `Mammal Richness` = mammals, `Plant Biodiversity` = plants)

# Find correlations
correlations <- cor(data_ecology, method = "spearman", use = "pairwise.complete.obs")

# Save figure
pdf("../figures/corrplot.pdf", width = 10, height = 10)
corrplot(correlations, method="color", 
         type="upper", order="hclust", 
         tl.col="black", tl.srt=45, tl.cex = 1.4, cl.cex = 1.2, 
         diag=FALSE 
)
dev.off()
png("../figures/corrplot.png", width = 1000, height = 1000)
corrplot(correlations, method="color", 
         type="upper", order="hclust", 
         tl.col="black", tl.srt=45, tl.cex = 1.4, cl.cex = 1.2, 
         diag=FALSE 
)
dev.off()

### Plot historical scope of religion variables 
# Convert from wide to long
data_long <- data %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, start_year, end_year, all_of(analysis_questions$`Question ID`)) %>%
  group_by(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, start_year, end_year) %>%
  pivot_longer(cols = all_of(analysis_questions$`Question ID`), names_to = "Question ID", values_to = "Answers") %>%
  left_join(analysis_questions, by = "Question ID") %>%
  mutate(Answers = case_when(Answers == 1 ~ "Yes",
                             Answers == 0 ~ "No",
                             is.na(Answers) ~ "Missing")) 
# Plot data
ggplot(data_long, aes(x = start_year, color = Answers, fill = Answers)) +
  geom_histogram(position="dodge", alpha=0.5, binwidth = 100) +
  xlab("Start Year") +
  ylab("Frequency") + 
  scale_fill_manual(values=c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_color_manual(values=c("#7570b3", "#d95f02", "#1b9e77")) +
  theme_classic() +
  theme(
    axis.text = element_text(colour = "black", size=11),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    strip.text.x = element_text(size = 10.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  facet_wrap(vars(Question), ncol = 1, scales='free') +
  scale_x_continuous(limits=c(-4500,2020), breaks = c(-4000,-3000,-2000,-1000,1,1000,2000)) + 
  scale_y_continuous(expand = c(0, 0), limits=c(0,100))
ggsave("../figures/historical_scope.pdf", width = 10, height = 30)
ggsave("../figures/historical_scope.png", width = 10, height = 30)

### Amount of missingness per variable
data_miss <- data %>%
  select(all_of(analysis_questions$`Question ID`)) %>%
  pivot_longer(cols = all_of(analysis_questions$`Question ID`), names_to = "Question ID", values_to = "Answers") %>%
  left_join(analysis_questions, by = "Question ID") %>%
  group_by(Question) %>%
  mutate(n_missing = sum(is.na(Answers))) %>%
  mutate(n_present = ifelse(!is.na(Answers), 1, 0)) %>%
  mutate(n_present = sum(n_present)) %>%
  mutate(pct_missing = n_missing/(n_missing + n_present) * 100) %>%
  mutate(pct_present = n_present/(n_missing + n_present) * 100) %>%
  select(`Question ID`, Question, pct_missing, pct_present) %>%
  ungroup() %>%
  distinct() %>%
  arrange(pct_missing) %>%
  pivot_longer(cols = c(pct_missing, pct_present), names_to = "Variable", values_to = "Percent") %>%
  mutate(Variable = case_when(Variable == "pct_missing" ~ "Missing", Variable == "pct_present" ~ "Present"))

# Plot data
ggplot(data = data_miss, aes(x = Question, y = Percent, fill = Variable, Percent)) +
  geom_col() +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name="", breaks=c('Present', 'Missing')) +
  coord_flip() +   
  theme_classic2() +
  theme(
    axis.text = element_text(colour = "black"),
    legend.position = "top") 
ggsave("../figures/missing_data.pdf", width = 11, height = 6)
ggsave("../figures/missing_data.png", width = 11, height = 6)

### Plot maps of question answers
# Convert to sf
data_sf <- st_as_sf(data)

# Make valid
data_sf <- st_make_valid(data_sf)

# Calculate centroids
data_centroid <- st_centroid(data_sf) %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, start_year, end_year, geometry)

# Join with answers
data_centroid_ans <- data_long %>%
  left_join(data_centroid, by = c("Entry ID", "Entry name", "Branching question", "Region ID", "Region name", "start_year", "end_year"))

# Convert to sf
data_centroid_ans <- st_as_sf(data_centroid_ans)

# Load map data
data(land, World)

# Plot distributions of answers
tmap_mode("plot")
questions_map <- tm_shape(World) +
  tm_fill() +
  tm_shape(data_centroid_ans) +  
  tm_symbols(col = "Answers", palette = "viridis") +
  tm_facets(by = "Question", ncol = 3, sync = TRUE) +
  tm_layout(legend.show = FALSE)

# Plot maps
pdf("../figures/question_maps.pdf", width = 15, height = 20)
questions_map
dev.off()

### Plot maps of precipitation and public food storage
# Extract public food storage data
food_s1 <- data_centroid_ans %>%
  filter(Question == "Does the religious group in question provide public food storage:") %>%
  filter(Answers != "Missing") %>%
  rename("Public Food Storage" = Answers) %>%
  mutate(`Public Food Storage` = case_when(`Public Food Storage` == "Yes" ~ "Present", `Public Food Storage` == "No" ~ "Absent"))

# Extract precipitation data
precipitation_s1 <- data %>%
  mutate(`Precipitation Variation` = start_prep_max - start_prep_min) %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, start_year, end_year, `Precipitation Variation`, geometry) %>%
  filter(!is.na(`Precipitation Variation`)) %>%
  filter(`Precipitation Variation` != -Inf)

# Convert to sf
precipitation_s1 <- st_as_sf(precipitation_s1)

# Plot map of public food storage
food_map <- tm_shape(World) +
  tm_fill() +
  tm_shape(food_s1) +  
  tm_symbols(col = "Public Food Storage", palette = "Dark2", size = 0.5) +
  tm_layout(title= 'A')

# Create new bounding box, to add extra margin space, so that the legend is readable
new_bbox <- st_bbox(World) 

# Extract ranges
x_range <- new_bbox$xmax - new_bbox$xmin
y_range <- new_bbox$ymax - new_bbox$ymin 

# Add space to the bottom left of the bbox
new_bbox[1] <- new_bbox[1] - (0.19 * x_range) 
new_bbox[2] <- new_bbox[2] - (0.19 * y_range)

# Convert to sf
new_bbox <- st_as_sfc(new_bbox)

# Plot map of precipitation variation
precipitation_map <- tm_shape(World, bbox = new_bbox) + 
  tm_fill() +
  tm_shape(precipitation_s1) +  
  tm_symbols(col = "Precipitation Variation", palette = "-viridis", size = 0.5,
             breaks = c(0, 250, 500, 1000, 2000, 3000, 4000, 5000, Inf),
             legend.hist = TRUE, legend.size.z = 1) +
  tm_layout(title= 'B')
  
# Combine maps
figure_maps <- tmap_arrange(food_map, precipitation_map)

# Save maps
pdf("../figures/precipitaion_food_storage_maps.pdf", width = 6, height = 6.5)
figure_maps
dev.off()
