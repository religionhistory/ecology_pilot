# Look at distribution of distance to freshwater and distance to coast between samples

rm(list = ls())

source("../project_support.r")
library(ggpubr)
library(patchwork)
library(corrplot)

# Load data
data <- readRDS("./input/data_ecology_wide.rds")
analysis_2_sample <- read_csv("./input/a_2_dict.csv")
analysis_3_sample <- read_csv("./input/a_3_dict.csv")
analysis_4_sample <- read_csv("./input/a_4_dict.csv")

# Calculate distance to freshwater
data <- data %>%
  group_by(`Entry ID`, `Entry name`, start_year, end_year, `Region ID`, `Region name`, `Branching question`) %>%
  mutate(dist_freshwater = min(dist_lakes, dist_rivers)) %>%
  ungroup() %>%
  # Convert to numeric
  mutate(dist_freshwater = as.numeric(dist_freshwater)) %>%
  mutate(dist_coastline = as.numeric(dist_coastline)) %>%
  # Convert to km
  mutate(dist_freshwater = dist_freshwater/1000) %>%
  mutate(dist_coastline = dist_coastline/1000) %>%
  # Add ID
  mutate(id = paste0(`Entry ID`, "_", gsub(",", "", `Branching question`), "_", `Region ID`, "_", start_year, "_", end_year))

# Extract distance to freshwater per sample
freshwater_s1 <- data %>%
  mutate(Sample = "Sample 1") %>%
  select(Sample, dist_freshwater)
freshwater_s2 <- data %>%
  filter(id %in% analysis_2_sample$ID) %>%
  mutate(Sample = "Sample 2") %>%
  select(Sample, dist_freshwater)
freshwater_s3 <- data %>%
  filter(id %in% analysis_3_sample$ID) %>%
  mutate(Sample = "Sample 3") %>%
  select(Sample, dist_freshwater)
freshwater_s4 <- data %>%
  filter(id %in% analysis_4_sample$ID) %>%
  mutate(Sample = "Sample 4") %>%
  select(Sample, dist_freshwater)
freshwater <- bind_rows(freshwater_s1, freshwater_s2, freshwater_s3, freshwater_s4) 

# Add comparisons
freshwater_sample_comparisons <- list(c("Sample 1", "Sample 2"), c("Sample 2", "Sample 3"), c("Sample 1", "Sample 3"), c("Sample 2", "Sample 4"), c("Sample 1", "Sample 4"))

# Plot data
freshwater_plot <- ggboxplot(freshwater, x = "Sample", y = "dist_freshwater") + 
  ylab("Distance to Freshwater (km)") +
  stat_compare_means(label.x = 2, label.y = 8300) +
  stat_compare_means(comparisons = freshwater_sample_comparisons, label =  "p.signif") 
freshwater_plot
ggsave("../figures/freshwater.pdf", width = 5, height = 5)
ggsave("../figures/freshwater.png", width = 5, height = 5)

# Look at start year and distance to coast
ggplot(data, aes(x = start_year, y = dist_freshwater)) +
  geom_point() + 
  ylab("Distance to Freshwater (km)") +
  xlab("Start Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(-50, 5500)) +
  theme_classic2() +
  theme(
    axis.text = element_text(colour = "black"))
ggsave("../figures/start_year_freshwater.pdf", width = 6, height = 4)
ggsave("../figures/start_year_freshwater.png", width = 6, height = 4)

# Extract distance to coast per sample
coastline_s1 <- data %>%
  mutate(Sample = "Sample 1") %>%
  select(Sample, dist_coastline)
coastline_s2 <- data %>%
  filter(id %in% analysis_2_sample$ID) %>%
  mutate(Sample = "Sample 2") %>%
  select(Sample, dist_coastline)
coastline_s3 <- data %>%
  filter(id %in% analysis_3_sample$ID) %>%
  mutate(Sample = "Sample 3") %>%
  select(Sample, dist_coastline)
coastline_s4 <- data %>%
  filter(id %in% analysis_4_sample$ID) %>%
  mutate(Sample = "Sample 4") %>%
  select(Sample, dist_coastline)
coastline <- bind_rows(coastline_s1, coastline_s2, coastline_s3, coastline_s4) 

# Add comparisons
coastline_sample_comparisons <- list(c("Sample 2", "Sample 3"), c("Sample 1", "Sample 3"), c("Sample 2", "Sample 4"), c("Sample 1", "Sample 4"))

# Plot data
coastline_plot <- ggboxplot(coastline, x = "Sample", y = "dist_coastline") + 
  ylab("Distance to Coast (km)") +
  stat_compare_means(label.x = 2, label.y = 2800) +
  stat_compare_means(comparisons = coastline_sample_comparisons, label =  "p.signif") 
coastline_plot
ggsave("../figures/coastline.pdf", width = 5, height = 5)
ggsave("../figures/coastline.png", width = 5, height = 5)

# Look at start year and distance to coast
ggplot(data, aes(x = start_year, y = dist_coastline)) +
  geom_point() + 
  ylab("Distance to Coast (km)") +
  xlab("Start Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 2000)) +
  theme_classic2() +
  theme(
    axis.text = element_text(colour = "black"))
ggsave("../figures/start_year_coastline.pdf", width = 6, height = 4)
ggsave("../figures/start_year_coastline.png", width = 6, height = 4)

# Combine figures
patchwork <- freshwater_plot + coastline_plot
patchwork + plot_annotation(tag_levels = 'A')
ggsave("../figures/figure_1.pdf", width = 10, height = 5)
ggsave("../figures/figure_1.png", width = 10, height = 5)

# Create corrplot
# Remove ID variables
data_ecology <- data %>%
  ungroup() %>%
  mutate(start_temp_var = start_temp_max - start_temp_min) %>%
  mutate(start_prep_var = start_prep_max - start_prep_min) %>%
  group_by(`Entry ID`, `Region ID`) %>%
  mutate(dist_freshwater = min(dist_lakes, dist_rivers)) %>%
  ungroup() %>%
  select(start_temp_avg, start_temp_var, start_prep_avg, start_prep_var, start_spei, start_pdsi, dist_coastline, dist_freshwater, elevation, mammals, plants) %>%
  mutate(dist_coastline = as.numeric(dist_coastline)) %>%
  mutate(dist_freshwater = as.numeric(dist_freshwater)) %>%
  distinct() %>%
  rename(`Average Temperature` = start_temp_avg, `Temperature Variation` = start_temp_var, `Average Precipitation` = start_prep_avg, `Precipitation Variation` = start_prep_var, `SPEI` = start_spei, `PDSI` = start_pdsi, `Distance to Coast` = dist_coastline, `Distance to Freshwater` = dist_freshwater, Elevation = elevation, `Mammal Richness` = mammals, `Plant Biodiversity` = plants)

# Find correlations
correlations <- cor(data_ecology, method = "spearman", use = "pairwise.complete.obs")

# Save Figure
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
