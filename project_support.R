
# Load libraries
library(raster)
library(tidyverse)
library(data.table)
library(sf)
library(exactextractr)
library(naniar)
library(splitstackshape)
library(testthat)

# Load functions 

# Create a new directory or delete and replace an existing directory
# From https://stackoverflow.com/questions/38375305/how-to-overwrite-a-folder-on-the-computer-using-functions-in-r
make.dir <- function(fp) {
  if(!file.exists(fp)) {  # If the folder does not exist, create a new one
    make.dir(dirname(fp))
    dir.create(fp)
  } else {   # If it existed, delete and replace with a new one  
    unlink(fp, recursive = FALSE)
    dir.create(fp)
  }
} 

# Find regions used by DRH entries
drh_regions <- function(data) {
  # Extract entry IDs
  region_ID <- paste(unique(data$`Region ID`), ".kml", sep="")
  kml_loc <- file.info(list.files(path = "../data", pattern="*.kml", full.names = T, recursive = T), extra_cols = FALSE) %>% filter(mtime == max(as.POSIXct(mtime))) %>% filter(row_number() == 1)
  region_ID <- paste(gsub("[^/]*$", "", row.names(kml_loc)), region_ID, sep="")
  
  # Create list of sf polygons from kml
  sf_poly_list <- list()
  for (i in 1:length(region_ID)){
    sf_poly_list[[i]] <- st_read(region_ID[i])
  }
  
  # Reduce to single data frame
  sf_df <- reduce(sf_poly_list, rbind)
  
  # Make valid
  sf_df <- st_make_valid(sf_df)
  
  # Rename for joining
  sf_df <- sf_df %>%
  mutate(`Region name` = Name)
}

# Find the mean, min and max values per region per year
get_values_year <- function(values, coverage){
  # Find mean value
  mean_values <- values %>%
    # Get spatially-weighted average
    summarize(across(everything(), ~ weighted.mean(.x, coverage, na.rm=TRUE)))
  # Remove rows with NA
  values <- values %>%
    drop_na()
  # Find min per year
  min_year <- values %>%
    summarize(across(everything(), ~ min(.))) %>%
    pivot_longer(everything(), names_to	= "year", values_to = "min")
  # Find max per year
  max_year <- values %>%
    summarize(across(everything(), ~ max(.))) %>%
    pivot_longer(everything(), names_to	= "year", values_to = "max")
  # Find mean value
  var_values <- mean_values %>%
    pivot_longer(everything(), names_to	= "year", values_to = "mean") %>%
    left_join(min_year, by = "year") %>%
    left_join(max_year, by = "year")
}

# Find the mean, min and max values per region with single year
get_single_values <- function(values, coverage){
  # Find overall min
  min_value <- min(values, na.rm = TRUE) 
  # Find overall max
  max_value <- max(values, na.rm = TRUE)
  # Find mean value
  var_values <- data.frame(value = values) %>%
    # Get spatially-weighted average
    summarize(across(everything(), ~ weighted.mean(.x, coverage, na.rm=TRUE))) %>%
    # Find mean over years
    mutate(mean = rowMeans(.)) %>%
    dplyr::select(mean) %>%
    mutate(min = min_value, max = max_value)
}

# Find mean value per region with single year or single dataset
get_single_mean <- function(values, coverage){
  # Find mean value
  var_values <- data.frame(values = values) %>%
    # Get spatially-weighted average
    summarize(across(everything(), ~ weighted.mean(.x, coverage, na.rm=TRUE)))
}

# Extract mean, min and max temperature for each region/date range
get_temperature <- function(data){
  
  # Split into start date BCE & end date BCE, start BCE & end pre-1900CE, start BCE & end post-1900CE, start CE & end pre-1900CE, start CE & end post-1900CE
  bce <- data %>%
    filter(start_year < 0 & end_year < 0) 
  start_bce_end_pre_1900 <- data %>%
    filter(start_year < 0 & end_year > 0 & end_year <= 1900)
  start_bce_end_post_1900 <- data %>%
    filter(start_year < 0 & end_year > 0 & end_year > 1900) %>%
    mutate(end_year_2020 = ifelse(end_year > 2020, 2020, end_year)) 
  start_end_pre_1900 <- data %>%
    filter(start_year > 0 & end_year > 0 & end_year <= 1900)
  start_pre_end_post_1900 <- data %>%
    filter(start_year > 0 & start_year <= 1900 & end_year > 0 & end_year > 1900) %>%
    mutate(end_year_2020 = ifelse(end_year > 2020, 2020, end_year))
  start_end_post_1900 <- data %>%
    filter(start_year > 1900 & end_year > 0 & end_year > 1900) %>%
    mutate(end_year_2020 = ifelse(end_year > 2020, 2020, end_year)) %>%
    mutate(start_year_2020 = ifelse(start_year > 2020, 2020, start_year))
  
  # Extract raster layer names
  krapp_temp_name <- names(mean_temp)
  temp_name <- names(temp)
  temp_name_1901ce <- names(temp_1901ce)
  
  # Extract years in overall dataset
  krapp_years <- data.frame(name = krapp_temp_name) %>%
    mutate(year = gsub("X\\.", "", name)) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(year = gsub("e.05", "00000", year)) %>%
    mutate(year = as.numeric(year)) %>%
    # Convert to BCE/CE (rounding to 0 = 2000, rather than 0 = 1950)
    mutate(year_ce = (year - 2000) * -1) %>%
    # Filter years not present in the overall dataset
    filter(year_ce >= min(data$start_year))
  
  # Subset temperature by years
  mean_temp <- subset(mean_temp, krapp_years$name)
  min_temp <- subset(min_temp, krapp_years$name)
  max_temp <- subset(max_temp, krapp_years$name)
  
  # Find mean, min and max temperature per time period
  # BCE
  bce_list <- list()
  for (i in 1:nrow(bce)){
    # Extract years
    years <- krapp_years %>%
      filter(year_ce >= bce$start_millennium[i] & year_ce <= bce$end_millennium[i])
    # Subset temperature by years
    mean_temp_years <- subset(mean_temp, years$name)
    min_temp_years <- subset(min_temp, years$name)
    max_temp_years <- subset(max_temp, years$name)
    # Extract average, min and max temperature
    if(nrow(years) > 1) {
      temp_min <- exact_extract(min_temp_years, bce$geometry[i], fun = "min")
      temp_max <- exact_extract(max_temp_years, bce$geometry[i], fun = "max")
      temp_mean <- exact_extract(mean_temp_years, bce$geometry[i], fun = get_single_mean)
      start_temp_min <- temp_min[,1]
      mid_temp_min <- temp_min[,round(ncol(temp_min)/2)]
      end_temp_min <- temp_min[,ncol(temp_min)]
      oa_temp_min <- min(temp_min, na.rm = TRUE)
      start_temp_max <- temp_max[,1]
      mid_temp_max <- temp_max[,round(ncol(temp_max)/2)]
      end_temp_max <- temp_max[,ncol(temp_max)]
      oa_temp_max <-  max(temp_max, na.rm = TRUE)
      start_temp_avg <- temp_mean[,1]
      mid_temp_avg <- temp_mean[,round(ncol(temp_mean)/2)]
      end_temp_avg <- temp_mean[,ncol(temp_mean)]
      temp_values <- temp_mean %>%
        # Find mean over years
        mutate(oa_temp_avg = rowMeans(.)) %>%
        dplyr::select(oa_temp_avg) %>%
        mutate(oa_temp_min = oa_temp_min, oa_temp_max = oa_temp_max, start_temp_avg = start_temp_avg, start_temp_min = start_temp_min, start_temp_max = start_temp_max, mid_temp_avg = mid_temp_avg, mid_temp_min = mid_temp_min, mid_temp_max = mid_temp_max, end_temp_avg = end_temp_avg, end_temp_min = end_temp_min, end_temp_max = end_temp_max)
      bce_list[[i]] <- temp_values
    } else {
      temp_min <- exact_extract(min_temp_years, bce$geometry[i], fun = "min")
      temp_max <- exact_extract(max_temp_years, bce$geometry[i], fun = "max")
      temp_mean <- exact_extract(mean_temp_years, bce$geometry[i], fun = get_single_mean)
      temp_values <- data.frame(oa_temp_avg = temp_mean[1,], oa_temp_min = temp_min, oa_temp_max = temp_max, start_temp_avg = temp_mean[1,], start_temp_min = temp_min, start_temp_max = temp_max, mid_temp_avg = temp_mean[1,], mid_temp_min = temp_min, mid_temp_max = temp_max, end_temp_avg = temp_mean[1,], end_temp_min = temp_min, end_temp_max = temp_max)
      bce_list[[i]] <- temp_values 
    }
  }
  bce_temp <- bind_rows(bce_list) 
  bce_temp <- bind_cols(bce, bce_temp)
  
  # Start BCE and end pre-1900CE
  bce_end_pre_1900_list <- list()
  for (i in 1:nrow(start_bce_end_pre_1900)){
    # Extract years
    years <- data.frame(name = temp_name) %>%
      mutate(year = gsub("X", "", name)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 1 & year <= start_bce_end_pre_1900$end_year[i])
    bce_years <- krapp_years %>%
      filter(year_ce >= start_bce_end_pre_1900$start_millennium[i] & year_ce <= start_bce_end_pre_1900$end_millennium[i])
    # Subset temperature by years
    temp_years <- subset(temp, years$name)
    mean_temp_years <- subset(mean_temp, bce_years$name)
    min_temp_years <- subset(min_temp, bce_years$name)
    max_temp_years <- subset(max_temp, bce_years$name)
    # Extract average, min and max temperature
    if(nrow(years) > 1) {
      ce_values <- exact_extract(temp_years, start_bce_end_pre_1900$geometry[i], fun = get_values_year)
    } else {
      ce_values <- exact_extract(temp_years, start_bce_end_pre_1900$geometry[i], fun = get_single_values) 
    }
    if(nrow(bce_years) > 1) {
    bce_min <- exact_extract(min_temp_years, start_bce_end_pre_1900$geometry[i], fun = "min") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "min.", values_to = "min")
    bce_max <- exact_extract(max_temp_years, start_bce_end_pre_1900$geometry[i], fun = "max") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "max.", values_to = "max")
    bce_mean <- exact_extract(mean_temp_years, start_bce_end_pre_1900$geometry[i], fun = get_single_mean) %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "values.", values_to = "mean") 
    bce_values <- bce_mean %>%
      left_join(bce_min, by = "year") %>%
      left_join(bce_max, by = "year")
    } else {
      bce_min <- exact_extract(min_temp_years, start_bce_end_pre_1900$geometry[i], fun = "min")
      bce_max <- exact_extract(max_temp_years, start_bce_end_pre_1900$geometry[i], fun = "max") 
      bce_mean <- exact_extract(mean_temp_years, start_bce_end_pre_1900$geometry[i], fun = get_single_mean)
      bce_values <- cbind(bce_mean, bce_min, bce_max) %>%
        rename(mean = values, min = bce_min, max = bce_max)
    }
    all_years <- bind_rows(bce_values, ce_values)
    start_temp_min <- bce_values$min[1]
    mid_temp_min <- bce_values$min[ceiling(nrow(bce_values)/2)]
    end_temp_min <- bce_values$min[nrow(bce_values)]
    oa_temp_min <- min(all_years$min, na.rm = TRUE)
    start_temp_max <- bce_values$max[1]
    mid_temp_max <- bce_values$max[ceiling(nrow(bce_values)/2)]
    end_temp_max <- bce_values$max[nrow(bce_values)]
    oa_temp_max <- max(all_years$max, na.rm = TRUE)
    start_temp_avg <- all_years$mean[1]
    mid_temp_avg <- all_years$mean[ceiling(nrow(bce_values)/2)]
    end_temp_avg <- all_years$mean[nrow(all_years)]
    oa_temp_avg <- mean(all_years$mean, na.rm = TRUE)
    bce_end_pre_1900_list[[i]] <- data.frame(oa_temp_avg = oa_temp_avg, oa_temp_min = oa_temp_min, oa_temp_max = oa_temp_max, start_temp_avg = start_temp_avg, start_temp_min = start_temp_min, start_temp_max = start_temp_max, mid_temp_avg = mid_temp_avg, mid_temp_min = mid_temp_min, mid_temp_max = mid_temp_max, end_temp_avg = end_temp_avg, end_temp_min = end_temp_min, end_temp_max = end_temp_max)
  }
  bce_end_pre_1900_temp <- bind_rows(bce_end_pre_1900_list) 
  start_bce_end_pre_1900_temp <- bind_cols(start_bce_end_pre_1900, bce_end_pre_1900_temp)
  
  # Start BCE and end post-1900CE
  bce_end_post_1900_list <- list()
  for (i in 1:nrow(start_bce_end_post_1900)){
    # Extract years
    years <- data.frame(name = temp_name) %>%
      mutate(year = gsub("X", "", name)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= 1 & year <= 1900)
    bce_years <- krapp_years %>%
      filter(year_ce >= start_bce_end_post_1900$start_millennium[i] & year_ce <= start_bce_end_post_1900$end_millennium[i])
    years_1901ce <- data.frame(name = temp_name_1901ce) %>%
      mutate(year = str_extract_all(name,"[[:digit:]]{4}")) %>%
      filter(year >= 1901 & year <= start_bce_end_post_1900$end_year_2020[i]) %>%
      mutate(year = as.character(year))
    # Subset temperature by years
    temp_years <- subset(temp, years$name)
    mean_temp_years <- subset(mean_temp, bce_years$name)
    min_temp_years <- subset(min_temp, bce_years$name)
    max_temp_years <- subset(max_temp, bce_years$name)
    temp_1901_years <- subset(temp_1901ce, years_1901ce$name)
    temp_min_1901_years <- subset(temp_min_1901ce, years_1901ce$name)
    temp_max_1901_years <- subset(temp_max_1901ce, years_1901ce$name)
    # Extract average, min and max temperature
    post_1900ce_min <- exact_extract(temp_min_1901_years, start_bce_end_post_1900$geometry[i], fun = "min") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "min.", values_to = "min")
    post_1900ce_max <- exact_extract(temp_max_1901_years, start_bce_end_post_1900$geometry[i], fun = "max") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "max.", values_to = "max")
    post_1900ce_mean <- exact_extract(temp_1901_years, start_bce_end_post_1900$geometry[i], fun = get_single_mean)%>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "values.", values_to = "mean") 
    post_1900ce_values <- post_1900ce_mean %>%
      left_join(post_1900ce_min, by = "year") %>%
      left_join(post_1900ce_max, by = "year") %>%
      rename(name = year) %>% 
      left_join(years_1901ce) %>% 
      group_by(year) %>% 
      summarize(mean = mean(mean), min = min(min), max = max(max))
    pre_1900ce_values <- exact_extract(temp_years, start_bce_end_post_1900$geometry[i], fun = get_values_year)
    if(nrow(bce_years) > 1) {
    bce_min <- exact_extract(min_temp_years, start_bce_end_post_1900$geometry[i], fun = "min") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "min.", values_to = "min")
    bce_max <- exact_extract(max_temp_years, start_bce_end_post_1900$geometry[i], fun = "max") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "max.", values_to = "max")
    bce_mean <- exact_extract(mean_temp_years, start_bce_end_post_1900$geometry[i], fun = get_single_mean) %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "values.", values_to = "mean") 
    bce_values <- bce_mean %>%
      left_join(bce_min, by = "year") %>%
      left_join(bce_max, by = "year")
    } else {
    bce_min <- exact_extract(min_temp_years, start_bce_end_post_1900$geometry[i], fun = "min")
    bce_max <- exact_extract(max_temp_years, start_bce_end_post_1900$geometry[i], fun = "max") 
    bce_mean <- exact_extract(mean_temp_years, start_bce_end_post_1900$geometry[i], fun = get_single_mean)
    bce_values <- cbind(bce_mean, bce_min, bce_max) %>%
      rename(mean = values, min = bce_min, max = bce_max)
    }
    min_max <- bind_rows(bce_values, post_1900ce_values)
    all_years <- bind_rows(bce_values, pre_1900ce_values, post_1900ce_values)
    start_temp_min <- min_max$min[1]
    mid_temp_min <- min_max$min[ceiling(nrow(min_max)/2)]
    end_temp_min <- min_max$min[nrow(min_max)]
    oa_temp_min <- min(all_years$min, na.rm = TRUE)
    start_temp_max <- min_max$max[1]
    mid_temp_max <- min_max$max[ceiling(nrow(min_max)/2)]
    end_temp_max <- min_max$max[nrow(min_max)]
    oa_temp_max <- max(all_years$max, na.rm = TRUE)
    start_temp_avg <- all_years$mean[1]
    mid_temp_avg <- all_years$mean[ceiling(nrow(all_years)/2)]
    end_temp_avg <- all_years$mean[nrow(all_years)]
    oa_temp_avg <- mean(all_years$mean, na.rm = TRUE)
    bce_end_post_1900_list[[i]] <- data.frame(oa_temp_avg = oa_temp_avg, oa_temp_min = oa_temp_min, oa_temp_max = oa_temp_max, start_temp_avg = start_temp_avg, start_temp_min = start_temp_min, start_temp_max = start_temp_max, mid_temp_avg = mid_temp_avg, mid_temp_min = mid_temp_min, mid_temp_max = mid_temp_max, end_temp_avg = end_temp_avg, end_temp_min = end_temp_min, end_temp_max = end_temp_max)
  }
  bce_end_post_1900_temp <- bind_rows(bce_end_post_1900_list) 
  start_bce_end_post_1900_temp <- bind_cols(start_bce_end_post_1900, bce_end_post_1900_temp) %>%
    dplyr:::select(-end_year_2020)
  
  # Start pre-1900CE and end pre-1900CE
  start_end_pre_1900_list <- list()
  for (i in 1:nrow(start_end_pre_1900)){
    # Extract years
    years <- data.frame(name = temp_name) %>%
      mutate(year = gsub("X", "", name)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= start_end_pre_1900$start_year[i] & year <= start_end_pre_1900$end_year[i]) %>%
      mutate(year = as.character(year))
    min_max_years <- krapp_years %>%
      filter(year_ce >= start_end_pre_1900$start_millennium[i] & year_ce <= start_end_pre_1900$end_millennium[i])
    # Subset temperature by years
    temp_years <- subset(temp, years$name)
    min_temp_years <- subset(min_temp, min_max_years$name)
    max_temp_years <- subset(max_temp, min_max_years$name)
    # Extract average, min and max temperature
    if(nrow(years) > 1) {
      temp_values <- exact_extract(temp_years, start_end_pre_1900$geometry[i], fun = get_values_year)
      start_temp_avg <- temp_values$mean[1]
      mid_temp_avg <- temp_values$mean[ceiling(nrow(temp_values)/2)]
      end_temp_avg <- temp_values$mean[nrow(temp_values)]
      oa_temp_avg <- mean(temp_values$mean, na.rm = TRUE)
      avg_temp <- data.frame(oa_temp_avg = oa_temp_avg, start_temp_avg = start_temp_avg, mid_temp_avg = mid_temp_avg, end_temp_avg = end_temp_avg)
    } else {
      temp_values <- exact_extract(temp_years, start_end_pre_1900$geometry[i], fun = get_single_values) 
      avg_temp <- data.frame(oa_temp_avg = temp_values$mean[1], start_temp_avg = temp_values$mean[1], mid_temp_avg = temp_values$mean[1], end_temp_avg = temp_values$mean[1])
    }
    if(nrow(min_max_years) > 1) {
      temp_min <- exact_extract(min_temp_years, start_end_pre_1900$geometry[i], fun = "min") %>%
        pivot_longer(everything(), names_to	= "year", names_prefix = "min.", values_to = "min")
      temp_max <- exact_extract(max_temp_years, start_end_pre_1900$geometry[i], fun = "max") %>%
        pivot_longer(everything(), names_to	= "year", names_prefix = "max.", values_to = "max")
      start_temp_min <- temp_min$min[1]
      mid_temp_min <- temp_min$min[ceiling(nrow(temp_min)/2)]
      end_temp_min <- temp_min$min[nrow(temp_min)]
      oa_temp_min <- min(temp_min$min, na.rm = TRUE)
      start_temp_max <- temp_max$max[1]
      mid_temp_max <- temp_max$max[ceiling(nrow(temp_max)/2)]
      end_temp_max <- temp_max$max[nrow(temp_max)]
      oa_temp_max <- max(temp_max$max, na.rm = TRUE)
      min_max <- data.frame(oa_temp_min = oa_temp_min, oa_temp_max = oa_temp_max, start_temp_min = start_temp_min, start_temp_max = start_temp_max, mid_temp_min = mid_temp_min, mid_temp_max = mid_temp_max, end_temp_min = end_temp_min, end_temp_max = end_temp_max)
    } else {
      temp_min <- exact_extract(min_temp_years, start_end_pre_1900$geometry[i], fun = "min")
      temp_max <- exact_extract(max_temp_years, start_end_pre_1900$geometry[i], fun = "max") 
      min_max <- data.frame(oa_temp_min = temp_min, oa_temp_max = temp_max, start_temp_min = temp_min, start_temp_max = temp_max, mid_temp_min = temp_min, mid_temp_max = temp_max, end_temp_min = temp_min, end_temp_max = temp_max)
    }
    temp_pre_1900ce <- bind_cols(avg_temp, min_max) %>%
      select(oa_temp_avg, oa_temp_min, oa_temp_max, start_temp_avg, start_temp_min, start_temp_max, mid_temp_avg, mid_temp_min, mid_temp_max, end_temp_avg, end_temp_min, end_temp_max)
    start_end_pre_1900_list[[i]] <- temp_pre_1900ce
  }
  start_end_pre_1900_temp <- bind_rows(start_end_pre_1900_list) 
  start_end_pre_1900_temp <- bind_cols(start_end_pre_1900, start_end_pre_1900_temp)
  
  # Start pre-1900CE and end post-1900CE
  start_pre_end_post_1900_list <- list()
  for (i in 1:nrow(start_pre_end_post_1900)){
    # Extract years
    years <- data.frame(name = temp_name) %>%
      mutate(year = gsub("X", "", name)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= start_pre_end_post_1900$start_year[i] & year <= 1900)
    years_1901ce <- data.frame(name = temp_name_1901ce) %>%
      mutate(year = str_extract_all(name,"[[:digit:]]{4}")) %>%
      filter(year >= 1901 & year <= start_pre_end_post_1900$end_year_2020[i]) %>%
      mutate(year = as.character(year))
    min_max_years <- krapp_years %>%
      filter(year_ce >= start_pre_end_post_1900$start_millennium[i] & year_ce <= start_pre_end_post_1900$end_millennium[i])
    # Subset temperature by years
    temp_years <- subset(temp, years$name)
    temp_1901_years <- subset(temp_1901ce, years_1901ce$name)
    temp_min_1901_years <- subset(temp_min_1901ce, years_1901ce$name)
    temp_max_1901_years <- subset(temp_max_1901ce, years_1901ce$name)
    min_temp_years <- subset(min_temp, min_max_years$name)
    max_temp_years <- subset(max_temp, min_max_years$name)
    # Extract average, min and max temperature
    post_1900ce_min <- exact_extract(temp_min_1901_years, start_pre_end_post_1900$geometry[i], fun = "min") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "min.", values_to = "min")
    post_1900ce_max <- exact_extract(temp_max_1901_years, start_pre_end_post_1900$geometry[i], fun = "max") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "max.", values_to = "max")
    post_1900ce_mean <- exact_extract(temp_1901_years, start_pre_end_post_1900$geometry[i], fun = get_single_mean)%>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "values.", values_to = "mean") 
    # Summarize mean per year
    post_1900ce_values <- post_1900ce_mean %>%
      left_join(post_1900ce_min, by = "year") %>%
      left_join(post_1900ce_max, by = "year") %>%
      rename(name = year) %>% 
      left_join(years_1901ce) %>% 
      group_by(year) %>% 
      summarize(mean = mean(mean), min = min(min), max = max(max))
    if(nrow(years) > 1) {
      pre_1900ce_values <- exact_extract(temp_years, start_pre_end_post_1900$geometry[i], fun = get_values_year)
    } else {
      pre_1900ce_values <- exact_extract(temp_years, start_pre_end_post_1900$geometry[i], fun = get_single_mean)
    }
    if(nrow(min_max_years) > 1) {
      temp_min <- exact_extract(min_temp_years, start_pre_end_post_1900$geometry[i], fun = "min") %>%
        pivot_longer(everything(), names_to	= "year", names_prefix = "min.", values_to = "min")
      temp_max <- exact_extract(max_temp_years, start_pre_end_post_1900$geometry[i], fun = "max") %>%
        pivot_longer(everything(), names_to	= "year", names_prefix = "max.", values_to = "max")
      all_years_min <- c(temp_min$min, post_1900ce_values$min)
      all_years_max <- c(temp_max$max, post_1900ce_values$max)
    } else {
      temp_min <- exact_extract(min_temp_years, start_pre_end_post_1900$geometry[i], fun = "min")
      temp_max <- exact_extract(max_temp_years, start_pre_end_post_1900$geometry[i], fun = "max")
      all_years_min <- c(temp_min, post_1900ce_values$min)
      all_years_max <- c(temp_max, post_1900ce_values$max)
    }
    all_years <- bind_rows(pre_1900ce_values, post_1900ce_values)
    start_temp_min <- all_years_min[1]
    mid_temp_min <- all_years_min[ceiling(length(all_years_min)/2)]
    end_temp_min <- all_years_min[length(all_years_min)]
    oa_temp_min <- min(all_years_min, na.rm = TRUE)
    start_temp_max <- all_years_max[1]
    mid_temp_max <- all_years_max[ceiling(length(all_years_max)/2)]
    end_temp_max <- all_years_max[length(all_years_max)]
    oa_temp_max <- max(all_years_max, na.rm = TRUE)
    start_temp_avg <- all_years$mean[1]
    mid_temp_avg <- all_years$mean[ceiling(nrow(all_years)/2)]
    end_temp_avg <- all_years$mean[nrow(all_years)]
    oa_temp_avg <- mean(all_years$mean, na.rm = TRUE)
    start_pre_end_post_1900_list[[i]] <- data.frame(oa_temp_avg = oa_temp_avg, oa_temp_min = oa_temp_min, oa_temp_max = oa_temp_max, start_temp_avg = start_temp_avg, start_temp_min = start_temp_min, start_temp_max = start_temp_max, mid_temp_avg = mid_temp_avg, mid_temp_min = mid_temp_min, mid_temp_max = mid_temp_max, end_temp_avg = end_temp_avg, end_temp_min = end_temp_min, end_temp_max = end_temp_max)
  }
  start_pre_end_post_1900_temp <- bind_rows(start_pre_end_post_1900_list) 
  start_pre_end_post_1900_temp <- bind_cols(start_pre_end_post_1900, start_pre_end_post_1900_temp) %>%
    dplyr:::select(-end_year_2020)
  
  # Start and End post-1900 CE
  start_end_post_1900_list <- list()
  for (i in 1:nrow(start_end_post_1900)){
    # Extract years
    years_1901ce <- data.frame(name = temp_name_1901ce) %>%
      mutate(year = str_extract_all(name,"[[:digit:]]{4}")) %>%
      filter(year >= start_end_post_1900$start_year_2020[i] & year <= start_end_post_1900$end_year_2020[i]) %>%
      mutate(year = as.character(year))
    # Subset temperature by years
    temp_1901_years <- subset(temp_1901ce, years_1901ce$name)
    temp_min_1901_years <- subset(temp_min_1901ce, years_1901ce$name)
    temp_max_1901_years <- subset(temp_max_1901ce, years_1901ce$name)
    # Extract average, min and max temperature
    post_1900ce_min <- exact_extract(temp_min_1901_years, start_end_post_1900$geometry[i], fun = "min") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "min.", values_to = "min")
    post_1900ce_max <- exact_extract(temp_max_1901_years, start_end_post_1900$geometry[i], fun = "max") %>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "max.", values_to = "max")
    post_1900ce_mean <- exact_extract(temp_1901_years, start_end_post_1900$geometry[i], fun = get_single_mean)%>%
      pivot_longer(everything(), names_to	= "year", names_prefix = "values.", values_to = "mean") 
    # Summarize mean per year
    temp_values <- post_1900ce_mean %>%
      left_join(post_1900ce_min, by = "year") %>%
      left_join(post_1900ce_max, by = "year") %>%
      rename(name = year) %>% 
      left_join(years_1901ce) %>% 
      group_by(year) %>% 
      summarize(mean = mean(mean), min = min(min), max = max(max))
    start_temp_avg <- temp_values$mean[1]
    mid_temp_avg <- temp_values$mean[ceiling(nrow(temp_values)/2)]
    end_temp_avg <- temp_values$mean[nrow(temp_values)]
    oa_temp_avg <- mean(temp_values$mean, na.rm = TRUE)
    start_temp_min <- temp_values$min[1]
    mid_temp_min <- temp_values$min[ceiling(nrow(temp_values)/2)]
    end_temp_min <- temp_values$min[nrow(temp_values)]
    oa_temp_min <- min(temp_values$min, na.rm = TRUE)
    start_temp_max <- temp_values$max[1]
    mid_temp_max <- temp_values$max[ceiling(nrow(temp_values)/2)]
    end_temp_max <- temp_values$max[nrow(temp_values)]
    oa_temp_max <- max(temp_values$max, na.rm = TRUE)
    start_end_post_1900_list[[i]]  <- data.frame(oa_temp_avg = oa_temp_avg, start_temp_avg = start_temp_avg, mid_temp_avg = mid_temp_avg, end_temp_avg = end_temp_avg, oa_temp_min = oa_temp_min, oa_temp_max = oa_temp_max, start_temp_min = start_temp_min, start_temp_max = start_temp_max, mid_temp_min = mid_temp_min, mid_temp_max = mid_temp_max, end_temp_min = end_temp_min, end_temp_max = end_temp_max)
  }
  start_end_post_1900_temp <- bind_rows(start_end_post_1900_list) 
  start_end_post_1900_temp <- bind_cols(start_end_post_1900, start_end_post_1900_temp) %>%
    dplyr:::select(-start_year_2020, -end_year_2020)
  
  # Combine temperature data for different periods
  data_temp <- bind_rows(bce_temp, start_bce_end_pre_1900_temp, start_bce_end_post_1900_temp, start_end_pre_1900_temp, start_pre_end_post_1900_temp, start_end_post_1900_temp)
  data_temp
}

# Get precipitation
get_precipitation <- function(data) {
  # Split into start date pre-1900CE & end pre-1900CE, start date pre-1900CE & end post-1900CE, and start and end date post-1900CE
  start_end_pre_1900 <- data %>%
    filter(end_year <= 1900) 
  start_pre_end_post_1900 <- data %>%
    filter(start_year <= 1900 & end_year > 1900) %>%
    mutate(end_year_2020 = ifelse(end_year > 2020, 2020, end_year)) 
  start_end_post_1900 <- data %>%
    filter(start_year > 1900) %>%
    mutate(end_year_2020 = ifelse(end_year > 2020, 2020, end_year)) %>%
    mutate(start_year_2020 = ifelse(start_year > 2020, 2020, start_year))
  
  # Extract raster layer names
  krapp_prep_name <- names(mean_prep)
  prep_name_1901ce <- names(prep_1901ce)
  
  # Extract years in overall dataset
  krapp_years <- data.frame(name = krapp_prep_name) %>%
    mutate(year = gsub("X\\.", "", name)) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(year = gsub("e.05", "00000", year)) %>%
    mutate(year = as.numeric(year)) %>%
    # Convert to BCE/CE (rounding to 0 = 2000, rather than 0 = 1950)
    mutate(year_ce = (year - 2000) * -1) %>%
    # Filter years not present in the overall dataset
    filter(year_ce >= min(data$start_year))
  
  # Subset precipitation by years
  mean_prep <- subset(mean_prep, krapp_years$name)
  min_prep <- subset(min_prep, krapp_years$name)
  max_prep <- subset(max_prep, krapp_years$name)
  
  # Find mean, min and max precipitation per time period
  # Start Date pre-1900CE & End Date pre-1900CE
  start_end_pre_1900_list <- list()
  for (i in 1:nrow(start_end_pre_1900)){
    # Extract years
    years <- krapp_years %>%
      filter(year_ce >= start_end_pre_1900$start_millennium[i] & year_ce <= start_end_pre_1900$end_millennium[i])
    # Subset precipitation by years
    mean_prep_years <- subset(mean_prep, years$name)
    min_prep_years <- subset(min_prep, years$name)
    max_prep_years <- subset(max_prep, years$name)
    # Extract average, min and max precipitation
    if(nrow(years) > 1) {
      prep_min <- exact_extract(min_prep_years, start_end_pre_1900$geometry[i], fun = "min")
      prep_max <- exact_extract(min_prep_years, start_end_pre_1900$geometry[i], fun = "max")
      prep_mean <- exact_extract(mean_prep_years, start_end_pre_1900$geometry[i], fun = get_single_mean)
      start_prep_min <- prep_min[,1]
      mid_prep_min <- prep_min[,ceiling(ncol(prep_min)/2)]
      end_prep_min <- prep_min[,ncol(prep_min)]
      oa_prep_min <- min(prep_min, na.rm = TRUE)
      start_prep_max <- prep_max[,1]
      mid_prep_max <- prep_max[,ceiling(ncol(prep_max)/2)]
      end_prep_max <- prep_max[,ncol(prep_max)]
      oa_prep_max <-  max(prep_max, na.rm = TRUE)
      start_prep_avg <- prep_mean[,1]
      mid_prep_avg <- prep_mean[,ceiling(ncol(prep_mean)/2)]
      end_prep_avg <- prep_mean[,ncol(prep_mean)]
      prep_values <- prep_mean %>%
        # Find mean over years
        mutate(oa_prep_avg = rowMeans(.)) %>%
        dplyr::select(oa_prep_avg) %>%
        mutate(oa_prep_min = oa_prep_min, oa_prep_max = oa_prep_max, start_prep_avg = start_prep_avg, start_prep_min = start_prep_min, start_prep_max = start_prep_max, mid_prep_avg = mid_prep_avg, mid_prep_min = mid_prep_min, mid_prep_max = mid_prep_max, end_prep_avg = end_prep_avg, end_prep_min = end_prep_min, end_prep_max = end_prep_max)
      start_end_pre_1900_list[[i]] <- prep_values
    } else {
      prep_min <- exact_extract(min_prep_years, start_end_pre_1900$geometry[i], fun = "min")
      prep_max <- exact_extract(max_prep_years, start_end_pre_1900$geometry[i], fun = "max")
      prep_mean <- exact_extract(mean_prep_years, start_end_pre_1900$geometry[i], fun = get_single_mean)
      prep_values <- data.frame(oa_prep_avg = prep_mean[1,], oa_prep_min = prep_min, oa_prep_max = prep_max, start_prep_avg = prep_mean[1,], start_prep_min = prep_min, start_prep_max = prep_max, mid_prep_avg = prep_mean[1,], mid_prep_min = prep_min, mid_prep_max = prep_max, end_prep_avg = prep_mean[1,], end_prep_min = prep_min, end_prep_max = prep_max)
      start_end_pre_1900_list[[i]] <- prep_values 
    }
  }
  start_end_pre_1900_prep_df <- bind_rows(start_end_pre_1900_list) 
  start_end_pre_1900_prep <- bind_cols(start_end_pre_1900, start_end_pre_1900_prep_df)
  
  # Start Date pre-1900CE & End Date post-1900CE
  start_pre_end_post_1900_list <- list()
  for (i in 1:nrow(start_pre_end_post_1900)){
    # Extract years
    years <- krapp_years %>%
      filter(year_ce >= start_pre_end_post_1900$start_millennium[i] & year_ce <= start_pre_end_post_1900$end_millennium[i])
    years_1901ce <- data.frame(name = prep_name_1901ce) %>%
      mutate(year = str_extract_all(name,"[[:digit:]]{4}")) %>%
      filter(year >= 1901 & year <= start_pre_end_post_1900$end_year_2020[i]) %>%
      mutate(year = as.character(year))
    # Subset precipitation by years
    mean_prep_years <- subset(mean_prep, years$name)
    min_prep_years <- subset(min_prep, years$name)
    max_prep_years <- subset(max_prep, years$name)
    prep_1901_years <- subset(prep_1901ce, years_1901ce$name)
    # Extract average, min and max precipitation
    if(nrow(years) > 1) {
      prep_min <- exact_extract(min_prep_years, start_pre_end_post_1900$geometry[i], fun = "min") %>%
        pivot_longer(everything(), names_to	= "year", names_prefix = "min.", values_to = "min")
      prep_max <- exact_extract(min_prep_years, start_pre_end_post_1900$geometry[i], fun = "max") %>%
        pivot_longer(everything(), names_to	= "year", names_prefix = "max.", values_to = "max")
      prep_mean <- exact_extract(mean_prep_years, start_pre_end_post_1900$geometry[i], fun = get_single_mean) %>%
        pivot_longer(everything(), names_to	= "year", names_prefix = "values.", values_to = "mean") 
      pre_1900ce <- prep_mean %>%
        left_join(prep_min, by = "year") %>%
        left_join(prep_max, by = "year")
    } else {
      prep_min <- exact_extract(min_prep_years, start_pre_end_post_1900$geometry[i], fun = "min")
      prep_max <- exact_extract(min_prep_years, start_pre_end_post_1900$geometry[i], fun = "max")
      prep_mean <- exact_extract(mean_prep_years, start_pre_end_post_1900$geometry[i], fun = get_single_mean)
      pre_1900ce <- data.frame(year = "x", mean = prep_mean, min = prep_min, max = prep_max)
    }
    post_1900ce <- exact_extract(prep_1901_years, start_pre_end_post_1900$geometry[i], fun = get_values_year)
    post_1900ce <- post_1900ce %>%
      rename(name = year) %>% 
      left_join(years_1901ce) %>% 
      group_by(year) %>% 
      summarize(mean = mean(mean), min = min(min), max = max(max)) %>%
      mutate(year = as.character(year))
    all_years <- bind_rows(pre_1900ce, post_1900ce)
    start_prep_min <- all_years$min[1]
    mid_prep_min <-  all_years$min[ceiling(length( all_years)/2)]
    end_prep_min <-  all_years$min[length( all_years)]
    oa_prep_min <- min( all_years$min, na.rm = TRUE)
    start_prep_max <- all_years$max[1]
    mid_prep_max <- all_years$max[ceiling(length(all_years)/2)]
    end_prep_max <- all_years$max[length(all_years)]
    oa_prep_max <- max(all_years$max, na.rm = TRUE)
    start_prep_avg <- all_years$mean[1]
    mid_prep_avg <- all_years$mean[ceiling(nrow(all_years)/2)]
    end_prep_avg <- all_years$mean[nrow(all_years)]
    oa_prep_avg <- mean(all_years$mean, na.rm = TRUE)
    start_pre_end_post_1900_list[[i]] <- data.frame(oa_prep_avg = oa_prep_avg, oa_prep_min = oa_prep_min, oa_prep_max = oa_prep_max, start_prep_avg = start_prep_avg, start_prep_min = start_prep_min, start_prep_max = start_prep_max, mid_prep_avg = mid_prep_avg, mid_prep_min = mid_prep_min, mid_prep_max = mid_prep_max, end_prep_avg = end_prep_avg, end_prep_min = end_prep_min, end_prep_max = end_prep_max)
  }
  start_pre_end_post_1900_prep_df <- bind_rows(start_pre_end_post_1900_list) 
  start_pre_end_post_1900_prep <- bind_cols(start_pre_end_post_1900, start_pre_end_post_1900_prep_df) %>%
    dplyr:::select(-end_year_2020)
  
  # Start Date post-1900CE & End Date post-1900CE
  start_end_post_1900_list <- list()
  for (i in 1:nrow(start_end_post_1900)){
    # Extract years
    years_1901ce <- data.frame(name = prep_name_1901ce) %>%
      mutate(year = str_extract_all(name,"[[:digit:]]{4}")) %>%
      filter(year >= start_end_post_1900$start_year_2020[i] & year <= start_end_post_1900$end_year_2020[i]) %>%
      mutate(year = as.character(year))
    # Subset precipitation by years
    prep_1901_years <- subset(prep_1901ce, years_1901ce$name)
    # Extract average, min and max precipitation
    post_1900ce <- exact_extract(prep_1901_years, start_end_post_1900$geometry[i], fun = get_values_year)
    prep_value <- post_1900ce %>%
      rename(name = year) %>% 
      left_join(years_1901ce) %>% 
      group_by(year) %>% 
      summarize(mean = mean(mean), min = min(min), max = max(max)) %>%
      mutate(year = as.character(year))
    start_prep_min <- prep_value$min[1]
    mid_prep_min <-  prep_value$min[ceiling(length(prep_value)/2)]
    end_prep_min <-  prep_value$min[length(prep_value)]
    oa_prep_min <- min(prep_value$min, na.rm = TRUE)
    start_prep_max <- prep_value$max[1]
    mid_prep_max <- prep_value$max[ceiling(length(prep_value)/2)]
    end_prep_max <- prep_value$max[length(prep_value)]
    oa_prep_max <- max(prep_value$max, na.rm = TRUE)
    start_prep_avg <- prep_value$mean[1]
    mid_prep_avg <- prep_value$mean[ceiling(nrow(prep_value)/2)]
    end_prep_avg <- prep_value$mean[nrow(prep_value)]
    oa_prep_avg <- mean(prep_value$mean, na.rm = TRUE)
    start_end_post_1900_list[[i]] <- data.frame(oa_prep_avg = oa_prep_avg, oa_prep_min = oa_prep_min, oa_prep_max = oa_prep_max, start_prep_avg = start_prep_avg, start_prep_min = start_prep_min, start_prep_max = start_prep_max, mid_prep_avg = mid_prep_avg, mid_prep_min = mid_prep_min, mid_prep_max = mid_prep_max, end_prep_avg = end_prep_avg, end_prep_min = end_prep_min, end_prep_max = end_prep_max)
  }
  start_end_post_1900_prep_df <- bind_rows(start_end_post_1900_list) 
  start_end_post_1900_prep <- bind_cols(start_end_post_1900, start_end_post_1900_prep_df) %>%
    dplyr:::select(-end_year_2020, -start_year_2020) 
  
  # Combine temperature data for different periods
  data_prep <- bind_rows(start_end_pre_1900_prep, start_pre_end_post_1900_prep, start_end_post_1900_prep)
  data_prep
}

# Get SPEI
get_spei <- function(data) {
  # Split into start date BCE, start date pre-1900CE & end pre-1900CE, start date pre-1900CE & end post-1900CE, and start and end date post-1900CE
  bce <- data %>%
    filter(start_year < 0) 
  start_end_pre_1900 <- data %>%
    filter(start_year > 0 & end_year > 0 & end_year <= 1900)
  start_pre_end_post_1900 <- data %>%
    filter(start_year <= 1900 & end_year > 0 & end_year > 1900) %>%
    mutate(end_year_2018 = ifelse(end_year > 2018, 2018, end_year))
  start_end_post_1900 <- data %>%
    filter(start_year > 1900) %>%
    mutate(end_year_2018 = ifelse(end_year > 2018, 2018, end_year)) %>%
    mutate(start_year_2018 = ifelse(start_year > 2018, 2018, start_year))
  
  # Extract raster layer names
  spei_name <- names(spei)
  spei_name_1901ce <- names(spei_1901ce)
  
  # Start pre-1900CE and end 1900CE
  start_end_pre_1900_list <- list()
  for (i in 1:nrow(start_end_pre_1900)){
    # Extract years
    years <- data.frame(name = spei_name) %>%
      mutate(year = gsub("X", "", name)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= start_end_pre_1900$start_year[i] & year <= start_end_pre_1900$end_year[i])
    # Subset SPEI by years
    spei_years <- subset(spei, years$name)
    # Extract SPEI
    if(nrow(years) > 1) {
      values <- exact_extract(spei_years, start_end_pre_1900$geometry[i], fun = get_single_mean)
      start_spei <- values[,1]
      mid_spei <- values[,round(ncol(values)/2)]
      end_spei <- values[,ncol(values)]
      spei_values <- values %>%
        # Find mean over years
        mutate(avg_spei = rowMeans(.)) %>%
        dplyr::select(avg_spei) %>%
        mutate(start_spei = start_spei, mid_spei = mid_spei, end_spei = end_spei)
      start_end_pre_1900_list[[i]] <- spei_values 
    } else {
      spei_values <- exact_extract(spei_years, start_end_pre_1900$geometry[i], fun = get_single_mean) 
      start_end_pre_1900_list[[i]] <- data.frame(avg_spei = spei_values[1,], start_spei = spei_values[1,], mid_spei = spei_values[1,], end_spei = spei_values[1,])
    }
  }
  start_end_pre_1900_spei <- bind_rows(start_end_pre_1900_list) 
  start_end_pre_1900_spei <- bind_cols(start_end_pre_1900, start_end_pre_1900_spei)
  
  # Start pre-1900CE and end post-1900CE
  start_pre_end_post_1900_list <- list()
  for (i in 1:nrow(start_pre_end_post_1900)){
    # Extract years
    years <- data.frame(name = spei_name) %>%
      mutate(year = gsub("X", "", name)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= start_pre_end_post_1900$start_year[i] & year <= 1900)
    years_1901ce <- data.frame(name = spei_name_1901ce) %>%
      mutate(year = str_extract_all(name,"[[:digit:]]{4}")) %>%
      filter(year >= 1901 & year <= start_pre_end_post_1900$end_year_2018[i]) %>%
      mutate(year = as.character(year))
    # Subset SPEI by years
    spei_years <- subset(spei, years$name)
    spei_1901_years <- subset(spei_1901ce, years_1901ce$name)
    # Extract SPEI
    if(nrow(years) > 1) {
      pre_1900ce <- exact_extract(spei_years, start_pre_end_post_1900$geometry[i], fun = get_values_year)
    } else {
      pre_1900ce <- exact_extract(spei_years, start_pre_end_post_1900$geometry[i], fun = get_single_mean) 
    }
    post_1900ce <- exact_extract(spei_1901_years, start_pre_end_post_1900$geometry[i], fun = get_values_year)
    post_1900ce <- post_1900ce %>%
      rename(name = year) %>% 
      left_join(years_1901ce) %>% 
      group_by(year) %>% 
      summarize(mean = mean(mean), min = min(min), max = max(max)) %>%
      select(mean)
    all_years <- bind_rows(pre_1900ce, post_1900ce)
    start_spei <- all_years$mean[1]
    mid_spei <- all_years$mean[ceiling(nrow(all_years)/2)]
    end_spei <- all_years$mean[nrow(all_years)]
    avg_spei <- mean(all_years$mean, na.rm = TRUE)
    start_pre_end_post_1900_list[[i]] <- data.frame(avg_spei = avg_spei, start_spei = start_spei, mid_spei = mid_spei, end_spei = end_spei)
  }
  start_pre_end_post_1900_spei <- bind_rows(start_pre_end_post_1900_list) 
  start_pre_end_post_1900_spei <- bind_cols(start_pre_end_post_1900, start_pre_end_post_1900_spei) %>%
    dplyr:::select(-end_year_2018)
  
  # Start Date post-1900CE & End Date post-1900CE
  start_end_post_1900_list <- list()
  for (i in 1:nrow(start_end_post_1900)){
    # Extract years
    years_1901ce <- data.frame(name = spei_name_1901ce) %>%
      mutate(year = str_extract_all(name,"[[:digit:]]{4}")) %>%
      filter(year >= start_end_post_1900$start_year_2018[i] & year <= start_end_post_1900$end_year_2018[i]) %>%
      mutate(year = as.character(year))
    # Subset SPEI by years
    spei_1901_years <- subset(spei_1901ce, years_1901ce$name)
    # Extract SPEI
    post_1900ce <- exact_extract(spei_1901_years, start_end_post_1900$geometry[i], fun = get_values_year)
    spei_values <- post_1900ce %>%
      rename(name = year) %>% 
      left_join(years_1901ce) %>% 
      group_by(year) %>% 
      summarize(mean = mean(mean), min = min(min), max = max(max))
    start_spei <- spei_values$mean[1]
    mid_spei <- spei_values$mean[ceiling(nrow(spei_values)/2)]
    end_spei <- spei_values$mean[nrow(spei_values)]
    avg_spei <- mean(spei_values$mean, na.rm = TRUE)
    start_end_post_1900_list[[i]] <- data.frame(avg_spei = avg_spei, start_spei = start_spei, mid_spei = mid_spei, end_spei = end_spei)
  }
  start_end_post_1900_spei <- bind_rows(start_end_post_1900_list) 
  start_end_post_1900_spei <- bind_cols(start_end_post_1900, start_end_post_1900_spei) %>%
    dplyr:::select(-end_year_2018, -start_year_2018)
  
  # Add empty SPEI variable for BCE data
  bce_spei <- bce %>%
    mutate(avg_spei = NA, start_spei = NA, mid_spei = NA, end_spei = NA)
  
  # Combine temperature data for different periods
  data_spei <- bind_rows(bce_spei, start_end_pre_1900_spei, start_pre_end_post_1900_spei, start_end_post_1900_spei)
  data_spei
}

# Get PDSI
get_pdsi <- function(data) {
  # Split into start date BCE, start date pre-1870CE & end pre-1870CE, start date pre-1870CE & end post-1870CE, and start and end date post-1870CE
  bce <- data %>%
    filter(start_year < 0) 
  start_end_pre_1870 <- data %>%
    filter(start_year > 0 & end_year > 0 & end_year <= 1870)
  start_pre_end_post_1870 <- data %>%
    filter(start_year <= 1870 & end_year > 0 & end_year > 1870) %>%
    mutate(end_year_2005 = ifelse(end_year > 2005, 2005, end_year))
  start_end_post_1870 <- data %>%
    filter(start_year > 1870) %>%
    mutate(end_year_2005 = ifelse(end_year > 2005, 2005, end_year)) %>%
    mutate(start_year_2005 = ifelse(start_year > 2005, 2005, start_year))
  
  # Extract raster layer names
  pdsi_name <- names(pdsi)
  pdsi_name_1870ce <- names(pdsi_1870ce)
  
  # Start pre-1870CE and end 1870CE
  start_end_pre_1870_list <- list()
  for (i in 1:nrow(start_end_pre_1870)){
    # Extract years
    years <- data.frame(name = pdsi_name) %>%
      mutate(year = gsub("X", "", name)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= start_end_pre_1870$start_year[i] & year <= start_end_pre_1870$end_year[i])
    # Subset PDSI by years
    pdsi_years <- subset(pdsi, years$name)
    # Extract PDSI
    if(nrow(years) > 1) {
      values <- exact_extract(pdsi_years, start_end_pre_1870$geometry[i], fun = get_single_mean)
      start_pdsi <- values[,1]
      mid_pdsi <- values[,ceiling(ncol(values)/2)]
      end_pdsi <- values[,ncol(values)]
      pdsi_values <- values %>%
        # Find mean over years
        mutate(avg_pdsi = rowMeans(.)) %>%
        dplyr::select(avg_pdsi) %>%
        mutate(start_pdsi = start_pdsi, mid_pdsi = mid_pdsi, end_pdsi = end_pdsi)
      start_end_pre_1870_list[[i]] <- pdsi_values 
    } else {
      pdsi_values <- exact_extract(pdsi_years, start_end_pre_1870$geometry[i], fun = get_single_mean) 
      start_end_pre_1870_list[[i]] <- data.frame(avg_pdsi = pdsi_values[1,], start_pdsi = pdsi_values[1,], mid_pdsi = pdsi_values[1,], end_pdsi = pdsi_values[1,])
    }
  }
  start_end_pre_1870_pdsi <- bind_rows(start_end_pre_1870_list)
  start_end_pre_1870_pdsi <- bind_cols(start_end_pre_1870, start_end_pre_1870_pdsi)
  
  # Start pre-1870CE and end post-1870CE
  start_pre_end_post_1870_list <- list()
  for (i in 1:nrow(start_pre_end_post_1870)){
    # Extract years
    years <- data.frame(name = pdsi_name) %>%
      mutate(year = gsub("X", "", name)) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year >= start_pre_end_post_1870$start_year[i] & year <= 1870)
    years_1870ce <- data.frame(name = pdsi_name_1870ce) %>%
      mutate(year = str_extract_all(name,"[[:digit:]]{4}")) %>%
      filter(year >= 1871 & year <= start_pre_end_post_1870$end_year_2005[i]) %>%
      mutate(year = as.character(year))
    # Subset PDSI by years
    pdsi_years <- subset(pdsi, years$name)
    pdsi_1870_years <- subset(pdsi_1870ce, years_1870ce$name)
    # Extract PDSI
    if(nrow(years) > 1) {
      pre_1870ce <- exact_extract(pdsi_years, start_pre_end_post_1870$geometry[i], fun = get_values_year)
    } else {
      pre_1870ce <- exact_extract(pdsi_years, start_pre_end_post_1870$geometry[i], fun = get_single_mean) 
    }
    post_1870ce <- exact_extract(pdsi_1870_years, start_pre_end_post_1870$geometry[i], fun = get_values_year)
    post_1870ce <- post_1870ce %>%
      rename(name = year) %>% 
      left_join(years_1870ce) %>% 
      group_by(year) %>% 
      summarize(mean = mean(mean), min = min(min), max = max(max))
    all_years <- bind_rows(pre_1870ce, post_1870ce)
    start_pdsi <- all_years$mean[1]
    mid_pdsi <- all_years$mean[ceiling(nrow(all_years)/2)]
    end_pdsi <- all_years$mean[nrow(all_years)]
    avg_pdsi <- mean(all_years$mean, na.rm = TRUE)
    start_pre_end_post_1870_list[[i]] <- data.frame(avg_pdsi = avg_pdsi, start_pdsi = start_pdsi, mid_pdsi = mid_pdsi, end_pdsi = end_pdsi)
  }
  start_pre_end_post_1870_pdsi <- bind_rows(start_pre_end_post_1870_list) 
  start_pre_end_post_1870_pdsi <- bind_cols(start_pre_end_post_1870, start_pre_end_post_1870_pdsi) %>%
    dplyr:::select(-end_year_2005)
  
  # Start Date post-1870CE & End Date post-1870CE
  start_end_post_1870_list <- list()
  for (i in 1:nrow(start_end_post_1870)){
    # Extract years
    years_1870ce <- data.frame(name = pdsi_name_1870ce) %>%
      mutate(year = str_extract_all(name,"[[:digit:]]{4}")) %>%
      filter(year >= start_end_post_1870$start_year_2005[i] & year <= start_end_post_1870$end_year_2005[i]) %>%
      mutate(year = as.character(year))
    # Subset PDSI by years
    pdsi_1870_years <- subset(pdsi_1870ce, years_1870ce$name)
    # Extract PDSI
    post_1870ce <- exact_extract(pdsi_1870_years, start_end_post_1870$geometry[i], fun = get_values_year)
    pdsi_values <- post_1870ce %>%
      rename(name = year) %>% 
      left_join(years_1870ce) %>% 
      group_by(year) %>% 
      summarize(mean = mean(mean), min = min(min), max = max(max))
    start_pdsi <- pdsi_values$mean[1]
    mid_pdsi <- pdsi_values$mean[ceiling(nrow(pdsi_values)/2)]
    end_pdsi <- pdsi_values$mean[nrow(pdsi_values)]
    avg_pdsi <- mean(pdsi_values$mean, na.rm = TRUE)
    start_end_post_1870_list[[i]] <- data.frame(avg_pdsi = avg_pdsi, start_pdsi = start_pdsi, mid_pdsi = mid_pdsi, end_pdsi = end_pdsi)
  }
  start_end_post_1870_pdsi <- bind_rows(start_end_post_1870_list) 
  start_end_post_1870_pdsi <- bind_cols(start_end_post_1870, start_end_post_1870_pdsi) %>%
    dplyr:::select(-end_year_2005, -start_year_2005)
  
  # Add empty PDSI variable for BCE data
  bce_pdsi <- bce %>%
    mutate(avg_pdsi = NA, start_pdsi = NA, mid_pdsi = NA, end_pdsi = NA)
  
  # Combine temperature data for different periods
  data_pdsi <- bind_rows(bce_pdsi, start_end_pre_1870_pdsi, start_pre_end_post_1870_pdsi, start_end_post_1870_pdsi)
  data_pdsi
}

# Get elevation data
get_elevation <- function(data) {
  elevation_list <- list()
  for (i in 1:nrow(data)){
    elevation_list[[i]] <- exact_extract(elev_data, data$geometry[i], fun = get_single_mean) 
  }
  elevation_df <- bind_rows(elevation_list) 
  data_elevation <- bind_cols(data, elevation_df) %>%
    rename(elevation = values)
  data_elevation
}

# Extract valid DRH geometries which can by processed using S2
valid_region <- function(data) {
  regions_valid <- data %>%
    filter(`Entry ID` != 310 & `Entry ID` != 491 & `Entry ID` != 675 & `Entry ID` != 683 & `Entry ID` != 967 & `Entry ID` != 989 & `Entry ID` != 1279)
}

# Extract invalid DRH geometries which cannot by processed using S2
# 310 - Aztec Imperial Core
# 491 - Anglican Church
# 675 - Chukchee
# 683 - The Ppo Romé temple-tower complex (Bimong Kalan Ppo Romé, Tháp Ppo Romé): Ppo Romé Temple-Tower Devotees
# 967 - Unitarian Universalism
# 989 - Opus Dei (66 Countries)
# 1279 - Mandarese Muslims
invalid_region <- function(data) {
  regions_valid <- data %>%
    filter(`Entry ID` == 310 | `Entry ID` == 491 | `Entry ID` == 675 | `Entry ID` == 683 | `Entry ID` == 967 | `Entry ID` == 989 | `Entry ID` == 1279)
}

# Extract distance to water
distance_water <- function(data, water) {
  # Create an index of the nearest feature
  index <- st_nearest_feature(x = data, y = water)
  # Slice water dataset based on the index
  overlaps <- water %>% slice(index)
  # calculate distance between polygons
  dist_water <- st_distance(x = data, y = overlaps, by_element = TRUE)
}

# Get distance to water
get_distance_water <- function(data) {
  # Extract valid and invalid regions
  regions_valid <- valid_region(data)
  regions_invalid <- invalid_region(data)
  
  # For valid regions use spherical geometries
  regions_valid$dist_coastline <- distance_water(regions_valid, coastline)
  # Add distance to lakes
  regions_valid$dist_lakes <- distance_water(regions_valid, lakes)
  # Add distance to rivers
  regions_valid$dist_rivers <- distance_water(regions_valid, rivers)
  
  # For invalid regions use planar intersects 
  sf_use_s2(FALSE)
  # Add distance to coastline
  regions_invalid$dist_coastline <- distance_water(regions_invalid, coastline)
  # Add distance to lakes
  regions_invalid$dist_lakes <- distance_water(regions_invalid, lakes)
  # Add distance to rivers
  regions_invalid$dist_rivers <- distance_water(regions_invalid, rivers)
  # Use s2
  sf_use_s2(TRUE)
  # Region data and add shortest distance to water
  data <- bind_rows(regions_valid, regions_invalid) %>%
    rowwise() %>%
    mutate(dist_water = min(dist_coastline, dist_lakes, dist_rivers))
  data
}

# Get mammal data
get_mammals <- function(data) {
  mammal_list <- list()
  for (i in 1:nrow(data)){
    mammal_list[[i]] <- exact_extract(mammal_data, data$geometry[i], fun = get_single_mean) 
  }
  mammals_df <- bind_rows(mammal_list) 
  data_mammals <- bind_cols(data, mammals_df) %>%
    rename(mammals = values)
  data_mammals
}

# Get plant data
get_plants <- function(data){

  # Extract plant variables
  plants <- plant_data %>%
    dplyr::select(L8_ID, ASR, geometry)
  
  # Join data
  plants_join <- st_join(data, plants)
  
  # Find overlaps 
  plant_overlaps <- st_intersection(plants, plants_join)
  
  # Convert ID to factor for splitting
  id_factor <- plant_overlaps %>%
    mutate(Entry.ID = as.factor(Entry.ID))
  
  # Split overlap data
  overlaps_split <- split(id_factor, id_factor$Entry.ID)
  
  # Calculate weighted mean plant richness
  plant_list <- lapply(overlaps_split, function(x) data.frame(plants = weighted.mean(x$ASR, as.numeric(st_area(x)))))
  
  # Extract data
  plant_richness <- bind_rows(plant_list) %>%
    mutate(`Entry ID` = names(plant_list)) %>%
    mutate(`Entry ID` = as.numeric(`Entry ID`))
  
  # Join with plant data
  data_plants <- left_join(data, plant_richness)
}

# Get biome data
get_biomes <- function(data) {
  # Find sample overlaps
  biome_overlaps <- st_intersects(data, biomes)
  
  # Set region names
  names(biome_overlaps) <- data$`Region name`
  
  # Extract regions with length > 0
  biome_overlaps <- biome_overlaps[lengths(biome_overlaps) > 0L]
  
  # Convert to list of strings
  biome_overlaps_str <- lapply(biome_overlaps, function(x) toString(x))
  
  # Convert to tibble and split each sampled location into rows
  biome_overlaps_tib <- enframe(biome_overlaps_str, name = "Region name", value = "bio_id") %>%
    mutate(bio_id = as.character(bio_id)) %>%
    mutate(bio_id = strsplit(bio_id, ","))
  biome_overlaps_dt <- as.data.table(biome_overlaps_tib)
  biome_overlaps_dt <- biome_overlaps_dt[, list(bio_id= as.character(unlist(bio_id))), by = setdiff(names(biome_overlaps_dt), "bio_id")][
    , bio_id := as.numeric(bio_id)]
  
  # Add ID to biomes
  biomes_id <- as_tibble(biomes) %>%
    mutate(bio_id = row_number()) %>%
    select(bio_id,BIOME_NAME) %>%
    rename(biome = BIOME_NAME)
  
  # Join regions with biome
  biome_region <- biome_overlaps_dt %>%
    left_join(data) %>%
    left_join(biomes_id) %>%
    select(-bio_id) %>%
    distinct()
  
  # Remove geometry and combine multiple biomes into strings
  data_biome <- biome_region %>%
    select(-geometry) %>%
    distinct() %>%
    group_by(`Entry ID`, start_year, end_year, `Region ID`, `Region name`) %>%
    mutate(biome = paste(biome, collapse = ";")) %>%
    ungroup() %>%
    distinct() %>%
    left_join(data)
}

# Extract sample size per analysis condition
analysis_sample_size <- function() {
  # Analysis 1
  analysis_1_samples <- data %>% 
    group_by(`Entry name`, `Entry ID`, `Region name`, `Region ID`) %>% 
    tally()
  sample_size_1 <- nrow(analysis_1_samples)
  # Analysis 2
  analysis_2_samples <- analysis_2_dict %>% 
    group_by(`Entry name`, `Entry ID`, `Region name`, `Region ID`) %>% 
    tally()
  sample_size_2 <- nrow(analysis_2_samples)
  # Analysis 3
  analysis_3_samples <- analysis_3_dict %>% 
    group_by(`Entry name`, `Entry ID`, `Region name`, `Region ID`) %>% 
    tally()
  sample_size_3 <- nrow(analysis_3_samples)
  # Analysis 4
  analysis_4_samples <- analysis_4_dict %>% 
    group_by(`Entry name`, `Entry ID`, `Region name`, `Region ID`) %>% 
    tally()
  sample_size_4 <- nrow(analysis_4_samples)
  # Create output table
  sample_size <- tibble(Analysis = c(1, 2, 3, 4), `Sample Size` = c(sample_size_1, sample_size_2, sample_size_3, sample_size_4))
}

# Run models, first running vif
run_models <- function(data, var) {
  a1_data <- data %>%
    rename(var = all_of(var)) %>%
    mutate(var = as.numeric(var)) %>%
    filter(!is.na(var)) %>%
    # If the same region appears multiple times a random offset between 0.01 and 0.05
    group_by(latitude, longitude) %>%
    mutate(lat2 = ifelse(latitude == lead(latitude), latitude + runif(n = 1, min=0.5, max=1), NA)) %>%
    mutate(lon2 = ifelse(longitude == lead(longitude), longitude + runif(n = 1, min=0.5, max=1), NA)) %>%
    mutate(lat3 = ifelse(lat2 == lead(lat2), lat2 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lon3 = ifelse(lon2 == lead(lon2), lon2 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lat4 = ifelse(lat3 == lead(lat3), lat3 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lon4 = ifelse(lon3 == lead(lon3), lon3 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lat5 = ifelse(lat4 == lead(lat4), lat4 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lon5 = ifelse(lon4 == lead(lon4), lon4 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lat6 = ifelse(lat5 == lead(lat5), lat5 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lon6 = ifelse(lon5 == lead(lon5), lon5 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lat7 = ifelse(lat6 == lead(lat6), lat5 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lon7 = ifelse(lon6 == lead(lon6), lon5 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lat8 = ifelse(lat7 == lead(lat7), lat5 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lon8 = ifelse(lon7 == lead(lon7), lon5 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lat9 = ifelse(lat8 == lead(lat8), lat5 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lon9 = ifelse(lon8 == lead(lon8), lon5 + runif(n = 1, min=0.01, max=0.05), NA)) %>% 
    mutate(lat10 = ifelse(lat9 == lead(lat9), lat5 + runif(n = 1, min=0.01, max=0.05), NA)) %>%
    mutate(lon10 = ifelse(lon9 == lead(lon9), lon5 + runif(n = 1, min=0.01, max=0.05), NA)) %>%  
    mutate(latitude = ifelse(!is.na(lat10), lat10,
                             ifelse(!is.na(lat9), lat9,
                                    ifelse(!is.na(lat8), lat8,
                                           ifelse(!is.na(lat7), lat7,
                                                  ifelse(!is.na(lat6), lat6, 
                                                         ifelse(!is.na(lat5), lat5, 
                                                                ifelse(!is.na(lat4), lat4,
                                                                       ifelse(!is.na(lat3), lat3, 
                                                                              ifelse(!is.na(lat2), lat2, latitude)))))))))) %>%
    mutate(longitude = ifelse(!is.na(lon10), lon10, 
                              ifelse(!is.na(lon9), lon9, 
                                     ifelse(!is.na(lon8), lon8, 
                                            ifelse(!is.na(lon7), lon7, 
                                                   ifelse(!is.na(lon6), lon6, 
                                                          ifelse(!is.na(lon5), lon5, 
                                                                 ifelse(!is.na(lon4), lon4,
                                                                        ifelse(!is.na(lon3), lon3, 
                                                                               ifelse(!is.na(lon2), lon2, longitude)))))))))) %>%
    select(-lat2, -lat3, -lat4, -lat5, -lat6, -lon2, -lon3, -lon4, -lon5, -lon6) %>%
    # Add ID
    mutate(id = paste0(`Entry ID`, "_", gsub(",", "", `Branching question`), "_", `Region ID`, "_", start_year, "_", end_year))
  
  # Subset data
  a2_data <- a1_data %>%
    filter(id %in% analysis_2_sample$ID) 
  a3_data <- a1_data %>%
    filter(id %in% analysis_3_sample$ID) 
  a4_data <- a1_data %>%
    filter(id %in% analysis_4_sample$ID) 
  
  # Functions to run GLS models
  # average temperature + average precipitation + precipitation variation 
  gls_model_1 <- function(data) {
    set.seed(10)
    gls(var ~ start_temp_avg + start_prep_avg + start_prep_var, data = data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  }
  # Run models if nrow > 1
  gls_model_poss_1 <- possibly(gls_model_1, otherwise = "The model failed to converge")
  
  # average temperature + average precipitation + precipitation variation + elevation 
  gls_model_2 <- function(data) {
    set.seed(10)
    gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation, data = data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  }
  gls_model_poss_2 <- possibly(gls_model_2, otherwise = "The model failed to converge")
  
  # average temperature + average precipitation + precipitation variation + elevation + plants 
  gls_model_3 <- function(data) {
    set.seed(10)
    gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants, data = data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  }
  gls_model_poss_3 <- possibly(gls_model_3, otherwise = "The model failed to converge")
  
  # average temperature + average precipitation + precipitation variation + elevation + plants + mammals
  gls_model_4 <- function(data) {
    set.seed(10)
    gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals, data = data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  }
  gls_model_poss_4 <- possibly(gls_model_4, otherwise = "The model failed to converge")
  
  # average temperature + average precipitation + precipitation variation + elevation + plants + mammals + dist_freshwater
  gls_model_5 <- function(data) {
    set.seed(10)
    gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals + dist_freshwater, data = data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  }
  gls_model_poss_5 <- possibly(gls_model_5, otherwise = "The model failed to converge")
  
  # average temperature + average precipitation + precipitation variation + elevation + plants + mammals + dist_freshwater + dist_coastline
  gls_model_6 <- function(data) {
    set.seed(10)
    gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals + dist_freshwater + dist_coastline, data = data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  }
  # Run models if nrow > 1
  gls_model_poss_6 <- possibly(gls_model_6, otherwise = "The model failed to converge")
  
  # If GLS model converges, run vif
  a1_model_1 <- gls_model_poss_1(data = a1_data)
  a2_model_1 <- gls_model_poss_1(data = a2_data)
  a3_model_1 <- gls_model_poss_1(data = a3_data)
  a4_model_1 <- gls_model_poss_1(data = a4_data)
  
  # Run VIF for each model: average temperature, average precipitation, precipitation variation
  if(class(a1_model_1) == "gls") {
    set.seed(10)
    a1_vif_1 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var, data = a1_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
    # Save output
    write.csv(a1_vif_1, paste0("./output/a1_vif_1_", var, ".csv"))
    if(max(a1_vif_1) <= 5) {
      # Save output 
      if(class(a1_model_1) == "gls") {
        saveRDS(a1_model_1, paste0("./output/a1_m_1_", var, ".rds"))
      } else {
        write.table(a1_model_1, paste0("./output/a1_m_1_", var, ".txt"))
      }
      a1_model_2 <- gls_model_poss_2(data = a1_data)
      if(class(a1_model_2) == "gls") {
        # Run VIF for average temperature + average precipitation + precipitation variation + elevation
        set.seed(10)
        a1_vif_2 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation, data = a1_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
        # Save output
        write.csv(a1_vif_2, paste0("./output/a1_vif_2_", var, ".csv"))
        if(max(a1_vif_2) <= 5) {
          # Save output 
          if(class(a1_model_2) == "gls") {
            saveRDS(a1_model_2, paste0("./output/a1_m_2_", var, ".rds"))
          } else {
            write.table(a1_model_2, paste0("./output/a1_m_2_", var, ".txt"))
          }
        } 
        a1_model_3 <- gls_model_poss_3(data = a1_data)
        if(class(a1_model_3) == "gls") {
          # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants
          set.seed(10)
          a1_vif_3 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants, data = a1_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
          # Save output
          write.csv(a1_vif_3, paste0("./output/a1_vif_3_", var, ".csv"))
          if(max(a1_vif_3) <= 5) {
            # Save output 
            if(class(a1_model_3) == "gls") {
              saveRDS(a1_model_3, paste0("./output/a1_m_3_", var, ".rds"))
            } else {
              write.table(a1_model_3, paste0("./output/a1_m_3_", var, ".txt"))
            }
          } 
          a1_model_4 <- gls_model_poss_4(data = a1_data)
          if(class(a1_model_4) == "gls") {
            # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals
            set.seed(10)
            a1_vif_4 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals, data = a1_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
            # Save output
            write.csv(a1_vif_4, paste0("./output/a1_vif_4_", var, ".csv"))
            if(max(a1_vif_4) <= 5) {
              # Save output 
              if(class(a1_model_4) == "gls") {
                saveRDS(a1_model_4, paste0("./output/a1_m_4_", var, ".rds"))
              } else {
                write.table(a1_model_4, paste0("./output/a1_m_4_", var, ".txt"))
              }
            } 
            a1_model_5 <- gls_model_poss_5(data = a1_data)
            if(class(a1_model_5) == "gls") {
              # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals + distance to freshwater
              set.seed(10)
              a1_vif_5 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals + dist_freshwater, data = a1_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
              # Save output
              write.csv(a1_vif_5, paste0("./output/a1_vif_5_", var, ".csv"))
              if(max(a1_vif_5) <= 5) {
                # Save output 
                if(class(a1_model_5) == "gls") {
                  saveRDS(a1_model_5, paste0("./output/a1_m_5_", var, ".rds"))
                } else {
                  write.table(a1_model_5, paste0("./output/a1_m_5_", var, ".txt"))
                }
              } 
              a1_model_6 <- gls_model_poss_6(data = a1_data)
              if(class(a1_model_6) == "gls") {
                # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals + distance to freshwater + distance to coastline
                set.seed(10)
                a1_vif_6 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals + dist_freshwater + dist_coastline, data = a1_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
                # Save output
                write.csv(a1_vif_6, paste0("./output/a1_vif_6_", var, ".csv"))
                if(max(a1_vif_6) <= 5) {
                  # Save output 
                  if(class(a1_model_6) == "gls") {
                    saveRDS(a1_model_6, paste0("./output/a1_m_6_", var, ".rds"))
                  } else {
                    write.table(a1_model_6, paste0("./output/a1_m_6_", var, ".txt"))
                  }
                }
              } else {
                a1_vif_6 <- "The model failed to converge"
                write.table(a1_vif_6, paste0("./output/a1_m_6_", var, ".txt"))
              }
            } else {
              a1_vif_5 <- "The model failed to converge"
              write.table(a1_vif_5, paste0("./output/a1_m_5_", var, ".txt"))
            }
          } else {
            a1_vif_4 <- "The model failed to converge"
            write.table(a1_vif_4, paste0("./output/a1_m_4_", var, ".txt"))
          }
        } else {
          a1_vif_3 <- "The model failed to converge"
          write.table(a1_vif_3, paste0("./output/a1_m_3_", var, ".txt"))
        }
      } else {
        a1_vif_2 <- "The model failed to converge"
        write.table(a1_vif_2, paste0("./output/a1_m_2_", var, ".txt"))
      }
    }
  } else {
    a1_vif_1 <- "The model failed to converge"
    write.table(a1_vif_1, paste0("./output/a1_m_1_", var, ".txt"))
  }
  if(class(a2_model_1) == "gls") {
    set.seed(10)
    a2_vif_1 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var, data = a2_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
    # Save output
    write.csv(a2_vif_1, paste0("./output/a2_vif_1_", var, ".csv"))
    if(max(a2_vif_1) <= 5) {
      # Save output 
      if(class(a2_model_1) == "gls") {
        saveRDS(a2_model_1, paste0("./output/a2_m_1_", var, ".rds"))
      } else {
        write.table(a2_model_1, paste0("./output/a2_m_1_", var, ".txt"))
      }
      a2_model_2 <- gls_model_poss_2(data = a2_data)
      if(class(a2_model_2) == "gls") {
        set.seed(10)
        # Run VIF for average temperature + average precipitation + precipitation variation + elevation
        a2_vif_2 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation, data = a2_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
        # Save output
        write.csv(a2_vif_2, paste0("./output/a2_vif_2_", var, ".csv"))
        if(max(a2_vif_2) <= 5) {
          # Save output 
          if(class(a2_model_2) == "gls") {
            saveRDS(a2_model_2, paste0("./output/a2_m_2_", var, ".rds"))
          } else {
            write.table(a2_model_2, paste0("./output/a2_m_2_", var, ".txt"))
          }
        }
        a2_model_3 <- gls_model_poss_3(data = a2_data)
        if(class(a2_model_3) == "gls") {
          set.seed(10)
          # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants
          a2_vif_3 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants, data = a2_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
          # Save output
          write.csv(a2_vif_3, paste0("./output/a2_vif_3_", var, ".csv"))
          if(max(a2_vif_3) <= 5) {
            # Save output 
            if(class(a2_model_3) == "gls") {
              saveRDS(a2_model_3, paste0("./output/a2_m_3_", var, ".rds"))
            } else {
              write.table(a2_model_3, paste0("./output/a2_m_3_", var, ".txt"))
            }
          } 
          a2_model_4 <- gls_model_poss_4(data = a2_data)
          if(class(a2_model_4) == "gls") {
            set.seed(10)
            # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals
            a2_vif_4 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals, data = a2_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
            # Save output
            write.csv(a2_vif_4, paste0("./output/a2_vif_4_", var, ".csv"))
            if(max(a2_vif_4) <= 5) {
              # Save output 
              if(class(a2_model_4) == "gls") {
                saveRDS(a2_model_4, paste0("./output/a2_m_4_", var, ".rds"))
              } else {
                write.table(a2_model_4, paste0("./output/a2_m_4_", var, ".txt"))
              }
            } 
            a2_model_5 <- gls_model_poss_5(data = a2_data)
            if(class(a2_model_5) == "gls") {
              set.seed(10)
              # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals + distance to freshwater
              a2_vif_5 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals + dist_freshwater, data = a2_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
              # Save output
              write.csv(a2_vif_5, paste0("./output/a2_vif_5_", var, ".csv"))
              if(max(a2_vif_5) <= 5) {
                # Save output 
                if(class(a2_model_5) == "gls") {
                  saveRDS(a2_model_5, paste0("./output/a2_m_5_", var, ".rds"))
                } else {
                  write.table(a2_model_5, paste0("./output/a2_m_5_", var, ".txt"))
                }
              }  
              a2_model_6 <- gls_model_poss_6(data = a2_data)
              if(class(a2_model_6) == "gls") {
                set.seed(10)
                # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals + distance to freshwater + distance to coastline
                a2_vif_6 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals + dist_freshwater + dist_coastline, data = a2_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
                # Save output
                write.csv(a2_vif_6, paste0("./output/a2_vif_6_", var, ".csv"))
                if(max(a2_vif_6) <= 5) {
                  # Save output 
                  if(class(a2_model_6) == "gls") {
                    saveRDS(a2_model_6, paste0("./output/a2_m_6_", var, ".rds"))
                  } else {
                    write.table(a2_model_6, paste0("./output/a2_m_6_", var, ".txt"))
                  }
                }
              } else {
                a2_vif_6 <- "The model failed to converge"
                write.table(a2_vif_6, paste0("./output/a2_m_6_", var, ".txt"))
              }
            } else {
              a2_vif_5 <- "The model failed to converge"
              write.table(a2_vif_5, paste0("./output/a2_m_5_", var, ".txt"))
            }
          } else {
            a2_vif_4 <- "The model failed to converge"
            write.table(a2_vif_4, paste0("./output/a2_m_4_", var, ".txt"))
          }
        } else {
          a2_vif_3 <- "The model failed to converge"
          write.table(a2_vif_3, paste0("./output/a2_m_3_", var, ".txt"))
        }
      } else {
        a2_vif_2 <- "The model failed to converge"
        write.table(a2_vif_2, paste0("./output/a2_m_2_", var, ".txt"))
      }
    }
  } else {
    a2_vif_1 <- "The model failed to converge"
    write.table(a2_vif_1, paste0("./output/a2_m_1_", var, ".txt"))
  }
  if(class(a3_model_1) == "gls") {
    set.seed(10)
    a3_vif_1 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var, data = a3_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
    # Save output
    write.csv(a3_vif_1, paste0("./output/a3_vif_1_", var, ".csv"))
    if(max(a3_vif_1) <= 5) {
      # Save output 
      if(class(a3_model_1) == "gls") {
        saveRDS(a3_model_1, paste0("./output/a3_m_1_", var, ".rds"))
      } else {
        write.table(a3_model_1, paste0("./output/a3_m_1_", var, ".txt"))
      }
      a3_model_2 <- gls_model_poss_2(data = a3_data)
      if(class(a3_model_2) == "gls") {
        set.seed(10)
        # Run VIF for average temperature + average precipitation + precipitation variation + elevation
        a3_vif_2 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var  + elevation, data = a3_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
        # Save output
        write.csv(a3_vif_2, paste0("./output/a3_vif_2_", var, ".csv"))
        if(max(a3_vif_2) <= 5) {
          # Save output 
          if(class(a3_model_2) == "gls") {
            saveRDS(a3_model_2, paste0("./output/a3_m_2_", var, ".rds"))
          } else {
            write.table(a3_model_2, paste0("./output/a3_m_2_", var, ".txt"))
          }
        }
        a3_model_3 <- gls_model_poss_3(data = a3_data)
        if(class(a3_model_3) == "gls") {
          set.seed(10)
          # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants
          a3_vif_3 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var  + elevation + plants, data = a3_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
          # Save output
          write.csv(a3_vif_3, paste0("./output/a3_vif_3_", var, ".csv"))
          if(max(a3_vif_3) <= 5) {
            # Save output 
            if(class(a3_model_3) == "gls") {
              saveRDS(a3_model_3, paste0("./output/a3_m_3_", var, ".rds"))
            } else {
              write.table(a3_model_3, paste0("./output/a3_m_3_", var, ".txt"))
            }
          }  
          a3_model_4 <- gls_model_poss_4(data = a3_data)
          if(class(a3_model_4) == "gls") {
            set.seed(10)
            # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals
            a3_vif_4 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var  + elevation + plants + mammals, data = a3_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
            # Save output
            write.csv(a3_vif_4, paste0("./output/a3_vif_4_", var, ".csv"))
            if(max(a3_vif_4) <= 5) {
              # Save output 
              if(class(a3_model_4) == "gls") {
                saveRDS(a3_model_4, paste0("./output/a3_m_4_", var, ".rds"))
              } else {
                write.table(a3_model_4, paste0("./output/a3_m_4_", var, ".txt"))
              }
            } 
            a3_model_5 <- gls_model_poss_5(data = a3_data)
            if(class(a3_model_5) == "gls") {
              set.seed(10)
              # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals + distance to freshwater
              a3_vif_5 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var  + elevation + plants + mammals + dist_freshwater, data = a3_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
              # Save output
              write.csv(a3_vif_5, paste0("./output/a3_vif_5_", var, ".csv"))
              if(max(a3_vif_5) <= 5) {
                # Save output 
                if(class(a3_model_5) == "gls") {
                  saveRDS(a3_model_5, paste0("./output/a3_m_5_", var, ".rds"))
                } else {
                  write.table(a3_model_5, paste0("./output/a3_m_5_", var, ".txt"))
                }
              } 
              a3_model_6 <- gls_model_poss_6(data = a3_data)
              if(class(a3_model_6) == "gls") {
                set.seed(10)
                # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals + distance to freshwater + distance to coastline
                a3_vif_6 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var  + elevation + plants + mammals + dist_freshwater + dist_coastline, data = a3_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
                # Save output
                write.csv(a3_vif_6, paste0("./output/a3_vif_6_", var, ".csv"))
                if(max(a3_vif_6) <= 5) {
                  # Save output 
                  if(class(a3_model_6) == "gls") {
                    saveRDS(a3_model_6, paste0("./output/a3_m_6_", var, ".rds"))
                  } else {
                    write.table(a3_model_6, paste0("./output/a3_m_6_", var, ".txt"))
                  }
                } 
              } else {
                a3_vif_6 <- "The model failed to converge"
                write.table(a3_vif_6, paste0("./output/a3_m_6_", var, ".txt"))
              }
            } else {
              a3_vif_5 <- "The model failed to converge"
              write.table(a3_vif_5, paste0("./output/a3_m_5_", var, ".txt"))
            }
          } else {
            a3_vif_4 <- "The model failed to converge"
            write.table(a3_vif_4, paste0("./output/a3_m_4_", var, ".txt"))
          }
        } else {
          a3_vif_3 <- "The model failed to converge"
          write.table(a3_vif_3, paste0("./output/a3_m_3_", var, ".txt"))
        }
      } else {
        a3_vif_2 <- "The model failed to converge"
        write.table(a3_vif_2, paste0("./output/a3_m_2_", var, ".txt"))
      }
    }
  } else {
    a3_vif_1 <- "The model failed to converge"
    write.table(a3_vif_1, paste0("./output/a3_m_1_", var, ".txt"))
  }
  if(class(a4_model_1) == "gls") {
    set.seed(10)
    a4_vif_1 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var, data = a4_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
    # Save output
    write.csv(a4_vif_1, paste0("./output/a4_vif_1_", var, ".csv"))
    if(max(a4_vif_1) <= 5) {
      # Save output 
      if(class(a4_model_1) == "gls") {
        saveRDS(a4_model_1, paste0("./output/a4_m_1_", var, ".rds"))
      } else {
        write.table(a4_mode_1, paste0("./output/a4_m_1_", var, ".txt"))
      }
      a4_model_2 <- gls_model_poss_2(data = a4_data)
      if(class(a4_model_2) == "gls") {
        set.seed(10)
        # Run VIF for average temperature + average precipitation + precipitation variation + elevation
        a4_vif_2 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation, data = a4_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
        # Save output
        write.csv(a4_vif_2, paste0("./output/a4_vif_2_", var, ".csv"))
        if(max(a4_vif_2) <= 5) {
          # Save output 
          if(class(a4_model_2) == "gls") {
            saveRDS(a4_model_2, paste0("./output/a4_m_2_", var, ".rds"))
          } else {
            write.table(a4_mode_2, paste0("./output/a4_m_2_", var, ".txt"))
          }
        }
        a4_model_3 <- gls_model_poss_3(data = a4_data)
        if(class(a4_model_3) == "gls") {
          set.seed(10)
          # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants
          a4_vif_3 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants, data = a4_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
          # Save output
          write.csv(a4_vif_3, paste0("./output/a4_vif_3_", var, ".csv"))
          if(max(a4_vif_3) <= 5) {
            # Save output 
            if(class(a4_model_3) == "gls") {
              saveRDS(a4_model_3, paste0("./output/a4_m_3_", var, ".rds"))
            } else {
              write.table(a4_mode_3, paste0("./output/a4_m_3_", var, ".txt"))
            }
          }
          a4_model_4 <- gls_model_poss_4(data = a4_data)
          if(class(a4_model_4) == "gls") {
            set.seed(10)
            # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals
            a4_vif_4 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals, data = a4_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
            # Save output
            write.csv(a4_vif_4, paste0("./output/a4_vif_4_", var, ".csv"))
            if(max(a4_vif_4) <= 5) {
              # Save output 
              if(class(a4_model_4) == "gls") {
                saveRDS(a4_model_4, paste0("./output/a4_m_4_", var, ".rds"))
              } else {
                write.table(a4_mode_4, paste0("./output/a4_m_4_", var, ".txt"))
              }
            }
            a4_model_5 <- gls_model_poss_5(data = a4_data)
            if(class(a4_model_5) == "gls") {
              set.seed(10)
              # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals + distance to freshwater
              a4_vif_5 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals + dist_freshwater, data = a4_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
              # Save output
              write.csv(a4_vif_5, paste0("./output/a4_vif_5_", var, ".csv"))
              if(max(a4_vif_5) <= 5) {
                # Save output 
                if(class(a4_model_5) == "gls") {
                  saveRDS(a4_model_5, paste0("./output/a4_m_5_", var, ".rds"))
                } else {
                  write.table(a4_mode_5, paste0("./output/a4_m_5_", var, ".txt"))
                }
              }
              a4_model_6 <- gls_model_poss_6(data = a4_data)
              if(class(a4_model_6) == "gls") {
                set.seed(10)
                # Run VIF for average temperature + average precipitation + precipitation variation + elevation + plants + mammals + distance to freshwater + distance to coastline
                a4_vif_6 <- vif(gls(var ~ start_temp_avg + start_prep_avg + start_prep_var + elevation + plants + mammals + dist_freshwater + dist_coastline, data = a4_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit))
                # Save output
                write.csv(a4_vif_6, paste0("./output/a4_vif_6_", var, ".csv"))
                if(max(a4_vif_6) <= 5) {
                  # Save output 
                  if(class(a4_model_6) == "gls") {
                    saveRDS(a4_model_6, paste0("./output/a4_m_6_", var, ".rds"))
                  } else {
                    write.table(a4_mode_6, paste0("./output/a4_m_6_", var, ".txt"))
                  }
                }
              } else {
                a4_vif_6 <- "The model failed to converge"
                write.table(a4_vif_6, paste0("./output/a4_m_6_", var, ".txt"))
              }
            } else {
              a4_vif_5 <- "The model failed to converge"
              write.table(a4_vif_5, paste0("./output/a4_m_5_", var, ".txt"))
            }
          } else {
            a4_vif_4 <- "The model failed to converge"
            write.table(a4_vif_4, paste0("./output/a4_m_4_", var, ".txt"))
          }
        } else {
          a4_vif_3 <- "The model failed to converge"
          write.table(a4_vif_3, paste0("./output/a4_m_3_", var, ".txt"))
        }
      } else {
        a4_vif_2 <- "The model failed to converge"
        write.table(a4_vif_2, paste0("./output/a4_m_2_", var, ".txt"))
      }
    }
  } else {
    a4_vif_1 <- "The model failed to converge"
    write.table(a4_vif_1, paste0("./output/a4_m_1_", var, ".txt"))
  }
}

# Get sample size per variable and condition
get_sample_size <- function(var) {
  a1_data <- analysis_data %>%
    rename(var = all_of(var)) %>%
    filter(!is.na(var)) %>%
    # Add ID
    mutate(id = paste0(`Entry ID`, "_", gsub(",", "", `Branching question`), "_", `Region ID`, "_", start_year, "_", end_year))
  
  # Subset data
  a2_data <- a1_data %>%
    filter(id %in% analysis_2_sample$ID) 
  a3_data <- a1_data %>%
    filter(id %in% analysis_3_sample$ID) 
  a4_data <- a1_data %>%
    filter(id %in% analysis_4_sample$ID) 
  
  # Extract sample size per condition
  a1_sample <- tibble(`Question ID` = var, Sample = 1, `Sample Size` = nrow(a1_data))
  a2_sample <- tibble(`Question ID` = var, Sample = 2, `Sample Size` = nrow(a2_data))
  a3_sample <- tibble(`Question ID` = var, Sample = 3, `Sample Size` = nrow(a3_data))
  a4_sample <- tibble(`Question ID` = var, Sample = 4, `Sample Size` = nrow(a4_data))
  
  # Combine into single tibble
  sample_size <- bind_rows(a1_sample, a2_sample, a3_sample, a4_sample)
}

# Extract results and apply multiple testing correction
extract_results <- function(model) {
  model <- summary(model)
  variables <- names(model$coefficients)
  coefficients <- model$coefficients
  t_table <- as.data.frame(model$tTable)
  p_value <- t_table$`p-value`
  log_lik <- model$logLik
  p_dims <- model$dims$p
  AIC <- (2 * p_dims) - (2 * log_lik)
  output <- data.frame(variables = variables, coefficients = coefficients, p_value = p_value, AIC = AIC)
  output <- output %>%
    mutate(adjust_p_value = p.adjust(p_value, method = "BH", n = 1768)) %>%
    mutate(coefficients = round(coefficients, 2), p_value = signif(p_value, 2), adjust_p_value = signif(adjust_p_value, 2)) %>%
    mutate(AIC = round(AIC, 2)) %>%
    mutate(sig = case_when(adjust_p_value < 0.001 ~ "***", 
                           adjust_p_value >= 0.001 & adjust_p_value < 0.01 ~ "**",
                           adjust_p_value >= 0.01 &  adjust_p_value < 0.05 ~ "*", 
                           adjust_p_value >= 0.05 ~ "" )) %>%
    relocate(AIC, .after = last_col())
}

# Create results tables
create_results_tables <- function(){
  # Load data
  results_files <- list.files(path = "./output", pattern = "*.rds", full.names = T, recursive = T)
  results_list <- lapply(results_files, readRDS)
  results_files_names <- gsub(".rds", "", list.files(path = "./output", pattern = "*.rds", recursive = T))
  names(results_list) <- results_files_names
  txt_files <- list.files(path = "./output", pattern = "*.txt", full.names = T, recursive = T)
  txt_list <- lapply(txt_files, read.table)
  txt_files_names <- gsub(".txt", "", list.files(path = "./output", pattern = "*.txt",  recursive = T))
  names(txt_list) <- txt_files_names
  
  # Extract results for each question
  for(i in 1:nrow(analysis_questions)) {
    var = analysis_questions$`Question ID`[i]
    var_results <- results_list[grepl(var, names(results_list))]
    var_txt <- txt_list[grepl(var, names(txt_list))]
    if(length(var_results) > 0) {
      var_results_list <- lapply(var_results, extract_results)
      var_list <- list()
      for(i in 1:length(var_results_list)) {
        name <- names(var_results_list[i])
        number <- gsub("(.+?)(\\_.*)", "\\1", name)
        number <- gsub("a", "", number)
        var_list[[i]] <- var_results_list[[i]] %>% 
          rename_with(., ~ paste0(., "_", number))
      }
      var_tib <- bind_cols(var_list) %>%
        mutate(`Question ID` = var) %>%
        select(`Question ID`, everything())  
      if(var != "5154") {
        var_tib <- var_tib %>%
          rename(Variable = variables_1)
      } else {
        var_tib <- var_tib %>%
          rename(Variable = variables_3)
      }
    }
    if(length(var_txt) > 0) {
      number_list <- list()
      for(i in 1:length(var_txt)) {
        name <- names(var_txt[i])
        number <- gsub("(.+?)(\\_.*)", "\\1", name)
        number <- gsub("a", "", number) 
        number_list[[i]] <- number
      }
      failed_analysis <- c(number_list)
    } 
    
    if(length(var_results) == 4) {
      var_tib <- var_tib %>%
        select(-variables_2, -variables_3, - variables_4) 
      var_tib
    } else if(length(var_results) > 0 & length(var_txt) > 0) {
      if(length(var_txt) == 1) {
        sample_missing <- tibble(coefficients = rep("-", nrow(var_tib)), p_value = rep("-", nrow(var_tib)), adjust_p_value = rep("-", nrow(var_tib)), sig = rep("-", nrow(var_tib)), AIC = rep("-", nrow(var_tib))) %>% 
          rename_with(., ~ paste0(., "_", failed_analysis[[1]]))
      } else if(length(var_txt) > 1) {
        missing_list <- list()
        for(i in 1:length(failed_analysis)) {
          number <- failed_analysis[[i]]
          missing_list[[i]] <- tibble(coefficients = rep("-", nrow(var_tib)), p_value = rep("-", nrow(var_tib)), adjust_p_value = rep("-", nrow(var_tib)), sig = rep("-", nrow(var_tib)), AIC = rep("-", nrow(var_tib))) %>% 
            rename_with(., ~ paste0(., "_", number))
        }
        sample_missing <- bind_cols(missing_list)
      }
      var_tib <- bind_cols(var_tib, sample_missing) %>%
        select(`Question ID`, Variable, coefficients_1, p_value_1, adjust_p_value_1, sig_1, AIC_1, coefficients_2, p_value_2, adjust_p_value_2, sig_2, AIC_2, coefficients_3, p_value_3, adjust_p_value_3, sig_3, AIC_3, coefficients_4, p_value_4, adjust_p_value_4, sig_4, AIC_4) 
    }
    # Join with question name 
    if(length(var_results) > 0) {
      output <- var_tib %>%
        left_join(analysis_questions, by = "Question ID") %>%
        select(Question, everything())
      write_csv(output, paste0("../results/", var, ".csv"))
    } else {
      output <- "The algorithm failed to converge in all sample conditions."
      write.table(output, paste0("../results/", var, ".txt"), quote = FALSE, row.names = FALSE)
    }
  }
}

# Get descriptive statistics for analysis questions per sample
get_sample_stats <- function() {
  # Sample 1
  sample_1 <- data %>%
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
    mutate(N = sum(`Frequency No`, `Frequency Yes`, na.rm = T)) %>%
    ungroup() %>%
    left_join(analysis_questions, by = "Question ID") %>%
    mutate(Sample = 1)
  
  # Sample 2
  sample_2 <- data %>%
    mutate(id = paste0(`Entry ID`, "_", gsub(",", "", `Branching question`), "_", `Region ID`, "_", start_year, "_", end_year)) %>%
    filter(id %in% analysis_2_sample$ID) %>%
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
    mutate(N = sum(`Frequency No`, `Frequency Yes`, na.rm = T)) %>%
    ungroup() %>%
    left_join(analysis_questions, by = "Question ID") %>%
    mutate(Sample = 2)
  
  # Sample 3
  sample_3 <- data %>%
    mutate(id = paste0(`Entry ID`, "_", gsub(",", "", `Branching question`), "_", `Region ID`, "_", start_year, "_", end_year)) %>%
    filter(id %in% analysis_3_sample$ID) %>%
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
    mutate(N = sum(`Frequency No`, `Frequency Yes`, na.rm = T)) %>%
    ungroup() %>%
    left_join(analysis_questions, by = "Question ID") %>%
    mutate(Sample = 3) 
  
  # Sample 4
  sample_4 <- data %>%
    mutate(id = paste0(`Entry ID`, "_", gsub(",", "", `Branching question`), "_", `Region ID`, "_", start_year, "_", end_year)) %>%
    filter(id %in% analysis_4_sample$ID) %>%
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
    mutate(N = sum(`Frequency No`, `Frequency Yes`, na.rm = T)) %>%
    ungroup() %>%
    left_join(analysis_questions, by = "Question ID") %>%
    mutate(Sample = 4) 
  
  # Combine into single tibble
  sample_stats <- bind_rows(sample_1, sample_2, sample_3, sample_4) %>%
    arrange(`Question ID`) %>%
    select(Question, Sample, N, `Frequency Yes`, `Frequency No`, `Percent Yes`, `Percent No`) %>%
    mutate(across(`Frequency Yes`:`Percent No`, ~ ifelse(is.na(.), 0.00, .)))
}
