
# Load libraries
library(pcaMethods)
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

# Get population size
get_population <- function (data) {
  
  # Extract raster layer names
  pop_names <- names(pop)
  
  # Extract the years of population rasterstack
  pop_raster_years <- data.frame(name = pop_names) %>%
    mutate(date = gsub("popc_", "", name)) %>%
    mutate(year = as.character(parse_number(date))) %>%
    mutate(year = ifelse(grepl("BC", date), paste0("-", year), year)) %>%
    mutate(year = as.numeric(year))
  
  # Extract population per time period
  pop_list <- list()
  # Extract years
  for (i in 1:nrow(data)){
    if(data$start_year[i] < 1 & data$end_year[i] < 1){
      years <- pop_raster_years %>%
        filter(year >= data$start_millennium[i] & year <= data$end_millennium[i])
    } else if(data$start_year[i] < 1 & data$end_year[i] > 1 & data$end_year[i] <= 1700){
      years <- pop_raster_years %>%
        filter(year >= data$start_millennium[i] & year <= data$end_century[i])
    } else if(data$start_year[i] < 1 & data$end_year[i] > 1700 & data$end_year[i] <= 2000){
      years <- pop_raster_years %>%
        filter(year >= data$start_millennium[i] & year <= data$end_decade[i]) 
    } else if(data$start_year[i] < 1 & data$end_year[i] > 2000){
      years <- pop_raster_years %>%
        filter(year >= data$start_millennium[i] & year <= data$end_year_2017[i]) 
    } else if(data$start_year[i] >= 1 & data$start_year[i] <= 1700 & data$end_year[i] <= 1700){
      years <- pop_raster_years %>%
        filter(year >= data$start_century[i] & year <= data$end_century[i]) 
    } else if(data$start_year[i] >= 1 & data$start_year[i] <= 1700 & data$end_year[i] > 1700 & data$end_year[i] <= 2000){
      years <- pop_raster_years %>%
        filter(year >= data$start_century[i] & year <= data$end_decade[i]) 
    } else if(data$start_year[i] >= 1 & data$start_year[i] <= 1700 & data$end_year[i] > 2000){
      years <- pop_raster_years %>%
        filter(year >= data$start_century[i] & year <= data$end_year_2017[i]) 
    } else if(data$start_year[i] > 1700 & data$start_year[i] <= 2000 & data$end_year[i] <= 2000){
      years <- pop_raster_years %>%
        filter(year >= data$start_decade[i] & year <= data$end_decade[i]) 
    } else if(data$start_year[i] > 1700 & data$start_year[i] <= 2000 & data$end_year[i] > 2000){
      years <- pop_raster_years %>%
        filter(year >= data$start_decade[i] & year <= data$end_year_2017[i]) 
    } else if(data$start_year[i] > 2000){
      years <- pop_raster_years %>%
        filter(year >= data$start_year_2017[i] & year <= data$end_year_2017[i]) 
    }
    # Subset population by years
    pop_years <- subset(pop, years$name)
    if(nrow(years) > 1) {
      values <- exact_extract(pop_years, data$geometry[i], fun = "sum")
      start_pop <- values[,1]
      mid_pop <- values[,round(ncol(values)/2)]
      end_pop <- values[,ncol(values)]
      pop_values <- values %>%
        # Find mean over years
        mutate(avg_pop = rowMeans(.)) %>%
        dplyr::select(avg_pop) %>%
        mutate(start_pop = start_pop, mid_pop = mid_pop, end_pop = end_pop)
      pop_list[[i]] <- pop_values 
    } else {
      pop_values <- exact_extract(pop_years, data$geometry[i], fun = "sum") 
      pop_list[[i]] <- data.frame(avg_pop = pop_values, start_pop = pop_values, mid_pop = pop_values, end_pop = pop_values)
    }
  } 
  data_pop <- bind_rows(pop_list) 
  data_pop <- bind_cols(data, data_pop)
  data_pop
}

# Find the number of words the match between strings
# from https://stackoverflow.com/questions/65409395/countidentify-common-words-in-two-string-vectors-r
num_words <- function(str1, str2) {
  mapply(function(x, y) length(intersect(x, y)), 
         strsplit(str1, ' '), strsplit(str2, ' '))
}

# Find the words the match between strings
# from https://stackoverflow.com/questions/65409395/countidentify-common-words-in-two-string-vectors-r
match_words <- function(str1, str2) {
  mapply(function(x, y) paste0(intersect(x, y),collapse = " "), 
         strsplit(str1, ' '), strsplit(str2, ' '))
}

# Calculate similarity in answers between entries for variable of interest
calc_similarity <- function(data, variable) {
  # Extract variable of interest
  analysis_variable <- data[[variable]]
  
  # Join with ID
  variable_ID <- as_tibble(cbind(ID = data$ID, analysis_variable)) %>%
    drop_na()
  
  # Create pairs of entries and calculate similarity
  entry_pairs <- expand.grid(variable_ID$ID, variable_ID$ID) %>%
    rename(ID = Var1) %>%
    left_join(variable_ID) %>%
    rename(ID_1 = ID, var_1 = analysis_variable, ID = Var2) %>%
    left_join(variable_ID) %>%
    rename(ID_2 = ID, var_2 = analysis_variable) %>%
    mutate(similarity = case_when(var_1 == var_2 ~ 0, var_1 != var_2 ~ 1)) 
  
  # Return pairwise matrix
  variable_distance <- xtabs(similarity ~ ID_1 + ID_2, data = entry_pairs)
}

# Create distance matrix of ecological principal components
calc_pc_distance <- function(data, pc) {
  # Extract principal component of interest
  analysis_pc <- data[[pc]]
  
  # Join with ID
  pc_ID <- as_tibble(cbind(ID = data$ID, analysis_pc)) %>%
    drop_na()
  
  # Create pairs of entries and calculate distance
  entry_pairs <- expand.grid(pc_ID$ID, pc_ID$ID) %>%
    rename(ID = Var1) %>%
    left_join(pc_ID) %>%
    rename(ID_1 = ID, var_1 = analysis_pc, ID = Var2) %>%
    left_join(pc_ID) %>%
    rename(ID_2 = ID, var_2 = analysis_pc) %>%
    mutate(across(starts_with("var_"), ~ as.numeric(.))) %>%
    mutate(distance = abs(var_1 - var_2))

  # Return pairwise matrix
  variable_distance <- xtabs(distance ~ ID_1 + ID_2, data = entry_pairs)
}

# Perform PPCA and extract matrices of PCs per sample
ppca_sample <- function(data, analysis) {
  
  # Prepare data for PPCA
  ppca_data <- data %>%
    select(start_temp_avg, start_temp_min, start_temp_max, start_prep_avg, start_prep_min, start_prep_max, dist_coastline, dist_lakes, dist_rivers, elevation, mammals, plants) %>%
    # Convert -Inf and NaN to NA
    mutate(across(start_temp_avg:plants, ~ ifelse(is.nan(.), NA, .))) %>%
    mutate(across(start_temp_avg:plants, ~ ifelse(is.infinite(.), NA, .))) %>%
    # Scale distance and numeric variables
    mutate(across(start_temp_avg:plants, ~ as.numeric(scale(., center = TRUE, scale = TRUE)))) 
  
  # Probabilistic PCA (PPCA) with 5 principal components
  ppca_output <- pca(ppca_data, method="ppca", nPcs=4, center=FALSE)
  
  # Extract PC scores
  pca_scores <- as_tibble(scores(ppca_output))
  
  # Join with metadata
  pca_meta <- data %>%
    select(ID) %>%
    bind_cols(pca_scores)
  
  # Extract at PC loadings
  ppca_loadings <- loadings(ppca_output)
  ppca_loadings <- round(ppca_loadings, 2)
  
  # Extract cumulative R2
  cum_R2 <- R2cum(ppca_output)
  cum_R2 <- round(cum_R2, 2)
  
  # Extract PC distance between entries
  pc_1 <- calc_pc_distance(pca_meta, "PC1")
  pc_2 <- calc_pc_distance(pca_meta, "PC2")
  pc_3 <- calc_pc_distance(pca_meta, "PC3")
  pc_4 <- calc_pc_distance(pca_meta, "PC4")
  
  # Save output
  write.csv(pc_1, paste0("./output/pc_1_", analysis, ".csv"))
  write.csv(pc_2, paste0("./output/pc_2_", analysis, ".csv"))
  write.csv(pc_3, paste0("./output/pc_3_", analysis, ".csv"))
  write.csv(pc_4, paste0("./output/pc_4_", analysis, ".csv"))
  write.csv(ppca_loadings, paste0("./output/ppca_loadings_", analysis, ".csv"))
  write.csv(cum_R2, paste0("./output/ppca_cum_R2_", analysis, ".csv"))
}

# Run GLS
run_gls <- function(data, var) {
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
  
  # Run models if nrow > 1
  a1_model <- gls(var ~ PC1 + PC2 + PC3 + dist_coastline + dist_freshwater, data = a1_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  a2_model <- gls(var ~ PC1 + PC2 + PC3 + dist_coastline + dist_freshwater, data = a2_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  a3_model <- gls(var ~ PC1 + PC2 + PC3 + dist_coastline + dist_freshwater, data = a3_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  a4_model <- gls(var ~ PC1 + PC2 + PC3 + dist_coastline + dist_freshwater, data = a4_data, correlation = corSpher(form = ~ latitude + longitude), na.action = na.omit)
  
  # Summary
  a1_model <- summary(a1_model)
  a2_model <- summary(a2_model)
  a3_model <- summary(a3_model)
  a4_model <- summary(a4_model)
  
  # Save output
  saveRDS(a1_model, paste0("./output/a1_m_", var, ".rds"))
  saveRDS(a2_model, paste0("./output/a2_m_", var, ".rds"))
  saveRDS(a3_model, paste0("./output/a3_m_", var, ".rds"))
  saveRDS(a4_model, paste0("./output/a4_m_", var, ".rds"))
}

# Extract results
extract_results <- function(data) {
  variables = names(data$coefficients)
  coefficients = data$coefficients
  t_table = as.data.frame(data$tTable)
  p_value = t_table$`p-value`
  log_lik = data$logLik
  p_dims = data$dims$p
  AIC = (2 * p_dims) - (2 * log_lik)
  output = data.frame(Variables = variables, Coefficients = coefficients, p_value = p_value, AIC = rep(AIC, 6))
  output = output %>%
    mutate(Coefficients = round(Coefficients, 2)) %>%
    mutate(AIC = round(AIC, 2)) %>%
    mutate(Sig = case_when(p_value < 0.001 ~ "***", 
                           p_value >= 0.001 & p_value < 0.01 ~ "**",
                           p_value >= 0.01 &  p_value < 0.05 ~ "*", 
                           p_value >= 0.05 ~ "" )) %>%
    mutate(Coefficients = paste0(Coefficients, " (", Sig, ")")) %>%
    mutate(Coefficients = gsub(" \\(\\)", "", Coefficients)) %>%
    select(-p_value, -Sig) %>%
    group_by(AIC) %>%
    pivot_wider(names_from = Variables, values_from = Coefficients) %>%
    select(-AIC, AIC) %>%
    ungroup()
}

# Create results table
create_results_table <- function(){
  # Load data
  results_files <- list.files(path = "./output", pattern = "*.rds", full.names = T, recursive = T)
  results_list <- lapply(results_files, readRDS)
  results_files_names <- gsub(".rds", "", list.files(path = "./output", pattern = "*.rds", recursive = T))
  results_files_names <- gsub("output/", "", results_files_names)
  names(results_list) <- results_files_names
  invisible(list2env(results_list, envir = .GlobalEnv))
  
  # Extract results for high gods
  high_gods <- list(a1_m_4828, a2_m_4828, a3_m_4828, a4_m_4828)
  high_gods <- lapply(high_gods, extract_results)
  high_gods <- bind_rows(high_gods) %>%
    mutate(Variable = "High Gods") %>%
    mutate(Sample = row_number()) %>%
    select(Variable, Sample, everything())
  
  # Extract results for vaiables
  # High gods
  high_gods <- list(a1_m_4828, a2_m_4828, a3_m_4828, a4_m_4828)
  high_gods <- lapply(high_gods, extract_results)
  high_gods <- bind_rows(high_gods) %>%
    mutate(Variable = "High Gods") %>%
    mutate(Sample = row_number()) %>%
    select(Variable, Sample, everything())
  
  # Good Weather/Healthy Crops
  good_weather <- list(a1_m_5007, a2_m_5007, a3_m_5007, a4_m_5007)
  good_weather <- lapply(good_weather, extract_results)
  good_weather <- bind_rows(good_weather) %>%
    mutate(Variable = "Good Weather/Healthy Crops") %>%
    mutate(Sample = row_number()) %>%
    select(Variable, Sample, everything())
  
  # Bad Weather/Crop Failure 
  bad_weather <- list(a1_m_5038, a2_m_5038, a3_m_5038, a4_m_5038)
  bad_weather <- lapply(bad_weather, extract_results)
  bad_weather <- bind_rows(bad_weather) %>%
    mutate(Variable = "Bad Weather/Crop Failure") %>%
    mutate(Sample = row_number()) %>%
    select(Variable, Sample, everything())
  
  # Fishing
  fishing <- list(a1_m_5227_3, a2_m_5227_3, a3_m_5227_3, a4_m_5227_3)
  fishing <- lapply(fishing, extract_results)
  fishing <- bind_rows(fishing) %>%
    mutate(Variable = "Fishing") %>%
    mutate(Sample = row_number()) %>%
    select(Variable, Sample, everything())
  
  # Previously Human Spirits
  human_spirits <- list(a1_m_4866, a2_m_4866, a3_m_4866, a4_m_4866)
  human_spirits <- lapply(human_spirits, extract_results)
  human_spirits <- bind_rows(human_spirits) %>%
    mutate(Variable = "Previously Human Spirits") %>%
    mutate(Sample = row_number()) %>%
    select(Variable, Sample, everything())
  
  # Non-human Supernatural Beings
  non_human <- list(a1_m_4897, a2_m_4897, a3_m_4897, a4_m_4897)
  non_human <- lapply(non_human, extract_results)
  non_human <- bind_rows(non_human) %>%
    mutate(Variable = "Non-human Supernatural Beings") %>%
    mutate(Sample = row_number()) %>%
    select(Variable, Sample, everything())
  
  # Mixed Human-divine Beings
  mixed_human_divine <- list(a1_m_4919, a2_m_4919, a3_m_4919, a4_m_4919)
  mixed_human_divine <- lapply(mixed_human_divine, extract_results)
  mixed_human_divine <- bind_rows(mixed_human_divine) %>%
    mutate(Variable = "Mixed Human-divine Beings") %>%
    mutate(Sample = row_number()) %>%
    select(Variable, Sample, everything())
  
  # Combine into single data.frame
  results <- bind_rows(high_gods, good_weather, bad_weather, fishing, human_spirits, non_human, mixed_human_divine)
}

