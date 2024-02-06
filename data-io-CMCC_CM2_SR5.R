library(tidyverse)
library(ncdf4)
library(terra)
library(arrow)
library(tictoc)
library(tidyterra)
library(geodata)
library(exactextractr)
library(parallel)

### Import geodata

regions_geoloc <-
  unwrap(read_rds("data/geodata/origin_regions_geodata.rds")) %>% select(REGION, NAME_1, NAME_3, geometry)

### Data extraction from climate grids

year_inds <- seq(1961, 2014)
index <- 1
spatvector <- regions_geoloc

# Function to extract each variable by year (parallelized) and output a single dataframe
extract_by_year <- function(index, spatvector) {
  temp_year <- year_inds[index] # Year index
  
  # Import raster data
  prec_rst <-
    rast(
      paste0(
        "data/climate_grids/CMIP6/CMCC-CM2-SR5/hist/pr/pr_day_CMCC-CM2-SR5_",
        temp_year,
        ".nc"
      )
    )
  
  # # Set raster names as timestamps
  names(prec_rst) <- terra::time(prec_rst)
  
  # Extract the data
  prec_tmp <-
    exact_extract(prec_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  
  # Set extracted dataframes names as raster timestamps
  # With HadGEM3 data, a "names not unique" error was thrown
  # names(tmin_tmp)[ncol(spatvector):ncol(tmin_tmp)] <-
  #   as.character(terra::time(tmin_rst))
  # names(tmax_tmp)[ncol(spatvector):ncol(tmax_tmp)] <-
  #   as.character(terra::time(tmax_rst))
  # names(prec_tmp)[ncol(spatvector):ncol(prec_tmp)] <-
  #   as.character(terra::time(prec_rst))
  
  # Pivot each dataframe to long format
  prec_tmp <-
    prec_tmp %>% pivot_longer(!(1:ncol(spatvector)-1),
                              names_to = "DATE",
                              values_to = "PREC")
  
  
  # Combine everything in a single dataframe
  df_tmp <-
    tibble(
      REGION = prec_tmp$REGION,
      UF = prec_tmp$NAME_1,
      UF_2 = prec_tmp$NAME_3,
      DATE = prec_tmp$DATE,
      PREC = prec_tmp$PREC
    ) %>% mutate(DATE = str_replace_all(DATE, "mean.", ""),
                 DATE = as_date(DATE)) %>%
    mutate(
      PREC = PREC * 86400
    ) # Convert units
  
  return(df_tmp) # Return dataframe
  
  gc() # Garbage collect
  
}


# Apply function in parallel to all years of interest
tic()
df_cmcc_cm2_sr5 <-
  mclapply(1:length(year_inds), function(x)
    extract_by_year(x, regions_geoloc), mc.cores = 4) %>% bind_rows()
toc()

# Export
write_parquet(df_cmcc_cm2_sr5, "data/CMCC-CM2-SR5_regions_daily.parquet")
