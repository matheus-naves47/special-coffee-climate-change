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

# index = 1
# spatvector = regions_geoloc

year_inds <- seq(1961, 2020)

# Function to extract each variable by year (parallelized) and output a single dataframe
extract_by_year <- function(index, spatvector) {
  temp_year <- year_inds[index] # Year index
  
  # Import raster data
  tmin_rst <-
    rast(paste0(
      "data/climate_grids/BR_DWGD/tmin/Tmin_BR-DWGD_",
      temp_year,
      ".nc"
    ))
  tmax_rst <-
    rast(paste0(
      "data/climate_grids/BR_DWGD/tmax/Tmax_BR-DWGD_",
      temp_year,
      ".nc"
    ))
  prec_rst <-
    rast(paste0(
      "data/climate_grids/BR_DWGD/pr/pr_BR-DWGD_",
      temp_year,
      ".nc"
    ))
  
  # Set raster names as timestamps
  names(tmin_rst) <- terra::time(tmin_rst)
  names(tmax_rst) <- terra::time(tmax_rst)
  names(prec_rst) <- terra::time(prec_rst)
  
  # Extract the data for each variable
  tmin_tmp <-
    exact_extract(tmin_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  tmax_tmp <-
    exact_extract(tmax_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  prec_tmp <-
    exact_extract(prec_rst,
                  spatvector,
                  c("mean", "sum"),
                  append_cols = T)
  
  # Pivot each dataframe to long format
  tmin_tmp <-
    tmin_tmp %>% pivot_longer(!(1:ncol(spatvector)-1),
                              names_to = "DATE",
                              values_to = "TMIN")
  tmax_tmp <-
    tmax_tmp %>% pivot_longer(!(1:ncol(spatvector)-1),
                              names_to = "DATE",
                              values_to = "TMAX")
  # prec_tmp <-
  #   prec_tmp %>% pivot_longer(!(1:ncol(spatvector)-1),
  #                             names_to = "DATE",
  #                             values_to = "PREC")
  prec_tmp <-
    prec_tmp |> pivot_longer(!(1:ncol(spatvector) - 1),
                             names_to = c(".value", "date"),
                             names_sep = "\\.") |>
    rename_with( ~ paste0("PR_", .),-(1:ncol(spatvector)))
  
  
  # Combine everything in a single dataframe
  df_tmp <-
    tibble(
      REGION = tmin_tmp$REGION,
      UF = tmin_tmp$NAME_1,
      UF_2 = tmin_tmp$NAME_3,
      DATE = tmin_tmp$DATE,
      TMIN = tmin_tmp$TMIN,
      TMAX = tmax_tmp$TMAX,
      PREC_MEAN = prec_tmp$PR_mean,
      PREC_SUM = prec_tmp$PR_sum
    ) %>% mutate(DATE = str_replace_all(DATE, "mean.", ""),
                 DATE = as_date(DATE))
  
  return(df_tmp) # Return dataframe
  
  gc() # Garbage collect
  
}


# Apply function in parallel to all years of interest
tic()
df_br_dwgd <-
  mclapply(1:length(year_inds), function(x)
    extract_by_year(x, regions_geoloc), mc.cores = 6) %>% bind_rows()
toc()

# Export
write_parquet(df_br_dwgd, "data/regions_data/BR_DWGD_regions_daily.parquet")
