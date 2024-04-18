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
cmip6_path <- "../data/climate_grids/CMIP6/"

year_inds <- seq(1961, 2014)

models <-
  list.dirs(path = cmip6_path,
            full.names = F,
            recursive = F)

# index = 1
# spatvector = regions_geoloc
# path = cmip6_path
# model = models[9]
# remapping = 'conservative'

# Function to extract each variable by year (parallelized) and output a single dataframe
extract_by_year <-
  function(index, spatvector, path, model, remapping) {
    temp_year <- year_inds[index] # Year index
    
    if (model == 'CMCC-CM2-SR5') {
      # CMCC-CM2-SR5 model only has precipitation (pr) variable
      prec_rst <-
        rast(
          paste0(
            path,
            model,
            '/historical/pr/',
            remapping,
            '/pr_day_',
            model,
            "_",
            temp_year,
            '.nc'
          )
        )
      
      # Set raster names as timestamps
      names(prec_rst) <- terra::time(prec_rst)
      
      # Then extract the data for each variable
      prec_tmp <-
        exact_extract(prec_rst,
                      spatvector,
                      fun = "mean",
                      append_cols = T)
      
      # Pivot dataframe to long format
      prec_tmp <-
        prec_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
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
        mutate(PREC = PREC * 86400) # Convert units
      
      return(df_tmp) # Return dataframe
      
    }
    else {
      # For the rest of the models, import raster data for the three variables
      tmin_rst <-
        rast(
          paste0(
            path,
            model,
            '/historical/tasmin/',
            remapping,
            '/tasmin_day_',
            model,
            "_",
            temp_year,
            '.nc'
          )
        )
      
      tmax_rst <-
        rast(
          paste0(
            path,
            model,
            '/historical/tasmax/',
            remapping,
            '/tasmax_day_',
            model,
            "_",
            temp_year,
            '.nc'
          )
        )
      
      prec_rst <-
        rast(
          paste0(
            path,
            model,
            '/historical/pr/',
            remapping,
            '/pr_day_',
            model,
            "_",
            temp_year,
            '.nc'
          )
        )
    }
    if (model == "HadGEM3-GC31-MM") {
      # Extract the data for each variable first
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
                      fun = "mean",
                      append_cols = T)
      
      # Then set extracted dataframes names as raster timestamps
      # HadGEM3 data, a "names not unique" error was thrown
      names(tmin_tmp)[ncol(spatvector):ncol(tmin_tmp)] <-
        as.character(terra::time(tmin_rst))
      names(tmax_tmp)[ncol(spatvector):ncol(tmax_tmp)] <-
        as.character(terra::time(tmax_rst))
      names(prec_tmp)[ncol(spatvector):ncol(prec_tmp)] <-
        as.character(terra::time(prec_rst))
    }
    else {
      # Set raster names as timestamps first
      names(tmin_rst) <- terra::time(tmin_rst)
      names(tmax_rst) <- terra::time(tmax_rst)
      names(prec_rst) <- terra::time(prec_rst)
      
      # Then extract the data for each variable
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
                      fun = "mean",
                      append_cols = T)
      
    }
    
    # Pivot each dataframe to long format
    tmin_tmp <-
      tmin_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
                                names_to = "DATE",
                                values_to = "TMIN")
    tmax_tmp <-
      tmax_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
                                names_to = "DATE",
                                values_to = "TMAX")
    prec_tmp <-
      prec_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
                                names_to = "DATE",
                                values_to = "PREC")
    
    
    # Combine everything in a single dataframe
    df_tmp <-
      tibble(
        REGION = tmin_tmp$REGION,
        UF = tmin_tmp$NAME_1,
        UF_2 = tmin_tmp$NAME_3,
        DATE = tmin_tmp$DATE,
        TMIN = tmin_tmp$TMIN,
        TMAX = tmax_tmp$TMAX,
        PREC = prec_tmp$PREC
      ) %>% mutate(DATE = str_replace_all(DATE, "mean.", ""),
                   DATE = as_date(DATE)) %>%
      mutate(
        TMIN = TMIN - 273.15,
        TMAX = TMAX - 273.15,
        PREC = PREC * 86400
      ) # Convert units
    
    return(df_tmp) # Return dataframe
    
    gc() # Garbage collect
    
  }


# Apply function in parallel to all years of interest, across all models remapped with 1st order conservative method
tic()
for (model in models) {
  assign(
    paste0("df_", model),
    mclapply(1:length(year_inds), function(x)
      extract_by_year(
        x,
        regions_geoloc,
        path = cmip6_path,
        model,
        remapping = "conservative"
      ), mc.cores = 6) %>% bind_rows()
  )
  
}
toc()

# Export
for (model in models) {
  write_parquet(
    get(paste0("df_", model)),
    paste0(
      "data/regions_data/",
      model,
      "_regions_daily_conservative.parquet"
    )
  )
  
}

# Apply function in parallel to all years of interest, across all models remapped with bilinear method
tic()
for (model in models) {
  assign(
    paste0("df_", model),
    mclapply(1:length(year_inds), function(x)
      extract_by_year(
        x,
        regions_geoloc,
        path = cmip6_path,
        model,
        remapping = "bilinear"
      ), mc.cores = 6) %>% bind_rows()
  )
  
}
toc()

# Export
for (model in models) {
  write_parquet(
    get(paste0("df_", model)),
    paste0(
      "data/regions_data/",
      model,
      "_regions_daily_bilinear.parquet"
    )
  )
  
}
