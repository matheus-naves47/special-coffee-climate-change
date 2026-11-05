# Function to extract each variable by year and output a single dataframe
extract_by_year_CMIP6 <-
  function(index, spatvector, path, model) {
    temp_year <- year_inds[index] # Year index

    if (model == "CMCC-CM2-SR5") {
      # CMCC-CM2-SR5 model only has precipitation (pr) variable
      prec_rst <-
        rast(
          paste0(
            path,
            model,
            "/historical/pr/pr_day_",
            model,
            "_",
            temp_year,
            ".nc"
          )
        )

      # Set raster names as timestamps
      names(prec_rst) <- terra::time(prec_rst)

      # Then extract the data for each variable
      prec_tmp <-
        exact_extract(prec_rst,
          spatvector,
          fun = "sum",
          append_cols = T
        )

      # Pivot dataframe to long format
      prec_tmp <-
        prec_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
          names_to = "DATE",
          values_to = "PREC"
        )


      # Combine everything in a single dataframe
      df_tmp <-
        tibble(
          REGION = prec_tmp$bsca_region,
          UF = prec_tmp$name_state,
          UF_2 = prec_tmp$abbrev_state,
          DATE = prec_tmp$DATE,
          PREC = prec_tmp$PREC * 86400
        ) %>%
        mutate(
          DATE = str_replace_all(DATE, "sum.", ""),
          DATE = as_date(DATE)
        )

      return(df_tmp) # Return dataframe

    } else {
      # For the rest of the models, import raster data for the three variables
      tmin_rst <-
        rast(
          paste0(
            path,
            model,
            "/historical/tasmin/tasmin_day_",
            model,
            "_",
            temp_year,
            ".nc"
          )
        )

      tmax_rst <-
        rast(
          paste0(
            path,
            model,
            "/historical/tasmax/tasmax_day_",
            model,
            "_",
            temp_year,
            ".nc"
          )
        )

      prec_rst <-
        rast(
          paste0(
            path,
            model,
            "/historical/pr/pr_day_",
            model,
            "_",
            temp_year,
            ".nc"
          )
        )
    }
    if (model == "HadGEM3-GC31-MM") {
      # Extract the data for each variable first
      tmin_tmp <-
        exact_extract(tmin_rst,
          spatvector,
          fun = "mean",
          append_cols = T
        )
      tmax_tmp <-
        exact_extract(tmax_rst,
          spatvector,
          fun = "mean",
          append_cols = T
        )
      prec_tmp <-
        exact_extract(prec_rst,
          spatvector,
          fun = "sum",
          append_cols = T
        )

      # Then set extracted dataframes names as raster timestamps
      # HadGEM3 data, a "names not unique" error was thrown
      names(tmin_tmp)[ncol(spatvector):ncol(tmin_tmp)] <-
        as.character(terra::time(tmin_rst))
      names(tmax_tmp)[ncol(spatvector):ncol(tmax_tmp)] <-
        as.character(terra::time(tmax_rst))
      names(prec_tmp)[ncol(spatvector):ncol(prec_tmp)] <-
        as.character(terra::time(prec_rst))
    } else {
      # Set raster names as timestamps first
      names(tmin_rst) <- terra::time(tmin_rst)
      names(tmax_rst) <- terra::time(tmax_rst)
      names(prec_rst) <- terra::time(prec_rst)

      # Then extract the data for each variable
      tmin_tmp <-
        exact_extract(tmin_rst,
          spatvector,
          fun = "mean",
          append_cols = T
        )
      tmax_tmp <-
        exact_extract(tmax_rst,
          spatvector,
          fun = "mean",
          append_cols = T
        )
      prec_tmp <-
        exact_extract(prec_rst,
          spatvector,
          fun = "sum",
          append_cols = T
        )
    }

    # Pivot each dataframe to long format
    tmin_tmp <-
      tmin_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
        names_to = "DATE",
        values_to = "TMIN"
      )
    tmax_tmp <-
      tmax_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
        names_to = "DATE",
        values_to = "TMAX"
      )
    prec_tmp <-
      prec_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
        names_to = "DATE",
        values_to = "PREC"
      )


    # Combine everything in a single dataframe
    df_tmp <-
      tibble(
        REGION = tmin_tmp$bsca_region,
        UF = tmin_tmp$name_state,
        UF_2 = tmin_tmp$abbrev_state,
        DATE = tmin_tmp$DATE,
        TMIN = tmin_tmp$TMIN - 273.15,
        TMAX = tmax_tmp$TMAX - 273.15,
        PREC = prec_tmp$PREC * 86400
      ) %>%
      mutate(
        DATE = str_replace_all(DATE, "mean.", ""),
        DATE = str_replace_all(DATE, "sum.", ""),
        DATE = as_date(DATE)
      )

    return(df_tmp) # Return dataframe
    gc() # Garbage collect
  }


# Function to extract each variable by year and output a single dataframe in municipal level
extract_by_year_CMIP6_muni <-
  function(index, spatvector, path, model) {
    temp_year <- year_inds[index] # Year index
    
    if (model == "CMCC-CM2-SR5") {
      # CMCC-CM2-SR5 model only has precipitation (pr) variable
      prec_rst <-
        rast(
          paste0(
            path,
            model,
            "/historical/pr/pr_day_",
            model,
            "_",
            temp_year,
            ".nc"
          )
        )
      
      # Set raster names as timestamps
      names(prec_rst) <- terra::time(prec_rst)
      
      # Then extract the data for each variable
      prec_tmp <-
        exact_extract(prec_rst,
                      spatvector,
                      fun = "sum",
                      append_cols = T
        )
      
      # Pivot dataframe to long format
      prec_tmp <-
        prec_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
                                  names_to = "DATE",
                                  values_to = "PREC"
        )
      
      
      # Combine everything in a single dataframe
      df_tmp <-
        tibble(
          REGION = prec_tmp$bsca_region,
          MUNICIPALITY = prec_tmp$name_muni,
          UF = prec_tmp$name_state,
          UF_2 = prec_tmp$abbrev_state,
          DATE = prec_tmp$DATE,
          PREC = prec_tmp$PREC * 86400
        ) %>%
        mutate(
          DATE = str_replace_all(DATE, "sum.", ""),
          DATE = as_date(DATE)
        )
      
      write_parquet(df_tmp, paste0("data/processed/CMIP6/municipal/", model, "/", model, "_", temp_year, "_regions_municipal_daily.parquet"))
      # return(df_tmp) # Return dataframe
      gc()
      
    } else {
      # For the rest of the models, import raster data for the three variables
      tmin_rst <-
        rast(
          paste0(
            path,
            model,
            "/historical/tasmin/tasmin_day_",
            model,
            "_",
            temp_year,
            ".nc"
          )
        )
      
      tmax_rst <-
        rast(
          paste0(
            path,
            model,
            "/historical/tasmax/tasmax_day_",
            model,
            "_",
            temp_year,
            ".nc"
          )
        )
      
      prec_rst <-
        rast(
          paste0(
            path,
            model,
            "/historical/pr/pr_day_",
            model,
            "_",
            temp_year,
            ".nc"
          )
        )
    if (model == "HadGEM3-GC31-MM") {
      # Extract the data for each variable first
      tmin_tmp <-
        exact_extract(tmin_rst,
                      spatvector,
                      fun = "mean",
                      append_cols = T
        )
      tmax_tmp <-
        exact_extract(tmax_rst,
                      spatvector,
                      fun = "mean",
                      append_cols = T
        )
      prec_tmp <-
        exact_extract(prec_rst,
                      spatvector,
                      fun = "sum",
                      append_cols = T
        )
      
      # Then set extracted dataframes names as raster timestamps
      # HadGEM3 data, a "names not unique" error was thrown
      names(tmin_tmp)[ncol(spatvector):ncol(tmin_tmp)] <-
        as.character(terra::time(tmin_rst))
      names(tmax_tmp)[ncol(spatvector):ncol(tmax_tmp)] <-
        as.character(terra::time(tmax_rst))
      names(prec_tmp)[ncol(spatvector):ncol(prec_tmp)] <-
        as.character(terra::time(prec_rst))
    } else {
      # Set raster names as timestamps first
      names(tmin_rst) <- terra::time(tmin_rst)
      names(tmax_rst) <- terra::time(tmax_rst)
      names(prec_rst) <- terra::time(prec_rst)
      
      # Then extract the data for each variable
      tmin_tmp <-
        exact_extract(tmin_rst,
                      spatvector,
                      fun = "mean",
                      append_cols = T
        )
      tmax_tmp <-
        exact_extract(tmax_rst,
                      spatvector,
                      fun = "mean",
                      append_cols = T
        )
      prec_tmp <-
        exact_extract(prec_rst,
                      spatvector,
                      fun = "sum",
                      append_cols = T
        )
    }
    
    # Pivot each dataframe to long format
    tmin_tmp <-
      tmin_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
                                names_to = "DATE",
                                values_to = "TMIN"
      )
    tmax_tmp <-
      tmax_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
                                names_to = "DATE",
                                values_to = "TMAX"
      )
    prec_tmp <-
      prec_tmp %>% pivot_longer(!(1:ncol(spatvector) - 1),
                                names_to = "DATE",
                                values_to = "PREC"
      )
    
    
    # Combine everything in a single dataframe
    df_tmp <-
      tibble(
        REGION = tmin_tmp$bsca_region,
        MUNICIPALITY = prec_tmp$name_muni,
        UF = tmin_tmp$name_state,
        UF_2 = tmin_tmp$abbrev_state,
        DATE = tmin_tmp$DATE,
        TMIN = tmin_tmp$TMIN - 273.15,
        TMAX = tmax_tmp$TMAX - 273.15,
        PREC = prec_tmp$PREC * 86400
      ) %>%
      mutate(
        DATE = str_replace_all(DATE, "mean.", ""),
        DATE = str_replace_all(DATE, "sum.", ""),
        DATE = as_date(DATE)
      )
    
    # return(df_tmp) # Return dataframe
    
    arrow::write_parquet(df_tmp, paste0("data/processed/CMIP6/municipal/", model, "/", model, "_", temp_year, "_regions_municipal_daily.parquet"))
    
    gc() # Garbage collect
  }
}