# Function to extract each variable by year and output a single dataframe
extract_by_year_DWGD <- function(index, spatvector, years, data_path) {
  temp_year <- years[index] # Year index
  
  # Import raster data
  ETo_rst <-
    rast(paste0(data_path,
                "BR_DWGD/ETo/ETo_BR-DWGD_",
                temp_year,
                ".nc"))
  pr_rst <-
    rast(paste0(data_path,
                "BR_DWGD/pr/pr_BR-DWGD_",
                temp_year,
                ".nc"))
  RH_rst <-
    rast(paste0(data_path,
                "BR_DWGD/RH/RH_BR-DWGD_",
                temp_year,
                ".nc"))
  Rs_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Rs/Rs_BR-DWGD_",
                temp_year,
                ".nc"))
  Tmax_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Tmax/Tmax_BR-DWGD_",
                temp_year,
                ".nc"))
  Tmin_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Tmin/Tmin_BR-DWGD_",
                temp_year,
                ".nc"))
  u2_rst <-
    rast(paste0(data_path,
                "BR_DWGD/u2/u2_BR-DWGD_",
                temp_year,
                ".nc"))
  
  
  # Set raster names as timestamps
  names(ETo_rst) <- terra::time(ETo_rst)
  names(pr_rst) <- terra::time(pr_rst)
  names(RH_rst) <- terra::time(RH_rst)
  names(Rs_rst) <- terra::time(Rs_rst)
  names(Tmax_rst) <- terra::time(Tmax_rst)
  names(Tmin_rst) <- terra::time(Tmin_rst)
  names(u2_rst) <- terra::time(u2_rst)
  
  
  # Extract the data for each variable
  ETo_tmp <-
    exact_extract(ETo_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  pr_tmp <-
    exact_extract(pr_rst,
                  spatvector,
                  fun = "sum",
                  append_cols = T)
  RH_tmp <-
    exact_extract(RH_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Rs_tmp <-
    exact_extract(Rs_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Tmax_tmp <-
    exact_extract(Tmax_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Tmin_tmp <-
    exact_extract(Tmin_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  u2_tmp <-
    exact_extract(u2_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  
  # Pivot each dataframe to long format
  ETo_tmp <-
    ETo_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "ETo") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  pr_tmp <-
    pr_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "PR") |>
    mutate(DATE = str_replace_all(DATE, "sum.", ""))
  
  RH_tmp <-
    RH_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "RH") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Rs_tmp <-
    Rs_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "RS") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Tmax_tmp <-
    Tmax_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMAX") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Tmin_tmp <-
    Tmin_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMIN") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  u2_tmp <-
    u2_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "U2") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))

  
  # Combine everything in a single dataframe
  df_tmp <-
    tibble(
      BSCA_REGION = Tmin_tmp$bsca_region,
      UF = Tmin_tmp$name_state,
      UF_2 = Tmin_tmp$abbrev_state,
      DATE = Tmin_tmp$DATE,
      TMIN = Tmin_tmp$TMIN,
      TMAX = Tmax_tmp$TMAX,
      PR = pr_tmp$PR,
      ETO = ETo_tmp$ETo,
      RH = RH_tmp$RH,
      Rs = Rs_tmp$RS,
      U2 = u2_tmp$U2
    ) |> mutate(DATE = str_replace_all(DATE, "mean.", ""),
                DATE = as_date(DATE))
  
  return(df_tmp) # Return dataframe
  
  gc() # Garbage collect
  
}

extract_by_year_DWGD_muni_regional <- function(index, spatvector, years) {
  temp_year <- years[index] # Year index
  
  # Import raster data
  ETo_rst <-
    rast(paste0(data_path,
                "BR_DWGD/ETo/ETo_BR-DWGD_",
                temp_year,
                ".nc"))
  pr_rst <-
    rast(paste0(data_path,
                "BR_DWGD/pr/pr_BR-DWGD_",
                temp_year,
                ".nc"))
  RH_rst <-
    rast(paste0(data_path,
                "BR_DWGD/RH/RH_BR-DWGD_",
                temp_year,
                ".nc"))
  Rs_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Rs/Rs_BR-DWGD_",
                temp_year,
                ".nc"))
  Tmax_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Tmax/Tmax_BR-DWGD_",
                temp_year,
                ".nc"))
  Tmin_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Tmin/Tmin_BR-DWGD_",
                temp_year,
                ".nc"))
  u2_rst <-
    rast(paste0(data_path,
                "BR_DWGD/u2/u2_BR-DWGD_",
                temp_year,
                ".nc"))
  
  
  # Set raster names as timestamps
  names(ETo_rst) <- terra::time(ETo_rst)
  names(pr_rst) <- terra::time(pr_rst)
  names(RH_rst) <- terra::time(RH_rst)
  names(Rs_rst) <- terra::time(Rs_rst)
  names(Tmax_rst) <- terra::time(Tmax_rst)
  names(Tmin_rst) <- terra::time(Tmin_rst)
  names(u2_rst) <- terra::time(u2_rst)
  
  
  # Extract the data for each variable
  ETo_tmp <-
    exact_extract(ETo_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  pr_tmp <-
    exact_extract(pr_rst,
                  spatvector,
                  fun = "sum",
                  append_cols = T)
  RH_tmp <-
    exact_extract(RH_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Rs_tmp <-
    exact_extract(Rs_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Tmax_tmp <-
    exact_extract(Tmax_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Tmin_tmp <-
    exact_extract(Tmin_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  u2_tmp <-
    exact_extract(u2_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  
  # Pivot each dataframe to long format
  ETo_tmp <-
    ETo_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "ETo") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  pr_tmp <-
    pr_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "PR") |>
    mutate(DATE = str_replace_all(DATE, "sum.", ""))
  
  RH_tmp <-
    RH_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "RH") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Rs_tmp <-
    Rs_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "RS") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Tmax_tmp <-
    Tmax_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMAX") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Tmin_tmp <-
    Tmin_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMIN") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  u2_tmp <-
    u2_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "U2") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  
  # Combine everything in a single dataframe
  df_tmp <-
    tibble(
      MUNICIPALITY = Tmin_tmp$name_muni,
      BSCA_REGION = Tmin_tmp$bsca_region,
      UF = Tmin_tmp$name_state,
      UF_2 = Tmin_tmp$abbrev_state,
      DATE = Tmin_tmp$DATE,
      ETO = ETo_tmp$ETo,
      PR = pr_tmp$PR,
      RH = RH_tmp$RH,
      Rs = Rs_tmp$RS,
      TMAX = Tmax_tmp$TMAX,
      TMIN = Tmin_tmp$TMIN,
      U2 = u2_tmp$U2
    ) |> mutate(DATE = str_replace_all(DATE, "mean.", ""),
                DATE = as_date(DATE))
  
  # return(df_tmp) # Return dataframe
  arrow::write_parquet(df_tmp, paste0("data/processed/BR_DWGD/municipal/BR_DWGD_", temp_year, "_regions_municipal_daily.parquet"))
  rm(df_tmp)
  gc() # Garbage collect
  
}


extract_by_year_DWGD_muni <- function(index, spatvector, years) {
  temp_year <- years[index] # Year index
  
  # Import raster data
  ETo_rst <-
    rast(paste0(data_path,
                "BR_DWGD/ETo/ETo_BR-DWGD_",
                temp_year,
                ".nc"))
  pr_rst <-
    rast(paste0(data_path,
                "BR_DWGD/pr/pr_BR-DWGD_",
                temp_year,
                ".nc"))
  RH_rst <-
    rast(paste0(data_path,
                "BR_DWGD/RH/RH_BR-DWGD_",
                temp_year,
                ".nc"))
  Rs_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Rs/Rs_BR-DWGD_",
                temp_year,
                ".nc"))
  Tmax_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Tmax/Tmax_BR-DWGD_",
                temp_year,
                ".nc"))
  Tmin_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Tmin/Tmin_BR-DWGD_",
                temp_year,
                ".nc"))
  u2_rst <-
    rast(paste0(data_path,
                "BR_DWGD/u2/u2_BR-DWGD_",
                temp_year,
                ".nc"))
  
  
  # Set raster names as timestamps
  names(ETo_rst) <- terra::time(ETo_rst)
  names(pr_rst) <- terra::time(pr_rst)
  names(RH_rst) <- terra::time(RH_rst)
  names(Rs_rst) <- terra::time(Rs_rst)
  names(Tmax_rst) <- terra::time(Tmax_rst)
  names(Tmin_rst) <- terra::time(Tmin_rst)
  names(u2_rst) <- terra::time(u2_rst)
  
  
  # Extract the data for each variable
  ETo_tmp <-
    exact_extract(ETo_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  pr_tmp <-
    exact_extract(pr_rst,
                  spatvector,
                  fun = "sum",
                  append_cols = T)
  RH_tmp <-
    exact_extract(RH_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Rs_tmp <-
    exact_extract(Rs_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Tmax_tmp <-
    exact_extract(Tmax_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Tmin_tmp <-
    exact_extract(Tmin_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  u2_tmp <-
    exact_extract(u2_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  
  # Pivot each dataframe to long format
  ETo_tmp <-
    ETo_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "ETo") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  pr_tmp <-
    pr_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "PR") |>
    mutate(DATE = str_replace_all(DATE, "sum.", ""))
  
  RH_tmp <-
    RH_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "RH") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Rs_tmp <-
    Rs_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "RS") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Tmax_tmp <-
    Tmax_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMAX") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Tmin_tmp <-
    Tmin_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMIN") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  u2_tmp <-
    u2_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "U2") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  
  # Combine everything in a single dataframe
  df_tmp <-
    tibble(
      MUNICIPALITY = Tmin_tmp$name_muni,
      MUNICIPALITY_CODE = Tmin_tmp$code_muni,
      UF = Tmin_tmp$name_state,
      UF_2 = Tmin_tmp$abbrev_state,
      DATE = Tmin_tmp$DATE,
      ETO = ETo_tmp$ETo,
      PR = pr_tmp$PR,
      RH = RH_tmp$RH,
      Rs = Rs_tmp$RS,
      TMAX = Tmax_tmp$TMAX,
      TMIN = Tmin_tmp$TMIN,
      U2 = u2_tmp$U2
    ) |> mutate(DATE = str_replace_all(DATE, "mean.", ""),
                DATE = as_date(DATE))
  
  # return(df_tmp) # Return dataframe
  arrow::write_parquet(df_tmp, paste0("data/processed/BR_DWGD/municipal/BR_DWGD_", temp_year, "_regions_municipal_daily.parquet"))
  rm(df_tmp)
  gc() # Garbage collect
  
}