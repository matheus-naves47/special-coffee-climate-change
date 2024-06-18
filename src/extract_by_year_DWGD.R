data_path <- "data/raw/climate_grids/"

# Function to extract each variable by year and output a single dataframe
extract_by_year_DWGD <- function(index, spatvector, years) {
  temp_year <- years[index] # Year index
  
  # Import raster data
  tmin_rst <-
    rast(paste0(data_path,
                "BR_DWGD/tmin/Tmin_BR-DWGD_",
                temp_year,
                ".nc"))
  tmax_rst <-
    rast(paste0(data_path,
                "BR_DWGD/tmax/Tmax_BR-DWGD_",
                temp_year,
                ".nc"))
  prec_rst <-
    rast(paste0(data_path,
                "BR_DWGD/pr/pr_BR-DWGD_",
                temp_year,
                ".nc"))
  
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
                  fun = "sum",
                  append_cols = T)
  
  # Pivot each dataframe to long format
  tmin_tmp <-
    tmin_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMIN") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  tmax_tmp <-
    tmax_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMAX") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  prec_tmp <-
    prec_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "PREC") |>
    mutate(DATE = str_replace_all(DATE, "sum.", ""))
  
  # Precipitation variable for 2020 is complete.
  # Here, we'll select only january to august to match Tmax and Tmin variables.
  
  if (temp_year == 2020) {  
    prec_tmp <- prec_tmp |> filter(month(DATE) <= 7)
  }
  
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
    ) |> mutate(DATE = str_replace_all(DATE, "mean.", ""),
                DATE = as_date(DATE))
  
  return(df_tmp) # Return dataframe
  
  gc() # Garbage collect
  
}
