# Function to calculate GoF statistics over regions given a model
gof_by_region <- function(index, model, remapping) {
      df_model <-
        read_parquet(
          paste0(
            "data/processed/CMIP6/",
            model,
            "_regions_daily_",
            remapping,
            ".parquet"
          )
        )
  
  if (model == "CMCC-CM2-SR5") {
    # CMCC-CM2-SR5 only has precipitation variable
    # Get simulated and observed (BR-DWGD) data for a specific region
    df_simulated <-
      df_model |> filter(REGION == regions[index])
    df_observed <-
      df_dwgd |> filter(REGION == regions[index]) |> right_join(df_simulated, by = join_by(REGION, UF, UF_2, DATE)) |>
      select(-c(TMIN, TMAX, PREC.y)) |>
      rename(PREC = PREC.x)
    
    # Calculate GoF measures for precipitation
    prec_gof <-
      gof(sim = df_simulated$PREC,
          obs = df_observed$PREC,
          na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
      pivot_wider(names_from = GOF, values_from = V1) |>
      mutate(
        REGION = df_observed$REGION[1],
        UF_2 = df_observed$UF_2[1],
        MODEL = model,
        VARIABLE = "PREC",
        REMAPPING = remapping,
        .before = ME
      )
    
    # Combine results in a single dataframe
    df_tmp <- bind_rows(prec_gof)
    
    return(df_tmp)
    
    gc() # Garbage collect
    
  }
  # For the rest of the models, calculate GoF measures for all three variable
  else {
    # Get simulated (HadGEM3) and observed (BR-DWGD) data for a specific region
    df_simulated <-
      df_model |> filter(REGION == regions[index])
    df_observed <-
      df_dwgd |> filter(REGION == regions[index]) |> right_join(df_simulated, by = join_by(REGION, UF, UF_2, DATE)) |>
      select(-c(TMIN.y, TMAX.y, PREC.y)) |>
      rename(TMIN = TMIN.x,
             TMAX = TMAX.x,
             PREC = PREC.x)
    
    # Calculate GoF measures for each variable
    tmin_gof <-
      gof(sim = df_simulated$TMIN,
          obs = df_observed$TMIN,
          na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
      pivot_wider(names_from = GOF, values_from = V1) |>
      mutate(
        REGION = df_observed$REGION[1],
        UF_2 = df_observed$UF_2[1],
        MODEL = model,
        VARIABLE = "TMIN",
        REMAPPING = remapping,
        .before = ME
      )
    tmax_gof <-
      gof(sim = df_simulated$TMAX,
          obs = df_observed$TMAX,
          na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
      pivot_wider(names_from = GOF, values_from = V1) |>
      mutate(
        REGION = df_observed$REGION[1],
        UF_2 = df_observed$UF_2[1],
        MODEL = model,
        VARIABLE = "TMAX",
        REMAPPING = remapping,
        .before = ME
      )
    prec_gof <-
      gof(sim = df_simulated$PREC,
          obs = df_observed$PREC,
          na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
      pivot_wider(names_from = GOF, values_from = V1) |>
      mutate(
        REGION = df_observed$REGION[1],
        UF_2 = df_observed$UF_2[1],
        MODEL = model,
        VARIABLE = "PREC",
        REMAPPING = remapping,
        .before = ME
      )
    
    # Combine results in a single dataframe
    df_tmp <- bind_rows(tmin_gof, tmax_gof, prec_gof)
    
    return(df_tmp)
    
    gc() # Garbage collect
    
  }
}


# Function to calculate GoF statistics over all regions given a model

gof_by_model <- function(model, remapping) {
  df_model <-
    read_parquet(paste0(
      "data/processed/CMIP6/",
      model,
      "_regions_daily_",
      remapping,
      ".parquet"
    ))
  
  if (model == "CMCC-CM2-SR5") {
    # CMCC-CM2-SR5 only has precipitation variable
    # Get simulated and observed (BR-DWGD) data for a specific region
    df_simulated <- df_model
    df_observed <-
      df_dwgd |> right_join(df_simulated, by = join_by(REGION, UF, UF_2, DATE)) |>
      select(-c(TMIN, TMAX, PREC.y)) |>
      rename(PREC = PREC.x)
    
    # Calculate GoF measures for precipitation
    prec_gof <-
      gof(sim = df_simulated$PREC,
          obs = df_observed$PREC,
          na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
      pivot_wider(names_from = GOF, values_from = V1) |>
      mutate(
        MODEL = model,
        VARIABLE = "PREC",
        REMAPPING = remapping,
        .before = ME
      )
    
    # Combine results in a single dataframe
    df_tmp <- bind_rows(prec_gof)
    
    return(df_tmp)
    
    gc() # Garbage collect
    
  }
  # For the rest of the models, calculate GoF measures for all three variable
  else {
    # Get simulated (HadGEM3) and observed (BR-DWGD) data for a specific region
    df_simulated <- df_model
    df_observed <- df_dwgd |> right_join(df_simulated, by = join_by(REGION, UF, UF_2, DATE)) |>
      select(-c(TMIN.y, TMAX.y, PREC.y)) |>
      rename(TMIN = TMIN.x,
             TMAX = TMAX.x,
             PREC = PREC.x)
    
    # Calculate GoF measures for each variable
    tmin_gof <-
      gof(sim = df_simulated$TMIN,
          obs = df_observed$TMIN,
          na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
      pivot_wider(names_from = GOF, values_from = V1) |>
      mutate(
        MODEL = model,
        VARIABLE = "TMIN",
        REMAPPING = remapping,
        .before = ME
      )
    tmax_gof <-
      gof(sim = df_simulated$TMAX,
          obs = df_observed$TMAX,
          na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
      pivot_wider(names_from = GOF, values_from = V1) |>
      mutate(
        MODEL = model,
        VARIABLE = "TMAX",
        REMAPPING = remapping,
        .before = ME
      )
    prec_gof <-
      gof(sim = df_simulated$PREC,
          obs = df_observed$PREC,
          na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
      pivot_wider(names_from = GOF, values_from = V1) |>
      mutate(
        MODEL = model,
        VARIABLE = "PREC",
        REMAPPING = remapping,
        .before = ME
      )
    
    # Combine results in a single dataframe
    df_tmp <- bind_rows(tmin_gof, tmax_gof, prec_gof)
    
    return(df_tmp)
    
    gc() # Garbage collect
    
  }
}

kge_by_region_climdex <- function(region, model, remapping) {
  df_tmp <- tibble()
  df_model <- df_climdex_cmip6 |> filter(MODEL == model &
                                           REMAPPING == str_to_title(remapping))
  
  # Get simulated and observed (BR-DWGD) climdex indices for a specific region
  df_simulated <-
    df_model |> filter(REGION == region)
  df_observed <-
    df_climdex_dwgd |> filter(REGION == region)
  
  df_tmp <- tibble() # Empty dataframe to receive the calculations
  # Calculate KGE index for each Climdex indices
  for (index in indices) {
    df_aux <- tibble(
      REGION = df_observed$REGION[1],
      MODEL = model,
      INDEX = index,
      REMAPPING = str_to_title(remapping),
      KGE = KGE(
        sim = df_simulated[[index]],
        obs = df_observed[[index]],
        na.rm = T
      )
    )
    
    df_tmp <- bind_rows(df_tmp, df_aux)
  }
  
  return(df_tmp)
  
  gc() # Garbage collect
  
}

bias_table_gen <- function(region, variable, remapping) {
  # Get row with value of ME closest to 0
  bias <- data |> filter(REGION == region &
                           VARIABLE == variable & REMAPPING == remapping)
  bias <- bias |> filter(abs(ME) == min(abs(bias$ME - 0)))
  
  return(bias)
}

gen_latex_table <- function(model, columns, variable) {
  get(paste0(model, "_gof")) |> left_join(macroregions_br, by = join_by(UF_2)) |>
    relocate(MACROREGION, .before = REGION) |>
    arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == variable) |>
    select(all_of(columns)) |> kbl("latex", booktabs = T)
  
}
