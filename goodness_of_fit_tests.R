library(tidyverse)
library(arrow)
library(hydroGOF)
library(parallel)
library(kableExtra)
library(plotrix)

# Import data on Brazil's macroregions
macroregions_br <-
  read_csv("data/geodata/regions_brazil.csv") |> filter(!duplicated(UF)) |>
  rename(UF_2 = UF)

# Import observed (BR-DWGD) data and list models
df_dwgd <-
  read_parquet("data/BR_DWGD_regions_daily.parquet") |> filter(year(DATE) <= 2014)

cmip6_models <-
  list.dirs("data/climate_grids/CMIP6/",
            recursive = F,
            full.names = F)[c(1,3:7)]

regions <- df_dwgd$REGION |> unique()


# Function to calculate GoF statistics over all regions given a model
gof_by_region <- function(index, model) {
  df_model <-
    read_parquet(paste0("data/", model, "_regions_daily.parquet")) |>
    filter(year(DATE) >= 1961)
  
  # Get simulated (HadGEM3) and observed (BR-DWGD) data for a specific region
  df_simulated <-
    df_model |> filter(REGION == regions[index])
  df_observed <-
    df_dwgd |> filter(REGION == regions[index]) |> right_join(df_simulated,
                                                              by = join_by(REGION, UF, UF_2, DATE)) |>
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
      VARIABLE = "TMIN",
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
      VARIABLE = "TMAX",
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
      VARIABLE = "PREC",
      .before = ME
    )
  
  # Combine results in a single dataframe
  df_tmp <- bind_rows(tmin_gof, tmax_gof, prec_gof)
  
  return(df_tmp)
  
  gc() # Garbage collect
  
  
}

gof_by_region2 <- function(index, model) {
  df_model <-
    read_parquet(paste0("data/", model, "_regions_daily.parquet")) |>
    filter(year(DATE) >= 1961)
  
  # Get simulated (HadGEM3) and observed (BR-DWGD) data for a specific region
  df_simulated <-
    df_model |> filter(REGION == regions[index])
  df_observed <-
    df_dwgd |> filter(REGION == regions[index]) |> right_join(df_simulated,
                                                              by = join_by(REGION, UF, UF_2, DATE)) |>
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
      VARIABLE = "PREC",
      .before = ME
    )
  
  # Combine results in a single dataframe
  df_tmp <- bind_rows(prec_gof)
  
  return(df_tmp)
  
  gc() # Garbage collect
  
  
}


# Apply function for all regions over all models
for (modl in cmip6_models) {
  assign(paste0(modl, "_gof"),
         lapply(1:length(regions), function(x)
           gof_by_region(x, modl)) |> bind_rows())
  
}

models_gofs <- paste0(cmip6_models, "_gof")

# Export tables
for (model_gof in models_gofs) {
  
  write_csv(get(model_gof), paste0("data/goodness_of_fit/", model_gof, ".csv"))
  
}


# Apply alternate function for all regions over CMCC-CM2-SR5 (contains only precipitation)
assign(paste0("CMCC-CM2-SR5", "_gof"),
         lapply(1:length(regions), function(x)
           gof_by_region2(x, "CMCC-CM2-SR5")) |> bind_rows())


write_csv(`CMCC-CM2-SR5_gof`, "data/goodness_of_fit/CMCC-CM2-SR5_gof.csv")


# Get LaTeX tables for report

measures <- c(2, 3, 5:8, 11, 14, 18, 19, 23, 26) # Selected measures for report

# BCC-ESM1 ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
`BCC-ESM1_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMIN") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`BCC-ESM1_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMAX") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`BCC-ESM1_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "PREC") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

# CMCC-ESM2 --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

`CMCC-ESM2_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMIN") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`CMCC-ESM2_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMAX") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`CMCC-ESM2_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "PREC") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

# CMCC-ESM2 --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

`CMCC-CM2-SR5_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "PREC") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

# HadGEM3-GC31-MM --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

`HadGEM3-GC31-MM_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMIN") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`HadGEM3-GC31-MM_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMAX") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`HadGEM3-GC31-MM_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "PREC") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

# INM-CM4-8 --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

`INM-CM4-8_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMIN") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`INM-CM4-8_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMAX") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`INM-CM4-8_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "PREC") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

# IPSL-CM6A-LR --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

`IPSL-CM6A-LR_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMIN") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`IPSL-CM6A-LR_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMAX") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`IPSL-CM6A-LR_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "PREC") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

# MRI-ESM2-0 --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

`MRI-ESM2-0_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMIN") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`MRI-ESM2-0_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "TMAX") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

`MRI-ESM2-0_gof` |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(MACROREGION, .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == "PREC") |>
  select(all_of(measures)) |> kbl("latex", booktabs = T)

