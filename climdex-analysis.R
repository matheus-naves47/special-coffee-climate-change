library(tidyverse)
library(arrow)
library(climdex.pcic)
library(parallel)

source("src/yearly_climdex_by_model_region.R")

# List models and calendars for models (cdo -sinfon 'grid.ncdf')
cmip6_models <-
  list.dirs("data/raw/climate_grids/CMIP6/",
            recursive = F,
            full.names = F)

calendars <-
  c(
    "365_day",
    "365_day",
    "365_day",
    "365_day",
    "360_day",
    "365_day",
    "gregorian",
    "gregorian",
    "365_day",
    "proleptic_gregorian",
    "proleptic_gregorian"
  )

remappings <- c("conservative", "bilinear")

# Loop to import every model
for (remapping in remappings) {
  for (model in cmip6_models) {
    assign(paste0("df_", model, "_", remapping),
           read_parquet(
             paste0(
               "data/processed/CMIP6/",
               model,
               "_regions_daily_",
               remapping,
               ".parquet"
             )))
  }
}

# Import BR-DWGD
`df_BR-DWGD` <-
  read_parquet("data/processed/BR-DWGD/BR_DWGD_regions_daily.parquet") |> filter(year(DATE) <= 2014)

# List regions
regions <- `df_BR-DWGD`$REGION |> unique()


# Apply function for all regions across all models and put it in a single dataframe
df_climdex_cmip6 <- tibble()

# Conservative
for (model in cmip6_models) {
  df_aux <- mclapply(regions, function(x)
    yearly_indices_by_model_region(
      model = model,
      region = x,
      remapping = 'conservative'
    ), mc.cores = 6) |> bind_rows()
  
  df_climdex_cmip6 <- bind_rows(df_climdex_cmip6, df_aux)
}

# # Bilinear
# for (model in cmip6_models) {
#   df_aux <- mclapply(regions, function(x)
#     yearly_indices_by_model_region(
#       model = model,
#       region = x,
#       remapping = 'bilinear'
#     ), mc.cores = 6) |> bind_rows()
#   
#   df_climdex_cmip6 <- bind_rows(df_climdex_cmip6, df_aux)
# }

# Apply function for all regions on BR-DWGD model
df_climdex_dwgd <- mclapply(regions, function(x)
  yearly_indices_by_model_region(model = "BR-DWGD", region = x), mc.cores = 6) |> bind_rows()


# Export results
write_csv(df_climdex_dwgd, "data/processed/climdex/BR-DWGD_climdex_indices.csv") # feed to goodness_of_fit_tests.R
write_csv(df_climdex_cmip6, "data/processed/climdex/CMIP6_climdex_indices.csv")
