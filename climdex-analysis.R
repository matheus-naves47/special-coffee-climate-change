library(tidyverse)
library(arrow)
library(climdex.pcic)
library(parallel)
library(tictoc)

# data_path <- "/media/matheus/281A928D1A9257A0/Users/Matheus/Downloads/CMIP6/"
source("src/yearly_climdex_by_model_region.R")

# List models and calendars for models (cdo -sinfon 'grid.ncdf')
cmip6_models <-
  list.dirs(data_path, recursive = F, full.names = F)

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
    "proleptic_gregorian",
    "proleptic_gregorian"
  )

# Loop to import every model
for (model in cmip6_models) {
  assign(paste0("df_", model), read_parquet(
    paste0(
      "data/processed/CMIP6/regional/",
      model,
      "_regions_daily.parquet"
    )
  ))
}

# Import BR-DWGD
`df_BR-DWGD` <-
  read_parquet("data/processed/BR_DWGD/regional/BR_DWGD_regions_daily.parquet") |> filter(year(DATE) <= 2014)

# List regions
regions <- `df_BR-DWGD`$BSCA_REGION |> unique()


# Apply function for all regions across all models and put it in a single dataframe
df_climdex_cmip6 <- tibble()

# Conservative
for (model in cmip6_models) {
  df_aux <- mclapply(regions, function(x)
    yearly_indices_by_model_region(model = model, region = x), mc.cores = 6) |> bind_rows()
  
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
write_csv(df_climdex_dwgd,
          "data/processed/climdex/BR-DWGD_climdex_indices.csv") # feed to goodness_of_fit_tests.R
write_csv(df_climdex_cmip6,
          "data/processed/climdex/CMIP6_climdex_indices.csv")


# Municipal data for the entire country

df_dwgd <- open_dataset("data/processed/BR_DWGD/municipal/")

geodata <- read_csv("data/raw/municipalities_2024_no_NA.csv")

states <- unique(geodata$UF_2)


# for (j in seq_along(states)){
#   folder<-dir.create(paste0("data/processed/climdex/municipal_per_state/", states[j]))}

tic()

for (state in states) {
  df_state <- df_dwgd %>% filter(UF_2 == state) %>% collect()
  
  municipalities <- (geodata %>% filter(UF_2 == state))$MUNICIPALITY_CODE
  
  for (muni in municipalities) {
    df_muni <- df_state %>% filter(MUNICIPALITY_CODE == muni)
    
    df_muni <- df_muni %>% arrange(DATE) %>%
      filter(year(DATE) <= 2023) %>%
      mutate(DATE = as.character(DATE))
    
    df_climdex_muni <- yearly_indices_by_municipality(df_muni)
    
    write_csv(
      df_climdex_muni,
      paste0(
        "data/processed/climdex/municipal_per_state/",
        state,
        "/climdex_municipal_",
        muni,
        "_1961_2023.csv"
      )
    )
    
    rm(df_climdex_muni, df_muni)
    gc()
  }
  
  rm(df_state)
  gc()
}

toc()

for (state in states) {
  
  df_joined <- open_csv_dataset(paste0("data/processed/climdex/municipal_per_state/", state, "/")) %>% 
    collect()
  
  write_csv(df_joined, paste0("data/processed/climdex/municipal_per_state/climdex_indices_municipal_", state, ".csv"))
  
}
