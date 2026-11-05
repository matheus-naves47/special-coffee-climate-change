library(tidyverse)
library(ncdf4)
library(terra)
library(arrow)
library(tictoc)
library(tidyterra)
library(geodata)
library(exactextractr)
library(parallel)

### Import extraction functions and set data path
source("src/extract_by_year_CMIP6.R")
cmip6_path <- "/media/matheus/281A928D1A9257A0/Users/Matheus/Downloads/CMIP6/"

### Import geodata
regions_geoloc <-
  unwrap(read_rds("data/processed/geodata/origin_regions_geodata_agg.rds")) %>% select(bsca_region, name_state, abbrev_state, geometry) %>%
  mutate(name_state = tidyr::replace_na(name_state, "Espírito Santo"),
         abbrev_state = tidyr::replace_na(abbrev_state, "ES"))

municipal_geoloc <- 
  unwrap(read_rds("data/processed/geodata/origin_regions_geodata.rds")) %>% select(bsca_region, name_muni, name_state, abbrev_state, geometry)

### Data extraction from climate grids
year_inds <- seq(1961, 2014)

models <-
  list.dirs(path = cmip6_path,
            full.names = F,
            recursive = F)

# Apply function in parallel to all years of interest, across all models remapped with 1st order conservative method
tic()
for (model in models[9]) {
  assign(
    paste0("df_", model),
    mclapply(1:length(year_inds), function(x)
      extract_by_year_CMIP6(
        x,
        regions_geoloc,
        path = cmip6_path,
        model
      ), mc.cores = 6) %>% bind_rows()
  )

# Export
  write_parquet(
    get(paste0("df_", model)),
    paste0(
      "data/processed/CMIP6/regional/",
      model,
      "_regions_daily.parquet"
    )
  )
  
  rm(list = ls(pattern = model))
  gc()

}
toc()

# Apply function in parallel to all years of interest, across all models remapped with 1st order conservative method, for geodata in municipal level
tic()
for (model in models[9]) {
    lapply(1:length(year_inds), function(x)
      extract_by_year_CMIP6_muni(
        x,
        municipal_geoloc,
        path = cmip6_path,
        model
      )) #%>% bind_rows()
  
  gc()
  
}
toc()