library(tidyverse)
library(ncdf4)
library(terra)
library(arrow)
library(tictoc)
library(tidyterra)
library(geodata)
library(exactextractr)
library(parallel)

### Import extraction functions
source("src/extract_by_year_CMIP6.R")

### Import geodata
regions_geoloc <-
  unwrap(read_rds("data/processed/geodata/origin_regions_geodata.rds")) %>% select(REGION, NAME_1, NAME_3, geometry)

### Data extraction from climate grids
year_inds <- seq(1961, 2014)

models <-
  list.dirs(path = cmip6_path,
            full.names = F,
            recursive = F)

# Apply function in parallel to all years of interest, across all models remapped with 1st order conservative method
tic()
for (model in models) {
  assign(
    paste0("df_", model),
    mclapply(1:length(year_inds), function(x)
      extract_by_year_CMIP(
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
      "data/processed/CMIP6/",
      model,
      "_regions_daily_conservative.parquet"
    )
  )
  
}

# # Apply function in parallel to all years of interest, across all models remapped with bilinear method
# tic()
# for (model in models) {
#   assign(
#     paste0("df_", model),
#     mclapply(1:length(year_inds), function(x)
#       extract_by_year(
#         x,
#         regions_geoloc,
#         path = cmip6_path,
#         model,
#         remapping = "bilinear"
#       ), mc.cores = 6) %>% bind_rows()
#   )
#   
# }
# toc()
# 
# # Export
# for (model in models) {
#   write_parquet(
#     get(paste0("df_", model)),
#     paste0(
#       "data/regions_data/",
#       model,
#       "_regions_daily_bilinear.parquet"
#     )
#   )
#   
# }
