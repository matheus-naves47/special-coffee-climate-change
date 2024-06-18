library(tidyverse)
library(terra)
library(exactextractr)

# Source io function
source("src/extract_by_year_DWGD.R")

# Import geodata
regions_geoloc <-
  terra::unwrap(read_rds("data/processed/geodata/origin_regions_geodata.rds")) |>
  select(REGION, NAME_1, NAME_3, geometry)


# Years for extraction
years <- seq(1961, 2020)

# Apply function in parallel to all years of interest
tictoc::tic()

df_br_dwgd <-
  parallel::mclapply(1:length(years), function(x)
    extract_by_year_DWGD(x, regions_geoloc, years), mc.cores = 6) |> bind_rows()

tictoc::toc()

arrow::write_parquet(df_br_dwgd, "data/processed/BR-DWGD/BR_DWGD_regions_daily.parquet")