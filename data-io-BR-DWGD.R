library(tidyverse)
library(terra)
library(exactextractr)
library(arrow)
library(tictoc)

# Source io function and data path
source("src/extract_by_year_DWGD.R")
data_path <- "../data/"

# Import geodata
regions_geoloc_agg <-
  terra::unwrap(read_rds("data/processed/geodata/origin_regions_geodata_agg.rds")) |>
  select(bsca_region, abbrev_state, name_state, geometry)

regions_geoloc <-
  terra::unwrap(read_rds("data/processed/geodata/origin_regions_geodata.rds")) |>
  select(bsca_region, name_muni, abbrev_state, name_state, geometry)

municipal_geoloc <- geobr::read_municipality(year=2024)
  
# Years for extraction
years <- seq(1961, 2024)

# Apply function in parallel to all years of interest 
# Aggregated data
tictoc::tic()

df_br_dwgd <-
  parallel::mclapply(1:length(years), function(x)
    extract_by_year_DWGD(x, regions_geoloc_agg, years, data_path), mc.cores = 6) |> bind_rows()

tictoc::toc()

arrow::write_parquet(df_br_dwgd, "data/processed/BR_DWGD/regional/BR_DWGD_regions_daily.parquet")

# Municipal data

periods <- seq(1961, 2024, 9)

for (i in 1:(length(periods)-1)) {
  
  if (i == 1) {
    years=seq(periods[i], periods[i+1])
    
    df_dwgd_tmp <-
      parallel::mclapply(1:length(years), function(x)
        extract_by_year_DWGD_muni_regional(x, regions_geoloc, years), mc.cores = 6) |> bind_rows()
    
    arrow::write_parquet(df_dwgd_tmp, paste0("data/processed/BR_DWGD/municipal_data/BR_DWGD_municipal_daily_", periods[i], "_", periods[i+1], ".parquet"))
    
    rm(df_dgwd_tmp)
    gc()
}
  else{
    years <- seq(periods[i]+1, periods[i+1])
    
    df_dwgd_tmp <-
      parallel::mclapply(1:length(years), function(x)
        extract_by_year_DWGD_muni_regional(x, regions_geoloc, years), mc.cores = 6) |> bind_rows()
    
    arrow::write_parquet(df_dwgd_tmp, paste0("data/processed/BR_DWGD/municipal_data/BR_DWGD_municipal_daily_", periods[i]+1, "_", periods[i+1], ".parquet"))
    
    rm(df_dgwd_tmp)
    gc()
  }

  
  
}


tic()

lapply(1:length(years), function(x)
    extract_by_year_DWGD_muni_regional(x, regions_geoloc, years)) #|> bind_rows()

toc()




# Municipal data for the entire country

tic()

lapply(1:length(years), function(x)
  extract_by_year_DWGD_muni(x, municipal_geoloc, years)) #|> bind_rows()

toc()

# Get states in a list
states <- municipal_geoloc$abbrev_state %>% unique()

# # Create folders for each state
# for (state in states) {
#   newdir <- paste0("data/processed/BR_DWGD/municipal/per_state/", state)
#   dir.create(newdir)
# }

dataset <- arrow::open_dataset("data/processed/BR_DWGD/municipal/")



tic()

for (state in states) {
  
  df <- dataset %>% filter(UF_2 == state) %>% collect()
  
  municipalities <- df$MUNICIPALITY %>% unique()
  
  for (municipality in municipalities) {

    df_exp <- df %>% filter(MUNICIPALITY == municipality) %>% arrange(DATE)
    
    xlsx::write.xlsx2(df_exp, paste0("data/processed/BR_DWGD/municipal_per_state/", state, "/BR_DWGD_municipal_", municipality, ".xlsx"))
    
    rm(df_exp)
    gc()
  }
}

toc()



