library(tidyverse)
library(trend)
library(arrow)
library(lubridate)

source("src/trend_per_region.R")

# import data and summarise the variables by year
# BR-DWGD has incomplete data for 2020
df_dwgd <-
  read_parquet("data/processed/BR-DWGD/BR_DWGD_regions_daily.parquet") |>
  subset(year(DATE) != 2020) |> group_by(REGION, UF, UF_2, year(DATE)) |> summarise(TMIN = mean(TMIN),
                                                                                    TMAX = mean(TMAX),
                                                                                    PREC = sum(PREC)) |> rename(YEAR = `year(DATE)`)

# list regions
regions <- df_dwgd$REGION |> unique()


# apply function to all regions
br_dwgd_trends <-
  lapply(1:length(regions), function(x)
    trend_per_region(data = df_dwgd, region = regions[x])) |> bind_rows()

## Export ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

write_csv(br_dwgd_trends, "data/processed/trends/BR-DWGD_trends.csv")
