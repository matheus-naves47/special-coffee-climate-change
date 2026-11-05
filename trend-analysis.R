library(tidyverse)
library(trend)
library(arrow)
library(lubridate)
library(geobr)
library(tictoc)
library(xlsx)

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

## Export 
write_csv(br_dwgd_trends, "data/processed/trends/BR-DWGD_trends.csv")


# Municipal Data ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

df <- open_dataset(
  "data/processed/BR_DWGD/municipal/"
) %>% group_by(MUNICIPALITY_CODE, MUNICIPALITY, UF, UF_2, year(DATE)) |>
  summarise(TMIN = mean(TMIN, na.rm = T),
            TMAX = mean(TMAX, na.rm = T),
            PR = sum(PR, na.rm = T)) |> rename(YEAR = `year(DATE)`) %>% collect() %>% drop_na() %>%
  arrange(MUNICIPALITY_CODE, YEAR)

# df <- df %>% filter(YEAR >= 1990)

municipalities <- df$MUNICIPALITY_CODE %>% unique()

tic()
dwgd_trends_muni <-
  lapply(1:length(municipalities), function(x)
    trend_per_muni(data = df, municipality = municipalities[x])) |> bind_rows()
toc()

dwgd_trends_muni <- dwgd_trends_muni %>% arrange(MUNICIPALITY_CODE)

write.xlsx2(dwgd_trends_muni, "data/processed/BR_DWGD/results/BR_DWGD_trends_municipal.xlsx")
  