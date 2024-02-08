library(tidyverse)
library(arrow)
library(climdex.pcic)

# Read daily data
df_dwgd <-
  read_parquet('data/BR_DWGD_regions_daily.parquet') |>
  subset(year(DATE) != 2020) |>
  mutate(DATE = as.character(DATE))


# List regions 
regions <- df_dwgd$REGION |> unique()

# Function to calculate yearly climdex indices by region
yearly_indices_by_region <- function(data, region) {
  
  df <- data |> subset(REGION == region)
  
  # Create climdex input
  input <- climdexInput.raw(
      tmax = df$TMAX,
      tmin = df$TMIN,
      prec = df$PREC,
      tmax.dates = as.PCICt(df$DATE, cal = "gregorian"),
      tmin.dates = as.PCICt(df$DATE, cal = "gregorian"),
      prec.dates = as.PCICt(df$DATE, cal = "gregorian"),
      northern.hemisphere = FALSE)
  
  # Create dataframe with the results
  indices_df <- tibble(
    REGION = df$REGION[1],
    YEAR = seq(1961,2019,1),
    CDD = climdex.cdd(input),
    CSDI = climdex.csdi(input),
    CWD = climdex.cwd(input),
    DTR = climdex.dtr(input, freq = "annual"),
    FD = climdex.fd(input),
    GSL = climdex.gsl(input),
    ID = climdex.id(input),
    PRCPTOT = climdex.prcptot(input),
    R10MM = climdex.r10mm(input),
    R20MM = climdex.r20mm(input),
    R95P = climdex.r95ptot(input),
    R99P = climdex.r99ptot(input),
    RX1DAY = climdex.rx1day(input, freq = "annual"),
    RX5DAY = climdex.rx5day(input, freq = "annual"),
    SDII = climdex.sdii(input),
    SU = climdex.su(input),
    TN10P = climdex.tn10p(input, freq = "annual"),
    TN90P = climdex.tn90p(input, freq = "annual"),
    TNN = climdex.tnn(input, freq = "annual"),
    TNX = climdex.tnx(input, freq = "annual"),
    TR = climdex.tr(input),
    TX10P = climdex.tx10p(input, freq = "annual"),
    TX90P = climdex.tx90p(input, freq = "annual"),
    TXN = climdex.txn(input, freq = "annual"),
    TXX = climdex.txx(input, freq = "annual"),
    WSDI = climdex.wsdi(input)
  )
  
}


# Function to calculate monthly climdex indices by region
monthly_indices_by_region <- function(data, region) {
  
  df <- data |> subset(REGION == region)
  
  # Create climdex input
  input <- climdexInput.raw(
    tmax = df$TMAX,
    tmin = df$TMIN,
    prec = df$PREC,
    tmax.dates = as.PCICt(df$DATE, cal = "gregorian"),
    tmin.dates = as.PCICt(df$DATE, cal = "gregorian"),
    prec.dates = as.PCICt(df$DATE, cal = "gregorian"),
    northern.hemisphere = FALSE)
  
  dtr <- climdex.dtr(input)
  
  # Create dataframe with the results
  indices_df <- tibble(
    REGION = df$REGION[1],
    YEAR = substr(names(dtr), 1, 4),
    MONTH = substr(names(dtr), 6, 7),
    DTR = dtr,
    RX1DAY = climdex.rx1day(input),
    RX5DAY = climdex.rx5day(input),
    TN10P = climdex.tn10p(input),
    TN90P = climdex.tn90p(input),
    TNN = climdex.tnn(input),
    TNX = climdex.tnx(input),
    TX10P = climdex.tx10p(input),
    TX90P = climdex.tx90p(input),
    TXN = climdex.txn(input),
    TXX = climdex.txx(input)
  )
  
}


# Apply function for all regions
yearly_climdex_dwgd <- lapply(1:length(regions), function(x)
  yearly_indices_by_region(df_dwgd, regions[x])) |> bind_rows()

monthly_climdex_dwgd <- lapply(1:length(regions), function(x)
  monthly_indices_by_region(df_dwgd, regions[x])) |> bind_rows()

# Export results
write_csv(yearly_climdex_dwgd, "data/climdex/BR-DWGD_yearly_climdex.csv")
write_csv(monthly_climdex_dwgd, "data/climdex/BR-DWGD_monthly_climdex.csv")
