library(tidyverse)
library(arrow)
library(climdex.pcic)
library(parallel)


# List models and calendars for models (cdo -sinfon 'grid.ncdf')
cmip6_models <-
  list.dirs("data/climate_grids/CMIP6/",
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
for (remapping in remappings[1]) {
  for (model in cmip6_models) {
    assign(paste0("df_", model, "_", remapping),
           read_parquet(
             paste0(
               "data/regions_data/",
               model,
               "_regions_daily_",
               remapping,
               ".parquet"
             )))
    }
}

# Import BR-DWGD
`df_BR-DWGD` <-
  read_parquet("data/regions_data/BR_DWGD_regions_daily.parquet") |> filter(year(DATE) <= 2014)

# List regions
regions <- df_MIROC6_conservative$REGION |> unique()


# Function to calculate yearly climdex indices by model/region
yearly_indices_by_model_region <-
  function(model, region, remapping = NULL) {
    index <- match(model, cmip6_models)
    
    if (!is.null(remapping)) {
      if (model == "CMCC-CM2-SR5") {
        df <-
          get(paste0("df_", model, "_", remapping)) |> subset(REGION == region) |> mutate(DATE = as.character(DATE),
                                                                                          TMIN = 0,
                                                                                          TMAX = 0)
        
      }
      
      else {
        df <-
          get(paste0("df_", model, "_", remapping)) |> subset(REGION == region) |> mutate(DATE = as.character(DATE))
      }
      
      
      # Create climdex input
      input <- climdexInput.raw(
        tmax = df$TMAX,
        tmin = df$TMIN,
        prec = df$PREC,
        tmax.dates = as.PCICt(df$DATE, cal = calendars[index]),
        tmin.dates = as.PCICt(df$DATE, cal = calendars[index]),
        prec.dates = as.PCICt(df$DATE, cal = calendars[index]),
        northern.hemisphere = FALSE
      )
      
      # Create dataframe with the results
      indices_df <- tibble(
        MODEL = model,
        REMAPPING = str_to_title(remapping),
        REGION = df$REGION[1],
        YEAR = seq(1961, 2014, 1),
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
      ) |> bind_rows()
      
      return(indices_df)
      
    }
    else{
      # For BR-DWGD
      df <-
        get(paste0("df_", model)) |> subset(REGION == region) |> mutate(DATE = as.character(DATE))
      
      # Create climdex input
      input <- climdexInput.raw(
        tmax = df$TMAX,
        tmin = df$TMIN,
        prec = df$PREC,
        tmax.dates = as.PCICt(df$DATE, cal = "gregorian"),
        tmin.dates = as.PCICt(df$DATE, cal = "gregorian"),
        prec.dates = as.PCICt(df$DATE, cal = "gregorian"),
        northern.hemisphere = FALSE
      )
      
      
      # Create dataframe with the results
      indices_df <- tibble(
        MODEL = model,
        REGION = df$REGION[1],
        YEAR = seq(1961, 2014, 1),
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
      ) |> bind_rows()
      
      return(indices_df)
      
    }
    
    
  }


# Function to calculate monthly climdex indices by region
# monthly_indices_by_region <- function(data, region) {
#
#   df <- data |> subset(REGION == region)
#
#   # Create climdex input
#   input <- climdexInput.raw(
#     tmax = df$TMAX,
#     tmin = df$TMIN,
#     prec = df$PREC,
#     tmax.dates = as.PCICt(df$DATE, cal = "gregorian"),
#     tmin.dates = as.PCICt(df$DATE, cal = "gregorian"),
#     prec.dates = as.PCICt(df$DATE, cal = "gregorian"),
#     northern.hemisphere = FALSE)
#
#   dtr <- climdex.dtr(input)
#
#   # Create dataframe with the results
#   indices_df <- tibble(
#     REGION = df$REGION[1],
#     YEAR = substr(names(dtr), 1, 4),
#     MONTH = substr(names(dtr), 6, 7),
#     DTR = dtr,
#     RX1DAY = climdex.rx1day(input),
#     RX5DAY = climdex.rx5day(input),
#     TN10P = climdex.tn10p(input),
#     TN90P = climdex.tn90p(input),
#     TNN = climdex.tnn(input),
#     TNX = climdex.tnx(input),
#     TX10P = climdex.tx10p(input),
#     TX90P = climdex.tx90p(input),
#     TXN = climdex.txn(input),
#     TXX = climdex.txx(input)
#   )
#
# }


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

# Bilinear
for (model in cmip6_models) {
  df_aux <- mclapply(regions, function(x)
    yearly_indices_by_model_region(
      model = model,
      region = x,
      remapping = 'bilinear'
    ), mc.cores = 6) |> bind_rows()
  
  df_climdex_cmip6 <- bind_rows(df_climdex_cmip6, df_aux)
}


# Apply function for all regions on BR-DWGD model
df_climdex_dwgd <- mclapply(regions, function(x)
  yearly_indices_by_model_region(model = "BR-DWGD", region = x), mc.cores = 6) |> bind_rows()


# Export results
write_csv(df_climdex_dwgd, "data/climdex/BR-DWGD_climdex_indices.csv") # feed to goodness_of_fit_tests.R
write_csv(df_climdex_cmip6, "data/climdex/CMIP6_climdex_indices.csv")
# write_csv(monthly_climdex_dwgd, "data/climdex/BR-DWGD_monthly_climdex.csv")
