# Function to calculate yearly climdex indices by model/region
yearly_indices_by_model_region <-
  function(model, region) {
    index <- match(model, cmip6_models)
    
    if (model == "BR-DWGD") {
      # For BR-DWGD
      df <-
        get(paste0("df_", model)) |> subset(BSCA_REGION == region) |> mutate(DATE = as.character(DATE))
      
      # Create climdex input
      input <- climdexInput.raw(
        tmax = df$TMAX,
        tmin = df$TMIN,
        prec = df$PR,
        tmax.dates = as.PCICt(df$DATE, cal = "gregorian"),
        tmin.dates = as.PCICt(df$DATE, cal = "gregorian"),
        prec.dates = as.PCICt(df$DATE, cal = "gregorian"),
        northern.hemisphere = FALSE
      )
      
      
      # Create dataframe with the results
      indices_df <- tibble(
        MODEL = model,
        REGION = df$BSCA_REGION[1],
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
    
    if (model == "CMCC-CM2-SR5") {
      df <-
        get(paste0("df_", model)) |>
        subset(REGION == region) |> mutate(DATE = as.character(DATE),
                                           TMIN = 0,
                                           TMAX = 0)
      
    } else {
      df <-
        get(paste0("df_", model)) |>
        subset(REGION == region) |>
        mutate(DATE = as.character(DATE))
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

yearly_indices_by_municipality <-
  function(dataset) {
    
    # df_aux <- dataset %>% filter(MUNICIPALITY_CODE == municipality)
    # 
    # df_aux <- df_aux %>% arrange(DATE) %>%
    #   filter(year(DATE) <= 2023) %>%
    #   mutate(DATE = as.character(DATE))
      
      # Create climdex input
      input <- climdexInput.raw(
        tmax = dataset$TMAX,
        tmin = dataset$TMIN,
        prec = dataset$PR,
        tmax.dates = as.PCICt(dataset$DATE, cal = "gregorian"),
        tmin.dates = as.PCICt(dataset$DATE, cal = "gregorian"),
        prec.dates = as.PCICt(dataset$DATE, cal = "gregorian"),
        northern.hemisphere = FALSE
      )
      
      
      # Create dataframe with the results
      indices_df <- tibble(
        MODEL = "BR-DWGD",
        MUNICIPALITY = dataset$MUNICIPALITY[1],
        MUNICIPALITY_CODE = dataset$MUNICIPALITY_CODE[1],
        UF = dataset$UF[1],
        UF_2 = dataset$UF_2[1],
        YEAR = seq(1961, 2023, 1),
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
      
      rm(dataset)
      gc()
      
      return(indices_df)
      
}
  