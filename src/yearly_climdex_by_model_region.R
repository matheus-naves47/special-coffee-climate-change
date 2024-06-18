# Function to calculate yearly climdex indices by model/region
yearly_indices_by_model_region <-
  function(model, region, remapping = NULL) {
    index <- match(model, cmip6_models)
    
    if (!is.null(remapping)) {
      if (model == "CMCC-CM2-SR5") {
        df <-
          get(paste0("df_", model, "_", remapping)) |>
          subset(REGION == region) |> mutate(DATE = as.character(DATE),
                                             TMIN = 0,
                                             TMAX = 0)
        
      }
      
      else {
        df <-
          get(paste0("df_", model, "_", remapping)) |>
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