library(tidyverse)
library(arrow)
library(hydroGOF)
library(parallel)
library(kableExtra)
library(plotrix)

# Import data on Brazil's macroregions
macroregions_br <-
  read_csv("data/geodata/regions_brazil.csv") |> filter(!duplicated(UF)) |>
  rename(UF_2 = UF)

# Import observed (BR-DWGD) data and list models
df_dwgd <-
  read_parquet("data/regions_data/BR_DWGD_regions_daily.parquet") |> filter(year(DATE) <= 2014)

cmip6_models <-
  list.dirs("data/climate_grids/CMIP6/",
            recursive = F,
            full.names = F)

regions <- df_dwgd$REGION |> unique()

# Function to calculate GoF statistics over all regions given a model
gof_by_region <- function(index, model, remapping) {
  
  df_model <-
    read_parquet(paste0("data/regions_data/", model, "_regions_daily_", remapping, ".parquet"))
  
  if (model == "CMCC-CM2-SR5") {
    
    # CMCC-CM2-SR5 only has precipitation variable
    # Get simulated and observed (BR-DWGD) data for a specific region
    df_simulated <-
      df_model |> filter(REGION == regions[index])
    df_observed <-
      df_dwgd |> filter(REGION == regions[index]) |> right_join(df_simulated,
                                                                by = join_by(REGION, UF, UF_2, DATE)) |>
      select(-c(TMIN, TMAX, PREC.y)) |>
      rename(PREC = PREC.x)
    
    # Calculate GoF measures for precipitation
    prec_gof <-
      gof(sim = df_simulated$PREC,
          obs = df_observed$PREC,
          na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
      pivot_wider(names_from = GOF, values_from = V1) |>
      mutate(
        REGION = df_observed$REGION[1],
        UF_2 = df_observed$UF_2[1],
        MODEL = model,
        VARIABLE = "PREC",
        REMAPPING = remapping,
        .before = ME
      )
    
    # Combine results in a single dataframe
    df_tmp <- bind_rows(prec_gof)
    
    return(df_tmp)
    
    gc() # Garbage collect
    
  }
  # For the rest of the models, calculate GoF measures for all three variable
  else {
  # Get simulated (HadGEM3) and observed (BR-DWGD) data for a specific region
  df_simulated <-
    df_model |> filter(REGION == regions[index])
  df_observed <-
    df_dwgd |> filter(REGION == regions[index]) |> right_join(df_simulated,
                                                              by = join_by(REGION, UF, UF_2, DATE)) |>
    select(-c(TMIN.y, TMAX.y, PREC.y)) |>
    rename(TMIN = TMIN.x,
           TMAX = TMAX.x,
           PREC = PREC.x)
  
  # Calculate GoF measures for each variable
  tmin_gof <-
    gof(sim = df_simulated$TMIN,
        obs = df_observed$TMIN,
        na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
    pivot_wider(names_from = GOF, values_from = V1) |>
    mutate(
      REGION = df_observed$REGION[1],
      UF_2 = df_observed$UF_2[1],
      MODEL = model,
      VARIABLE = "TMIN",
      REMAPPING = remapping,
      .before = ME
    )
  tmax_gof <-
    gof(sim = df_simulated$TMAX,
        obs = df_observed$TMAX,
        na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
    pivot_wider(names_from = GOF, values_from = V1) |>
    mutate(
      REGION = df_observed$REGION[1],
      UF_2 = df_observed$UF_2[1],
      MODEL = model,
      VARIABLE = "TMAX",
      REMAPPING = remapping,
      .before = ME
    )
  prec_gof <-
    gof(sim = df_simulated$PREC,
        obs = df_observed$PREC,
        na.rm = TRUE) |> as_tibble(rownames = pkgconfig::get_config("tibble::rownames", "GOF")) |>
    pivot_wider(names_from = GOF, values_from = V1) |>
    mutate(
      REGION = df_observed$REGION[1],
      UF_2 = df_observed$UF_2[1],
      MODEL = model,
      VARIABLE = "PREC",
      REMAPPING = remapping,
      .before = ME
    )
  
  # Combine results in a single dataframe
  df_tmp <- bind_rows(tmin_gof, tmax_gof, prec_gof)
  
  return(df_tmp)
  
  gc() # Garbage collect
  
  } 
}


# Empty dataframe to receive data
df_cmip6_gof <- tibble()

# Apply function for all regions over all models remapped with conservative method
for (model in cmip6_models) {
  
  df_aux <- mclapply(1:length(regions), function(x)
           gof_by_region(x, model, remapping = 'conservative'), mc.cores = 6) |> bind_rows()
  
  df_cmip6_gof <- bind_rows(df_cmip6_gof, df_aux)
  
}

# Apply function for all regions over all models remapped with bilinear method
# No multimodel ensemble for bilinear interpolation method
for (model in cmip6_models) {
  
  df_aux <- mclapply(1:length(regions), function(x)
    gof_by_region(x, model, remapping = 'bilinear'), mc.cores = 6) |> bind_rows()
  
  df_cmip6_gof <- bind_rows(df_cmip6_gof, df_aux)
  
}

# Export tables
write_csv(df_cmip6_gof, 'data/goodness_of_fit/CMIP6_gof.csv')


# LaTeX Tables ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# columns <- c(2, 3, 5:8, 11, 14, 18, 19, 23, 26) # Selected columns for report
# 
# # Function to generate LaTeX tables given model, columns and variable
# gen_latex_table <- function(model, columns, variable) {
#   get(paste0(model, "_gof")) |> left_join(macroregions_br, by = join_by(UF_2)) |>
#     relocate(MACROREGION, .before = REGION) |>
#     arrange_at(c("MACROREGION", "UF_2")) |> filter(VARIABLE == variable) |>
#     select(all_of(columns)) |> kbl("latex", booktabs = T)
#   
# }
# 
# # Put tables in a list
# tmin_tables <- list()
# tmax_tables <- list()
# prec_tables <- list()
# 
# for (model in cmip6_models) {
#   tmin_tables[model] <- gen_latex_table(model, columns, variable = "TMIN")
#   tmax_tables[model] <- gen_latex_table(model, columns, variable = "TMAX")
#   prec_tables[model] <- gen_latex_table(model, columns, variable = "PREC")
#   
# }
# 
# # Export tables
# lapply(tmin_tables, write, "data/goodness_of_fit/tmin_tables_conservative.txt", append=TRUE)
# lapply(tmax_tables, write, "data/goodness_of_fit/tmax_tables_conservative.txt", append=TRUE)
# lapply(prec_tables, write, "data/goodness_of_fit/prec_tables_conservative.txt", append=TRUE)
# 
# # Export tables
# lapply(tmin_tables, write, "data/goodness_of_fit/tmin_tables_bilinear.txt", append=TRUE)
# lapply(tmax_tables, write, "data/goodness_of_fit/tmax_tables_bilinear.txt", append=TRUE)
# lapply(prec_tables, write, "data/goodness_of_fit/prec_tables_bilinear.txt", append=TRUE)



# Climdex GoF ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_climdex_dwgd <- read_csv("data/climdex/BR-DWGD_climdex_indices.csv") # from climdex.R
df_climdex_cmip6 <- read_csv("data/climdex/CMIP6_climdex_indices.csv")

# list indices
indices <- names(df_climdex_cmip6[5:30])

# Function to calculate KGE index for each Climdex indices, given region, model and remapping method used
kge_by_region_climdex <- function(region, model, remapping) {
  
  df_tmp <- tibble()
  df_model <- df_climdex_cmip6 |> filter(MODEL == model & REMAPPING == str_to_title(remapping))

    # Get simulated and observed (BR-DWGD) climdex indices for a specific region
    df_simulated <-
      df_model |> filter(REGION == region)
    df_observed <-
      df_climdex_dwgd |> filter(REGION == region)
    
    df_tmp <- tibble() # Empty dataframe to receive the calculations
    # Calculate KGE index for each Climdex indices
    for (index in indices) {
      df_aux <- tibble(
        REGION = df_observed$REGION[1],
        MODEL = model,
        INDEX = index,
        REMAPPING = str_to_title(remapping),
        KGE = KGE(sim = df_simulated[[index]], obs = df_observed[[index]], na.rm = T)
      )
      
      df_tmp <- bind_rows(df_tmp, df_aux)
    }
    
    return(df_tmp)
    
    gc() # Garbage collect
    
}


# Empty dataframe to receive data
df_climdex_kge <- tibble()

# Apply over regions for all models using conservative remapping 
for (model in cmip6_models) {
  df_aux <- mclapply(regions, function(x)
    kge_by_region_climdex(
      region = x,
      model = model,
      remapping = 'conservative'
    ), mc.cores = 6) |> bind_rows()
  
  df_climdex_kge <- bind_rows(df_climdex_kge, df_aux)
}

# Apply over regions for all models using bilinear remapping
for (model in cmip6_models) {
  df_aux <- mclapply(regions, function(x)
    kge_by_region_climdex(
      region = x,
      model = model,
      remapping = 'bilinear'
    ), mc.cores = 6) |> bind_rows()
  
  df_climdex_kge <- bind_rows(df_climdex_kge, df_aux)
}

# Export
write_csv(df_climdex_kge, "data/climdex/CMIP6_climdex_kge.csv") # feed to visualizations.R


# Specific Demands -------------------------------------------------------------------------------------------------------------------------------

# Get the model with the ME value closest to 0 per region and variable
# List CMIP6 models
cmip6_models <-
  list.dirs("data/climate_grids/CMIP6/",
            recursive = F,
            full.names = F)[c(1:8, 10, 11)]

data <- read_csv("data/goodness_of_fit/CMIP6_gof.csv") |> select(c(1:6))

regions <- unique(data$REGION)
remappings <- c("bilinear", "conservative")
# remapping <- remappings[2]

bias_table_gen <- function(region, variable, remapping) {
  # Get row with value of ME closest to 0
  bias <- data |> filter(REGION == region & VARIABLE == variable & REMAPPING == remapping) 
  bias <- bias |> filter(abs(ME) == min(abs(bias$ME - 0)))
  
  return(bias)
}

variables <- c("TMIN", "TMAX", "PREC")

# Empty dataframe to receive data
df_bias <- tibble()

for (variable in variables) {
  
  df_aux <- lapply(regions, function(x) bias_table_gen(x, variable = variable, remapping = "conservative")) |> bind_rows()
  
  df_bias <- bind_rows(df_bias, df_aux)
  
}

for (variable in variables) {
  
  df_aux <- lapply(regions, function(x) bias_table_gen(x, variable = variable, remapping = "bilinear")) |> bind_rows()
  
  df_bias <- bind_rows(df_bias, df_aux)
  
}

df_bias <- df_bias |> rename(REGIAO = REGION, UF = UF_2, MODELO = MODEL, VARIAVEL = VARIABLE, REMAPEAMENTO = REMAPPING)

write_csv(df_bias, 'data/goodness_of_fit/optimal_models_per_bias.csv')
