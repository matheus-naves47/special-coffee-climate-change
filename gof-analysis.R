library(tidyverse)
library(arrow)
library(hydroGOF)
library(parallel)
library(kableExtra)
library(plotrix)

source("src/gof_functions.R")

# Import data on Brazil's macroregions
macroregions_br <-
  read_csv("data/raw/geodata/regions_brazil.csv") |> filter(!duplicated(UF)) |>
  rename(UF_2 = UF)

# Import observed (BR-DWGD) data and list models
df_dwgd <-
  read_parquet("data/processed/BR-DWGD/BR_DWGD_regions_daily.parquet") |> filter(year(DATE) <= 2014)

cmip6_models <-
  list.dirs("data/raw/climate_grids/CMIP6/",
            recursive = F,
            full.names = F)

regions <- df_dwgd$REGION |> unique()

remappings <- c("conservative", "bilinear")

# Empty dataframe to receive data
df_cmip6_gof_region <- tibble()

# Apply function for all regions over all models remapped with conservative method
for (model in cmip6_models) {
  
  df_aux <- mclapply(1:length(regions), function(x)
    gof_by_region(x, model, remapping = 'conservative'), mc.cores = 6) |> bind_rows()
  
  df_cmip6_gof_region <- bind_rows(df_cmip6_gof_region, df_aux)
  
}

# # Apply function for all regions over all models remapped with bilinear method
# # No multimodel ensemble for bilinear interpolation method
# for (model in cmip6_models) {
#   
#   df_aux <- mclapply(1:length(regions), function(x)
#     gof_by_region(x, model, remapping = 'bilinear'), mc.cores = 6) |> bind_rows()
#   
#   df_cmip6_gof_region <- bind_rows(df_cmip6_gof_region, df_aux)
#   
# }


# df_cmip6_gof_model <- tibble()

# # Apply function across all models, bilinear and 1st order conservative interpolated
# for (remap in remappings) {
# 
#   df_aux <- mclapply(cmip6_models, function(x) gof_by_model(x, remapping = remap), mc.cores = 6) |>  bind_rows()
# 
#   df_cmip6_gof_model <- bind_rows(df_cmip6_gof_model, df_aux)
# 
# }
df_cmip6_gof_model <- mclapply(cmip6_models, function(x) gof_by_model(x, remapping = "conservative"), mc.cores = 6) |>  bind_rows()


# Export tables
write_csv(df_cmip6_gof_region, 'data/processed/gof/CMIP6_gof_region.csv')
write_csv(df_cmip6_gof_model, 'data/processed/gof/CMIP6_gof_model.csv')


# LaTeX Tables ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# columns <- c(2, 3, 5:8, 11, 14, 18, 19, 23, 26) # Selected columns for report
# 
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
df_climdex_dwgd <- read_csv("data/processed/climdex/BR-DWGD_climdex_indices.csv") # from climdex.R
df_climdex_cmip6 <- read_csv("data/processed/climdex/CMIP6_climdex_indices.csv")

# list indices
indices <- names(df_climdex_cmip6[5:30])

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

# # Apply over regions for all models using bilinear remapping
# for (model in cmip6_models) {
#   df_aux <- mclapply(regions, function(x)
#     kge_by_region_climdex(
#       region = x,
#       model = model,
#       remapping = 'bilinear'
#     ), mc.cores = 6) |> bind_rows()
#   
#   df_climdex_kge <- bind_rows(df_climdex_kge, df_aux)
# }

# Export
write_csv(df_climdex_kge, "data/processed/climdex/CMIP6_climdex_kge.csv") # feed to visualizations.R


# # Specific Demands -------------------------------------------------------------------------------------------------------------------------------
# 
# # Get the model with the ME value closest to 0 per region and variable
# # List CMIP6 models
# cmip6_models <-
#   list.dirs("data/climate_grids/CMIP6/",
#             recursive = F,
#             full.names = F)[c(1:8, 10, 11)]
# 
# data <- read_csv("data/goodness_of_fit/CMIP6_gof.csv") |> select(c(1:6))
# 
# regions <- unique(data$REGION)
# remappings <- c("bilinear", "conservative")
# # remapping <- remappings[2]
# 
# bias_table_gen <- function(region, variable, remapping) {
#   # Get row with value of ME closest to 0
#   bias <- data |> filter(REGION == region & VARIABLE == variable & REMAPPING == remapping) 
#   bias <- bias |> filter(abs(ME) == min(abs(bias$ME - 0)))
#   
#   return(bias)
# }
# 
# variables <- c("TMIN", "TMAX", "PREC")
# 
# # Empty dataframe to receive data
# df_bias <- tibble()
# 
# for (variable in variables) {
#   
#   df_aux <- lapply(regions, function(x) bias_table_gen(x, variable = variable, remapping = "conservative")) |> bind_rows()
#   
#   df_bias <- bind_rows(df_bias, df_aux)
#   
# }
# 
# for (variable in variables) {
#   
#   df_aux <- lapply(regions, function(x) bias_table_gen(x, variable = variable, remapping = "bilinear")) |> bind_rows()
#   
#   df_bias <- bind_rows(df_bias, df_aux)
#   
# }
# 
# df_bias <- df_bias |> rename(REGIAO = REGION, UF = UF_2, MODELO = MODEL, VARIAVEL = VARIABLE, REMAPEAMENTO = REMAPPING)
# 
# write_csv(df_bias, 'data/goodness_of_fit/optimal_models_per_bias.csv')
# 
# 
# # Verify if HadGEM3-GC31-MM performs better than multimodel ensemble
# 
# df_had_mm <- df_climdex_kge |> filter(MODEL %in% c("HadGEM3-GC31-MM", "MM-ENSEMBLE"))
