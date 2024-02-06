library(tidyverse)
library(arrow)
library(plotrix)
library(RColorBrewer)


# Color palette to use in the plots
pal <- RColorBrewer::brewer.pal(n=7, "Dark2")

macroregions_br <-
  read_csv("data/geodata/regions_brazil.csv") |> filter(!duplicated(UF)) |>
  rename(UF_2 = UF)

# Import observed (BR-DWGD) data and list models
df_dwgd <-
  read_parquet("data/BR_DWGD_regions_daily.parquet") |> filter(year(DATE) <= 2014)

cmip6_models <-
  list.dirs("data/climate_grids/CMIP6/",
            recursive = F,
            full.names = F)

# Loop to import every model
for (model in cmip6_models) {
  assign(paste0("df_", model),
         read_parquet(paste0("data/", model, "_regions_daily.parquet")) |>
           filter(year(DATE) >= 1961))
  
}

# Loop to import every GoF dataframe
for (model in cmip6_models) {
  assign(paste0(model, "_gof"),
         read_csv(paste0("data/goodness_of_fit/", model, "_gof.csv")))
  
}

# Combine all dataframes in a single one combining all dates in common
data <- df_dwgd |> right_join(`df_BCC-ESM1`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_DWGD", "_BCC_ESM1")) |>
  right_join(`df_CMCC-CM2-SR5`, by = join_by(REGION, UF, UF_2, DATE)) |> 
  right_join(`df_CMCC-ESM2`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_CMCC_CM2_SR5", "_CMCC_ESM2")) |>
  rename(TMIN_CMCC_ESM2 = TMIN, TMAX_CMCC_ESM2 = TMAX) |> 
  right_join(`df_INM-CM4-8`, by = join_by(REGION, UF, UF_2, DATE)) |> 
  right_join(`df_IPSL-CM6A-LR`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_INM_CM4_8", "_IPSL_CM6A_LR")) |> 
  right_join(`df_MRI-ESM2-0`, by = join_by(REGION, UF, UF_2, DATE)) |>
  right_join(`df_HadGEM3-GC31-MM`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_MRI_ESM2_0", "_HadGEM3_GC31_MM"))

rm(list = ls(pattern = "df_")) # Remove dataframes from memory



# Taylor Diagrams ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Tmin
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_BCC_ESM1, normalize = T, col = pal[1], main = "TMIN")
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_CMCC_ESM2, normalize = T, add = T, col = pal[2])
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_HadGEM3_GC31_MM, normalize = T, add = T, col = pal[3])
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_INM_CM4_8, normalize = T, add = T, col = pal[4])
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_IPSL_CM6A_LR, normalize = T, add = T, col = pal[5])
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_MRI_ESM2_0, normalize = T, add = T, col = pal[6])
legend("topright", cmip6_models[c(1,3:7)], pch = 19, col = pal, ncol = 2, inset = c(-0.04,-0.1), cex=.65)

# Tmax
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_BCC_ESM1, normalize = T, col = pal[1], main = "TMAX")
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_CMCC_ESM2, normalize = T, add = T, col = pal[2])
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_HadGEM3_GC31_MM, normalize = T, add = T, col = pal[3])
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_INM_CM4_8, normalize = T, add = T, col = pal[4])
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_IPSL_CM6A_LR, normalize = T, add = T, col = pal[5])
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_MRI_ESM2_0, normalize = T, add = T, col = pal[6])
legend("topright", cmip6_models[c(1,3:7)], pch = 19, col = pal, ncol = 2, inset = c(-0.04,-0.1), cex=.65)

# Prec
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_BCC_ESM1, normalize = T, col = pal[1], main = "PREC")
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_CMCC_CM2_SR5, normalize = T, add = T, col = pal[2])
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_CMCC_ESM2, normalize = T, add = T, col = pal[3])
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_HadGEM3_GC31_MM, normalize = T, add = T, col = pal[4])
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_INM_CM4_8, normalize = T, add = T, col = pal[5])
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_IPSL_CM6A_LR, normalize = T, add = T, col = pal[6])
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_MRI_ESM2_0, normalize = T, add = T, col = pal[7])
legend("topright", cmip6_models, pch = 19, col = pal, ncol = 2, inset = c(-0.04,-0.1), cex=.6)

rm(data)

# Portrait Plots ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Function to create dataframe with specific GoF measures across all models
gof_per_model <- function(model) {
  
  # Put model name as a variable and join with macroregions data to arrange by macroregion
  gof_model <- read_csv(paste0("data/goodness_of_fit/", model, "_gof.csv")) |> 
    mutate(MODEL = paste0(model), .before = REGION) |> left_join(macroregions_br, by = join_by(UF_2)) |>
    relocate(MACROREGION, .before = REGION) |>
    arrange_at(c("MACROREGION", "UF_2"))
  
  # Combine results in a single dataframe
  df_tmp <- bind_rows(gof_model)
  
  return(df_tmp)
  
  gc() # Garbage collect
  
  
}

data <- lapply(cmip6_models, function(x)
  gof_per_model(x)) |> bind_rows()

data$REGION <- factor(data$REGION, levels = unique(data$REGION[order(data$MACROREGION, data$REGION, decreasing = T)]))
data$MODEL <- factor(data$MODEL, levels = unique(data$MODEL[order(data$MACROREGION, data$MODEL, decreasing = F)]))

# Tmin


data |> filter(VARIABLE == "TMIN") |> ggplot(aes(x = MODEL, y = REGION, fill = ME)) +
  geom_tile(colour = 'white') + scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu"))) + 
  labs(title = "Minimum Temperature", subtitle = "Mean Bias (C)") + 
  guides(fill = guide_colorbar(barheight = 15)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.box.margin = margin(5, 5, 0, 0),
        axis.text.x = element_text(hjust = 1,
                                   angle = 30, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

data |> filter(VARIABLE == "TMAX") |> ggplot(aes(x = MODEL, y = REGION, fill = ME)) +
  geom_tile(colour = 'white') + scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu"))) + 
  labs(title = "Maximum Temperature", subtitle = "Mean Bias (C)") + 
  guides(fill = guide_colorbar(barheight = 15)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.box.margin = margin(5, 5, 0, 0),
        axis.text.x = element_text(hjust = 1,
                                   angle = 30, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

data |> filter(VARIABLE == "PREC") |> ggplot(aes(x = MODEL, y = REGION, fill = ME)) +
  geom_tile(colour = 'white') + scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu"))) + 
  labs(title = "Precipitation", subtitle = "Mean Bias (mm)") + 
  guides(fill = guide_colorbar(barheight = 15)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.box.margin = margin(5, 5, 0, 0),
        axis.text.x = element_text(hjust = 1,
                                   angle = 30, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


data |> filter(VARIABLE == "TMIN") |> ggplot(aes(x = MODEL, y = REGION, fill = MAE)) +
  geom_tile(colour = 'black') + scale_fill_gradientn(colours = brewer.pal(9, "Greens")) + 
  labs(title = "Minimum Temperature", subtitle = "Mean Absolute Error (C)") + 
  guides(fill = guide_colorbar(barheight = 15)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.box.margin = margin(5, 5, 0, 0),
        axis.text.x = element_text(hjust = 1,
                                   angle = 30, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

data |> filter(VARIABLE == "TMAX") |> ggplot(aes(x = MODEL, y = REGION, fill = MAE)) +
  geom_tile(colour = 'black') + scale_fill_gradientn(colours = brewer.pal(9, "Greens")) + 
  labs(title = "Maximum Temperature", subtitle = "Mean Absolute Error (C)") + 
  guides(fill = guide_colorbar(barheight = 15)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.box.margin = margin(5, 5, 0, 0),
        axis.text.x = element_text(hjust = 1,
                                   angle = 30, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

data |> filter(VARIABLE == "PREC") |> ggplot(aes(x = MODEL, y = REGION, fill = MAE)) +
  geom_tile(colour = 'black') + scale_fill_gradientn(colours = brewer.pal(9, "Greens")) + 
  labs(title = "Precipitation", subtitle = "Mean Absolute Error (mm)") + 
  guides(fill = guide_colorbar(barheight = 15)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.box.margin = margin(5, 5, 0, 0),
        axis.text.x = element_text(hjust = 1,
                                   angle = 30, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
