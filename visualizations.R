library(tidyverse)
library(arrow)
library(plotrix)
library(ggsci)
library(RColorBrewer)


# Color palette to use in the plots
pal <- pal_npg("nrc", alpha = 1)(10)

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


# remapping <- "conservative"
remapping <- "bilinear"

# Loop to import every model and GoF dataframe
for (model in cmip6_models) {
  
  assign(paste0("df_", model),
         read_parquet(paste0("data/regions_data/", model, "_regions_daily_", remapping, ".parquet")))
  
  assign(paste0(model, "_gof"),
         read_csv(paste0("data/goodness_of_fit/", model, "_gof_", remapping, ".csv")))
  
}

# Combine all dataframes in a single one combining all dates in common
data <- df_dwgd |> right_join(`df_IPSL-CM6A-LR`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_DWGD", "_IPSL_CM6A_LR")) |> 
  right_join(df_MIROC6, by = join_by(REGION, UF, UF_2, DATE)) |> 
  right_join(`df_MPI-ESM1-2-HR`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_MIROC6", "_MPI_ESM1_2_HR")) |> 
  right_join(`df_MRI-ESM2-0`, by = join_by(REGION, UF, UF_2, DATE)) |> 
  right_join(`df_BCC-ESM1`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_MRI_ESM2_0", "_BCC_ESM1")) |> 
  right_join(`df_CMCC-CM2-SR5`, by = join_by(REGION, UF, UF_2, DATE)) |> rename(PREC_CMCC_CM2_SR5 = PREC) |> 
  right_join(`df_CMCC-ESM2`, by = join_by(REGION, UF, UF_2, DATE)) |> 
  right_join(`df_GFDL-ESM4`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_CMCC_ESM2", "_GFDL_ESM4")) |> 
  right_join(`df_INM-CM4-8`, by = join_by(REGION, UF, UF_2, DATE)) |> 
  right_join(`df_HadGEM3-GC31-MM`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_INM_CM4_8", "_HadGEM3_GC31_MM"))

rm(list = ls(pattern = "df_")) # Remove dataframes from memory


# Taylor Diagrams ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Tmin
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_BCC_ESM1, normalize = T, col = pal[1], pch=19, main = "TMIN", pcex=1.5)

for (i in c(3:10)) {
  taylor.diagram(ref = data$TMIN_DWGD, model = data[[str_replace_all(paste0("TMIN_", cmip6_models[i]), "-", "_")]], normalize = T, col = pal[i], pch = i, pcex = 1.5, add = TRUE)
  
}
legend("topright", cmip6_models[c(1,3:10)], pch=c(19, 3:10), col = pal[c(1,3:10)], ncol = 1, inset = c(-0.055,-0.125), cex=.8, pt.cex = c(rep(1.5,5), rep(1.25,3), 1.5), text.width = 0.325)



# Tmax
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMAX_BCC_ESM1, normalize = T, col = pal[1], pch=19, main = "TMAX", pcex=1.5)

for (i in c(3:10)) {
  taylor.diagram(ref = data$TMIN_DWGD, model = data[[str_replace_all(paste0("TMAX_", cmip6_models[i]), "-", "_")]], normalize = T, col = pal[i], pch = i, pcex = 1.5, add = TRUE)
  
}
legend("topright", cmip6_models[c(1,3:10)], pch=c(19, 3:10), col = pal[c(1,3:10)], ncol = 1, inset = c(-0.075,-0.125), cex=.8, pt.cex = c(rep(1.5,5), rep(1.25,3), 1.5), text.width = 0.4)


# Prec
taylor.diagram(ref = data$TMIN_DWGD, model = data$PREC_BCC_ESM1, normalize = T, col = pal[1], pch=19, main = "PREC", pcex=1.5)

for (i in c(2:10)) {
  taylor.diagram(ref = data$TMIN_DWGD, model = data[[str_replace_all(paste0("PREC_", cmip6_models[i]), "-", "_")]], normalize = T, col = pal[i], pch = i, pcex = 1.5, add = TRUE)
  
}
legend("topright", cmip6_models[c(1:10)], pch=c(19, 2:10), col = pal, ncol = 1, inset = c(-0.05,-0.09), cex=.8, pt.cex = c(rep(1.5,5), rep(1.25,4), 1.5), text.width = 0.9)

rm(data)

# Portrait Plots ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Function to create dataframe with specific GoF measures across all models
gof_per_model <- function(model, remapping) {
  
  # Put model name as a variable and join with macroregions data to arrange by macroregion
  gof_model <- read_csv(paste0("data/goodness_of_fit/", model, "_gof_", remapping, ".csv")) |> 
    mutate(MODEL = paste0(model), .before = REGION) |> left_join(macroregions_br, by = join_by(UF_2)) |>
    relocate(MACROREGION, .before = REGION) |>
    arrange_at(c("MACROREGION", "UF_2"))
  
  # Combine results in a single dataframe
  df_tmp <- bind_rows(gof_model)
  
  return(df_tmp)
  
  gc() # Garbage collect
  
  
}

data <- lapply(cmip6_models, function(x)
  gof_per_model(x, remapping)) |> bind_rows()

data$REGION <- factor(data$REGION, levels = unique(data$REGION[order(data$MACROREGION, data$REGION, decreasing = T)]))
data$MODEL <- factor(data$MODEL, levels = unique(data$MODEL[order(data$MACROREGION, data$MODEL, decreasing = F)]))
data$VARIABLE <- factor(data$VARIABLE, levels = c("TMIN", "TMAX", "PREC"))

# Mean Error Portraits

tmp_plot <-
  data |> ggplot(aes(x = MODEL, y = REGION, fill = ME)) +
  geom_tile(colour = 'black') + scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu"))) +
  labs(title = "Mean Bias (ME)") +
  guides(fill = guide_colorbar(barheight = 15)) + theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.box.margin = margin(5, 5, 0, 0),
    axis.text.x = element_text(
      hjust = 1,
      angle = 90,
      face = "bold"
    ),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_blank()
  ) + facet_grid( ~ VARIABLE)


# show(tmp_plot)
ggsave(tmp_plot, device = 'eps', file=paste0("plots/mean_bias_", remapping, ".eps"), width = 8, height = 7, units = 'in', dpi = 600)

# Mean Average Error Portraits
tmp_plot <-
  data |> ggplot(aes(x = MODEL, y = REGION, fill = MAE)) +
  geom_tile(colour = 'black') + scale_fill_gradientn(colours = brewer.pal(9, "Greens")) +
  labs(title = "Mean Absolute Error (MAE)") +
  guides(fill = guide_colorbar(barheight = 15)) + theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.box.margin = margin(5, 5, 0, 0),
    axis.text.x = element_text(
      hjust = 1,
      angle = 90,
      face = "bold"
    ),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_blank()
  ) + facet_grid( ~ VARIABLE)


ggsave(tmp_plot, device = 'eps', file = paste0("plots/mae_", remapping, ".eps"), width = 8, height = 7, units = 'in', dpi = 600)

# show(tmp_plot)
rm(tmp_plot)


# # Plots v1

# variables <- c("TMIN", "TMAX", "PREC")
# labels <- c("Minimum Temperature", "Maximum Temperature", "Precipitation")

# for (i in c(1,2,3)) {
#   # Mean Error Portraits
#   
#   tmp_plot <- data |> filter(VARIABLE == variables[i]) |> ggplot(aes(x = MODEL, y = REGION, fill = ME)) +
#     geom_tile(colour = 'black') + scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu"))) + 
#     labs(title = labels[i], subtitle = "Mean Bias (C)") + 
#     guides(fill = guide_colorbar(barheight = 15)) + theme_bw() +
#     theme(axis.title.x = element_blank(),
#           axis.title.y = element_blank(),
#           legend.title = element_blank(),
#           legend.box.margin = margin(5, 5, 0, 0),
#           axis.text.x = element_text(hjust = 1,
#                                      angle = 30, face = "bold"),
#           axis.text.y = element_text(face = "bold"),
#           plot.title = element_text(hjust = 0.5),
#           plot.subtitle = element_text(hjust = 0.5),
#           panel.border = element_blank())
#   
#   show(tmp_plot)
#   # ggsave(tmp_plot, device = 'eps', file=paste0("plots/mean_bias_", str_to_lower(variables[i]), "_", remapping, ".eps"), width = 5, height = 7, units = 'in', dpi = 600)
#   
#   # Mean Average Error Portraits
#   tmp_plot <- data |> filter(VARIABLE == variables[i]) |> ggplot(aes(x = MODEL, y = REGION, fill = MAE)) +
#     geom_tile(colour = 'black') + scale_fill_gradientn(colours = brewer.pal(9, "Greens")) + 
#     labs(title = labels[i], subtitle = "Mean Absolute Error (C)") + 
#     guides(fill = guide_colorbar(barheight = 15)) + theme_bw() +
#     theme(axis.title.x = element_blank(),
#           axis.title.y = element_blank(),
#           legend.title = element_blank(),
#           legend.box.margin = margin(5, 5, 0, 0),
#           axis.text.x = element_text(hjust = 1,
#                                      angle = 30, face = "bold"),
#           axis.text.y = element_text(face = "bold"),
#           plot.title = element_text(hjust = 0.5),
#           plot.subtitle = element_text(hjust = 0.5),
#           panel.border = element_blank())
#   
#   ggsave(tmp_plot, device = 'eps', file = paste0("plots/mae_", str_to_lower(variables[i]), "_", remapping, ".eps"), width = 5, height = 7, units = 'in', dpi = 600)
#   
#   show(tmp_plot)
#   # rm(tmp_plot)
#   
# }


# KGE portrait plots - Climdex Indices 
data <- read_csv("data/climdex/CMIP6_climdex_kge.csv")

temp_indices <- c("TXX", "TNN", "DTR", "TN10P", "TN90P", "TX10P", "TX90P", "WSDI")
prec_indices <- c("PRCPTOT", "RX1DAY", "RX5DAY", "R95P", "SDII", "R20MM", "CWD", "CDD")

# Fix
fix <- read_csv("data/goodness_of_fit/BCC-ESM1_gof_bilinear.csv") |> select(REGION, UF_2) |> filter(!duplicated(REGION))

data <- data |> left_join(fix, by = join_by(REGION)) |> left_join(macroregions_br, by = join_by(UF_2)) |>
  relocate(c(MACROREGION, UF_2), .before = REGION) |>
  arrange_at(c("MACROREGION", "UF_2"))

rm(fix)

data$REGION <- factor(data$REGION, levels = unique(data$REGION[order(data$MACROREGION, data$REGION, decreasing = T)]))
data$MODEL <- factor(data$MODEL, levels = unique(data$MODEL[order(data$MACROREGION, data$MODEL, decreasing = F)]))
data$INDEX <- factor(data$INDEX, levels = unique(data$INDEX))

# KGE portrait for temperature indices

remappings <- c("conservative", "bilinear")


for (remapping in remappings) {
  if (remapping == "conservative") {
    subtitle = "1st Order Conservative Remapping"
    
  }
  else {
    subtitle = "Bilinear Interpolation Remapping"
  }
  
  tmp_plot <-
    data |> filter(REMAPPING == str_to_title(remapping)) |> filter(INDEX %in% temp_indices) |> ggplot(aes(x = MODEL, y = REGION, fill = KGE)) +
    geom_tile(colour = 'black') + scale_fill_gradientn(colours = brewer.pal(11, "RdBu")) +
    labs(title = "KGE Index for Temperature Indices", subtitle = subtitle) +
    guides(fill = guide_colorbar(barheight = 15)) + theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.box.margin = margin(5, 5, 0, 0),
      axis.text.x = element_text(
        hjust = 1,
        angle = 90,
        face = "bold"
      ),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.border = element_blank()
    ) + facet_grid(~INDEX)
  
  # show(tmp_plot)
  ggsave(tmp_plot, device = 'eps', file=paste0("plots/kge_temps_climdex_", remapping, ".eps"), width = 16, height = 9, units = 'in', dpi = 600)
}

for (remapping in remappings) {
  if (remapping == "conservative") {
    subtitle = "1st Order Conservative Remapping"
    
  }
  else {
    subtitle = "Bilinear Interpolation Remapping"
  }
  
  tmp_plot <-
    data |> filter(REMAPPING == str_to_title(remapping)) |> filter(INDEX %in% prec_indices) |> ggplot(aes(x = MODEL, y = REGION, fill = KGE)) +
    geom_tile(colour = 'black') + scale_fill_gradientn(colours = brewer.pal(11, "RdBu")) +
    labs(title = "KGE Index for Precipitation Indices", subtitle = subtitle) +
    guides(fill = guide_colorbar(barheight = 15)) + theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.box.margin = margin(5, 5, 0, 0),
      axis.text.x = element_text(
        hjust = 1,
        angle = 90,
        face = "bold"
      ),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.border = element_blank()
    ) + facet_grid(~INDEX)
  
  # show(tmp_plot)
  ggsave(tmp_plot, device = 'eps', file=paste0("plots/kge_prec_climdex_", remapping, ".eps"), width = 16, height = 9, units = 'in', dpi = 600)
}
