library(tidyverse)
library(arrow)
library(plotrix)
library(RColorBrewer)


# Color palette to use in the plots
pal <- RColorBrewer::brewer.pal(n=6, "Dark2")

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

# Combine all dataframes in a single one combining all dates in common
data <- df_dwgd |> right_join(`df_BCC-ESM1`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_DWGD", "_BCC_ESM1")) |> 
  right_join(`df_CMCC-ESM2`, by = join_by(REGION, UF, UF_2, DATE)) |> 
  right_join(`df_INM-CM4-8`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_CMCC_ESM2", "_INM_CM4_8")) |> 
  right_join(`df_IPSL-CM6A-LR`, by = join_by(REGION, UF, UF_2, DATE)) |> 
  right_join(`df_MRI-ESM2-0`, by = join_by(REGION, UF, UF_2, DATE), suffix = c("_IPSL_CM6A_LR", "_MRI_ESM2_0")) |>
  right_join(`df_HadGEM3-GC31-MM`, by = join_by(REGION, UF, UF_2, DATE)) |> 
  rename(TMIN_HadGEM3_GC31_MM = TMIN, TMAX_HadGEM3_GC31_MM = TMAX, PREC_HadGEM3_GC31_MM = PREC)


# Taylor Diagrams

# Tmin
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_BCC_ESM1, normalize = T, col = pal[1], main = "TMIN")
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_CMCC_ESM2, normalize = T, add = T, col = pal[2])
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_HadGEM3_GC31_MM, normalize = T, add = T, col = pal[3])
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_INM_CM4_8, normalize = T, add = T, col = pal[4])
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_IPSL_CM6A_LR, normalize = T, add = T, col = pal[5])
taylor.diagram(ref = data$TMIN_DWGD, model = data$TMIN_MRI_ESM2_0, normalize = T, add = T, col = pal[6])
legend("topright", c("BCC-ESM1", "CMCC-ESM2", "HadGEM3-GC31-MM", "INM-CM4-8", "IPSL-CM6A-LR", "MRI-ESM2-0"), pch = 19, col = pal, ncol = 2, inset = c(-0.04,-0.1), cex=.75)

# Tmax
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_BCC_ESM1, normalize = T, col = pal[1], main = "TMAX")
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_CMCC_ESM2, normalize = T, add = T, col = pal[2])
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_HadGEM3_GC31_MM, normalize = T, add = T, col = pal[3])
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_INM_CM4_8, normalize = T, add = T, col = pal[4])
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_IPSL_CM6A_LR, normalize = T, add = T, col = pal[5])
taylor.diagram(ref = data$TMAX_DWGD, model = data$TMAX_MRI_ESM2_0, normalize = T, add = T, col = pal[6])
legend("topright", c("BCC-ESM1", "CMCC-ESM2", "HadGEM3-GC31-MM", "INM-CM4-8", "IPSL-CM6A-LR", "MRI-ESM2-0"), pch = 19, col = pal, ncol = 2, inset = c(-0.04,-0.1), cex=.75)

# Prec
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_BCC_ESM1, normalize = T, col = pal[1], main = "PREC")
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_CMCC_ESM2, normalize = T, add = T, col = pal[2])
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_HadGEM3_GC31_MM, normalize = T, add = T, col = pal[3])
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_INM_CM4_8, normalize = T, add = T, col = pal[4])
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_IPSL_CM6A_LR, normalize = T, add = T, col = pal[5])
taylor.diagram(ref = data$PREC_DWGD, model = data$PREC_MRI_ESM2_0, normalize = T, add = T, col = pal[6])
legend("topright", c("BCC-ESM1", "CMCC-ESM2", "HadGEM3-GC31-MM", "INM-CM4-8", "IPSL-CM6A-LR", "MRI-ESM2-0"), pch = 19, col = pal, ncol = 2, inset = c(-0.04,-0.1), cex=.75)




