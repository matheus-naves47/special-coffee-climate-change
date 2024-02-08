library(tidyverse)
library(trend)
library(arrow)
library(lubridate)

# import data and summarise the variables by year
# BR-DWGD has incomplete data for 2020
df_dwgd_yearly <-
  read_parquet("data/BR_DWGD_regions_daily.parquet") |>
  subset(year(DATE) != 2020) |> group_by(REGION, UF, UF_2, year(DATE)) |> summarise(TMIN = mean(TMIN),
                                                                                    TMAX = mean(TMAX),
                                                                                    PREC = sum(PREC)) |> rename(YEAR = `year(DATE)`)

# list regions
regions <- df_dwgd_yearly$REGION |> unique()


# function to run Mann-Kendall and Pettitt tests over a region
trend_per_region <- function(data, region) {
  # subset data for region
  df_aux <- data |> subset(REGION == region)
  
  # run Mann-Kendall and Pettitt tests on data (each variable)
  mk_tmin <- mk.test(df_aux$TMIN)
  ptt_tmin <- pettitt.test(df_aux$TMIN)
  
  mk_tmax <- mk.test(df_aux$TMAX)
  ptt_tmax <- pettitt.test(df_aux$TMAX)
  
  mk_prec <- mk.test(df_aux$PREC)
  ptt_prec <- pettitt.test(df_aux$PREC)
  
  # create dataframes for each variable
  df_tmin <- tibble(
    REGION = region,
    UF = df_aux$UF[1],
    VARIABLE = "TMIN",
    MK_p = mk_tmin$p.value,
    MK_z = mk_tmin$statistic,
    MK_S = mk_tmin$estimates[1],
    MK_varS = mk_tmin$estimates[2],
    MK_tau = mk_tmin$estimates[3],
    PTT_cp = df_aux[[ptt_tmin$estimate[1], 4]],
    PTT_p = ptt_tmin$p.value,
    PTT_U = ptt_tmin$statistic,
    PTT_mu1 = mean(df_aux[1:ptt_tmin$estimate[1],]$TMIN),
    PTT_mu2 = mean(df_aux[ptt_tmin$estimate[1]:nrow(df_aux),]$TMIN)
  )
  
  df_tmax <- tibble(
    REGION = region,
    UF = df_aux$UF[1],
    VARIABLE = "TMAX",
    MK_p = mk_tmax$p.value,
    MK_z = mk_tmax$statistic,
    MK_S = mk_tmax$estimates[1],
    MK_varS = mk_tmax$estimates[2],
    MK_tau = mk_tmax$estimates[3],
    PTT_cp = df_aux[[ptt_tmax$estimate[1], 4]],
    PTT_p = ptt_tmax$p.value,
    PTT_U = ptt_tmax$statistic,
    PTT_mu1 = mean(df_aux[1:ptt_tmax$estimate[1],]$TMAX),
    PTT_mu2 = mean(df_aux[ptt_tmax$estimate[1]:nrow(df_aux),]$TMAX)
  )
  
  df_prec <- tibble(
    REGION = region,
    UF = df_aux$UF[1],
    VARIABLE = "PREC",
    MK_p = mk_prec$p.value,
    MK_z = mk_prec$statistic,
    MK_S = mk_prec$estimates[1],
    MK_varS = mk_prec$estimates[2],
    MK_tau = mk_prec$estimates[3],
    PTT_cp = df_aux[[ptt_prec$estimate[1], 4]],
    PTT_p = ptt_prec$p.value,
    PTT_U = ptt_prec$statistic,
    PTT_mu1 = mean(df_aux[1:ptt_prec$estimate[1],]$PREC),
    PTT_mu2 = mean(df_aux[ptt_prec$estimate[1]:nrow(df_aux),]$PREC)
  )
  
  # combine into a single dataframe and interpret the test results
  results <- bind_rows(df_tmin, df_tmax, df_prec) |> mutate(
    MK_trend = case_when(
      MK_S > 0 & MK_p < 0.05 ~ 'increasing',
      MK_S > 0 & MK_p > 0.05 ~ 'no trend',
      MK_S == 0 ~ 'no trend',
      MK_S < 0 & MK_p > 0.05 ~ 'no trend',
      MK_S < 0 & MK_p < 0.05 ~ 'decreasing'
    )
  ) |> relocate(MK_trend, .after = UF)
  
  return(results)
}


# apply function to all regions
br_dwgd_trends <-
  lapply(1:length(regions), function(x)
    trend_per_region(data = df_dwgd_yearly, region = regions[x])) |> bind_rows()

## Export ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

write_csv(br_dwgd_trends, "data/trend_tests/BR-DWGD_trends.csv")
