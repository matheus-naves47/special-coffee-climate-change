# Função para extrair cada variável do BR-DWGD para as áreas de interesse por ano
extract_by_year_DWGD <- function(index, spatvector, years, data_path) {
  temp_year <- years[index] # Índice do ano
  
  # Importar os dados em raster para extração
  # Aqui deixei apenas as variáveis que você pediu.
  ETo_rst <-
    rast(paste0(data_path,
                "BR_DWGD/ETo/ETo_BR-DWGD_",
                temp_year,
                ".nc"))
  pr_rst <-
    rast(paste0(data_path,
                "BR_DWGD/pr/pr_BR-DWGD_",
                temp_year,
                ".nc"))
  Tmax_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Tmax/Tmax_BR-DWGD_",
                temp_year,
                ".nc"))
  Tmin_rst <-
    rast(paste0(data_path,
                "BR_DWGD/Tmin/Tmin_BR-DWGD_",
                temp_year,
                ".nc"))
  
  
  # Aqui, para cada raster, modifico o nome de cada entrada do raster para a data correspondente.
  # Se não fizer isso, ele fica com a convenção de nomes padrão, que é meio confusa, na minha opinião.
  # Isso é apenas questão de organização.
  names(ETo_rst) <- terra::time(ETo_rst)
  names(pr_rst) <- terra::time(pr_rst)
  names(Tmax_rst) <- terra::time(Tmax_rst)
  names(Tmin_rst) <- terra::time(Tmin_rst)
  
  
  # Aqui se faz a extração para um dataframe temporário, de cada variável de interesse, usando a função exact_extract.
  # Como expliquei, essa função faz uma média ponderada com a fração de cobertura do polígono com as grades dos dados.
  # No caso da precipitação, utilizamos a função de soma, pois o que interessa é a precipitação total. A média de precipitação nesse caso não faz sentido.
  ETo_tmp <-
    exact_extract(ETo_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  pr_tmp <-
    exact_extract(pr_rst,
                  spatvector,
                  fun = "sum",
                  append_cols = T)
  Tmax_tmp <-
    exact_extract(Tmax_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  Tmin_tmp <-
    exact_extract(Tmin_rst,
                  spatvector,
                  fun = "mean",
                  append_cols = T)
  
  # Aqui se faz o pivoteamento de cada dataframe temporário gerado no bloco anterior, do formato largo pro formato longo. 
  # É bastante importante saber fazer isso, e é bem convencional trabalhar com dados em formato longo (e mais fácil também).
  
  ETo_tmp <-
    ETo_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "ETo") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  pr_tmp <-
    pr_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "PR") |>
    mutate(DATE = str_replace_all(DATE, "sum.", ""))
  
  Tmax_tmp <-
    Tmax_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMAX") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  Tmin_tmp <-
    Tmin_tmp |> pivot_longer(!(1:ncol(spatvector) - 1), names_to = "DATE", values_to = "TMIN") |>
    mutate(DATE = str_replace_all(DATE, "mean.", ""))
  
  
  # Aqui se combina todos esses conjuntos de dados em um só. Como temos algumas informações repetidas, 
  # pode pegar de qualquer um, que no caso aqui, peguei do Tmin_tmp. No final, converto a coluna DATE para o formato de data apropriado,
  # pra melhor leitura e manipulações futuras.
  df_tmp <-
    tibble(
      MUNICIPALITY_CODE = Tmin_tmp$code_muni,
      MUNICIPALITY = Tmin_tmp$name_muni,
      UF = Tmin_tmp$name_state,
      UF_2 = Tmin_tmp$abbrev_state,
      DATE = Tmin_tmp$DATE,
      TMIN = Tmin_tmp$TMIN,
      TMAX = Tmax_tmp$TMAX,
      PR = pr_tmp$PR,
      ETO = ETo_tmp$ETo
    ) |> mutate(DATE = str_replace_all(DATE, "mean.", ""),
                DATE = as_date(DATE))
  
  return(df_tmp) # Aqui retorna o conjunto de dados
  
  gc() # Limpa a memória
  
}