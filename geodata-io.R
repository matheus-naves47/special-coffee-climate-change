library(tidyverse)
library(terra)
library(tidyterra)
library(geobr)
library(sf)

# Brazil's shapefile at municipal administrative level (using geobr's latest data)
brazil_mun_lvl <- read_municipality(year = 2024)

brazil_mun_lvl <- brazil_mun_lvl %>% mutate(name_muni = str_to_title(name_muni))

# Study areas data (with corrections to match geobr's data)
regions <- read_csv("data/raw/origin_regions.csv") %>% 
  rename(name_muni = MUNICIPIO,
         name_state = UF,
         REGION = REGIAO,
         abbrev_state = UF_SIGLA) %>% 
  mutate(name_muni = str_replace_all(name_muni, "d`", "D'"),
         name_muni = str_replace_all(name_muni, "Thomé", "Tomé"),
         name_muni = str_replace_all(name_muni, "Passa-", "Passa "),
         name_muni = str_replace_all(name_muni, "Dona Eusébia", "Dona Euzébia"),
         name_muni = str_to_title(name_muni),
         name_muni = str_replace_all(name_muni, "Santo Antônio Do Leverger", "Santo Antônio De Leverger"),
         name_muni = str_replace_all(name_muni, "Barão De Monte Alto", "Barão Do Monte Alto"))


# Subset geodata to study areas
regions_geoloc <- brazil_mun_lvl %>% inner_join(regions, by = c("name_muni", "name_state", "abbrev_state")) %>% rename(bsca_region = REGION) %>% 
  select(bsca_region, name_muni, everything(), -code_region) %>% 
  mutate(code_state = as.character(code_state)) %>% 
  arrange(bsca_region)

regions_geoloc <- vect(regions_geoloc)
regions_geoloc_agg <- regions_geoloc %>% terra::aggregate(by='bsca_region')

regions_geoloc <- sf::st_as_sf(regions_geoloc)
regions_geoloc_agg <- sf::st_as_sf(regions_geoloc_agg)

# Fix Caparaó region names (region contains municipalities from both Minas Gerais and Espírito Santo states)
# regions_geoloc[5,]$NAME_1 <- "Espírito Santo"
# regions_geoloc[5,]$NAME_3 <- "ES"


write_rds(regions_geoloc, "data/processed/geodata/origin_regions_geodata.rds", compress = "xz") # Export
write_rds(regions_geoloc_agg, "data/processed/geodata/origin_regions_geodata_agg.rds", compress = "xz") # Export
