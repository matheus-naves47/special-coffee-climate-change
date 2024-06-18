library(tidyverse)
library(terra)
library(tidyterra)
library(geodata)
library(geobr)

# Study areas data (with corrections to match GADM data)
regions <- read_csv("data/raw/origin_regions.csv") %>% 
  mutate(MUNICIPIO = str_replace_all(MUNICIPIO, "d`", "D'"),
         MUNICIPIO = str_replace(MUNICIPIO, "Passa-Vinte", "Passa Vinte"),
         MUNICIPIO = str_replace(MUNICIPIO, "Dona Eusébia", "Dona Euzébia"),
         MUNICIPIO = str_replace(MUNICIPIO, "São Thomé das Letras", "São Tomé das Letras"),
         MUNICIPIO = str_replace(MUNICIPIO, "Diamante d'Oeste", "Diamante D'Oeste"),
         MUNICIPIO = str_replace(MUNICIPIO, "Rancho Alegre d'Oeste", "Rancho Alegre D'Oeste"),
         MUNICIPIO = str_replace(MUNICIPIO, "Machadinho d'Oeste", "Machadinho D'Oeste"),
         MUNICIPIO = str_replace(MUNICIPIO, "Alta Floresta d'Oeste", "Alta Floresta D'Oeste"),
         MUNICIPIO = str_replace(MUNICIPIO, "Alvorada d'Oeste", "Alvorada D'Oeste"),
         MUNICIPIO = str_replace(MUNICIPIO, "Espigão d'Oeste", "Espigão D'Oeste"),
         MUNICIPIO = str_replace(MUNICIPIO, "Nova Brasilândia d'Oeste", "Nova Brasilândia D'Oeste"),
         MUNICIPIO = str_replace(MUNICIPIO, "Santa Luzia d'Oeste", "Santa Luzia D'Oeste"),
         MUNICIPIO = str_replace(MUNICIPIO, "São Felipe d'Oeste", "São Felipe D'Oeste")) %>% 
  rename(NAME_2 = MUNICIPIO,
         NAME_1 = UF,
         REGION = REGIAO,
         NAME_3 = UF_SIGLA)
  
# Brazil's shapefile at municipal administrative level
brazil_mun_lvl <- gadm("BRA", level = 2, "data/raw/geodata")

# Subset geodata to study areas
regions_geoloc <- brazil_mun_lvl %>% inner_join(regions, by = c("NAME_1", "NAME_2"))# %>% terra::aggregate(by='REGION')
regions_geoloc <- sf::st_as_sf(regions_geoloc) # Convert to sf

# Fix Caparaó region names (region contains municipalities from both Minas Gerais and Espírito Santo states)
regions_geoloc[5,]$NAME_1 <- "Espírito Santo"
regions_geoloc[5,]$NAME_3 <- "ES"

rm(brazil_mun_lvl, regions) # Cleanup

write_rds(regions_geoloc, "data/processed/geodata/origin_regions_geodata.rds", compress = "xz") # Export