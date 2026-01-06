# fdp_funciona
#-------------------------------------------------------------------------------
# Script destinado a seg etapa da analise, voltado mais para validacao ----
#-------------------------------------------------------------------------------
# manipulacao dos dados: -------------------------------------------------------
library(ggplot2)
library(dplyr)
library(sf)
library(sp)
library(geobr)
library(hexbin)
library(tidyr)
rm(list = ls())
#-------------------------------------------------------------------------------
# Imortacao dos dados ----------------------------------------------------------
#-------------------------------------------------------------------------------
hex_grid_sf <- readRDS("data_1/shapefile_18_1.rds") |>
  dplyr::rename(ID = CLASE)
estado <- geobr::read_state()
BASE_COM <- readRDS("data_1/base_prec.rds")
# fltro para hexagonos menores que 10 mil
# filtrando dados para hexagonos maiores que 10 mil hectares: -----------------
hex_grid_sf <- hex_grid_sf  |> dplyr::mutate(area = sf::st_area(geometry)/10000) 

# filtro
filtro_hexa <- hex_grid_sf %>%
  mutate(area_num = units::set_units(area, "m^2") %>% as.numeric()) %>%
  filter(area_num < 10000)

# chave do filtro:
filtro_hexa <- unique(filtro_hexa$ID)

#filtrando
hex_grid_sf<- hex_grid_sf %>%
  dplyr::filter(!ID%in%filtro_hexa)

# Criando filtro para o Bioma --------------------------------------------------
# importando shapefile biomas:
biomas <- geobr::read_biomes(year=2019)

# Garantir que ambos os shapefiles estão no mesmo sistema de coordenadas
hex_grid_sf_bioma <- st_transform(hex_grid_sf, crs = st_crs(biomas))

# Executar a interseção espacial
hex_biomes <- st_intersection(hex_grid_sf_bioma, biomas)


# Função para verificar se o hexágono está completamente dentro de um bioma
is_within_biome <- function(hex, biomes) {
  for (i in 1:nrow(biomes)) {
    if (st_within(hex, biomes$geom[i], sparse = FALSE)[1,1]) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Aplicar a função a cada hexágono
hex_biomes_full <- hex_biomes %>%
  filter(sapply(geometry, is_within_biome, biomes = biomas))

# dicionario dos biomas 
dic_bioma <- hex_biomes_full |>
  sf::st_drop_geometry()|>
  dplyr::select(ID, name_biome, code_biome)
# ajustando com gambiarra:

dic_bioma <- dic_bioma |>
  dplyr::mutate(gam=1) |>
  dplyr::group_by(ID) |>
  dplyr::mutate(gam=sum(gam,na.rm = T))|>
  dplyr::ungroup()|>
  dplyr::mutate(name_biome=ifelse(gam>1,NA,name_biome),
                code_biome=ifelse(gam>1,NA,code_biome)) |>
  dplyr::group_by(ID,name_biome, code_biome) |>
  dplyr::summarise(gam=sum(gam,na.rm = T))|>
  dplyr::ungroup()|>
  dplyr::select(-gam)
rm(biomas, hex_biomes, hex_biomes_full, hex_grid_sf_bioma)
# dicionario das regioes -------------------------------------------------------
# importando shapefile biomas:
regiao <- geobr::read_region(year=2019)

# Garantir que ambos os shapefiles estão no mesmo sistema de coordenadas
hex_grid_sf_regiao <- st_transform(hex_grid_sf, crs = st_crs(regiao))

# Executar a interseção espacial
hex_regiao <- st_intersection(hex_grid_sf_regiao, regiao)


# Função para verificar se o hexágono está completamente dentro de um bioma
is_within_regiao <- function(hex, regiaos) {
  for (i in 1:nrow(regiaos)) {
    if (st_within(hex, regiaos$geom[i], sparse = FALSE)[1,1]) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Aplicar a função a cada hexágono
hex_regiao_full <- hex_regiao %>%
  filter(sapply(geometry, is_within_regiao, regiaos = regiao))

# dicionario por regiao
dic_regiao <- hex_regiao_full |>
  sf::st_drop_geometry()|>
  dplyr::select(ID, code_region, name_region)
dic_regiao <- dic_regiao |>
  dplyr::mutate(gam=1) |>
  dplyr::group_by(ID) |>
  dplyr::mutate(gam=sum(gam,na.rm = T))|>
  dplyr::ungroup()|>
  dplyr::mutate(code_region=ifelse(gam>1,NA,code_region),
                name_region=ifelse(gam>1,NA,name_region)) |>
  dplyr::group_by(ID,code_region, name_region) |>
  dplyr::summarise(gam=sum(gam,na.rm = T))|>
  dplyr::ungroup()|>
  dplyr::select(-gam)
rm(regiao, hex_regiao, hex_regiao_full, hex_grid_sf_regiao)
# juntando dicionario----------------------------------------------------------
dicionario_regioes <- dplyr::full_join(dic_bioma, dic_regiao , by="ID")
saveRDS(dicionario_regioes, file = "results/dicionario_regioes.rds")