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
# Script voltado para a juncao dos dados de forma a gerar um unico DF ----------
#-------------------------------------------------------------------------------
# Carregar pacotes necessários
library(dplyr)
#
# Função para ler cada arquivo e adicionar uma coluna de origem
read_and_label <- function(file) {
  df <- readRDS(file)
  return(df)
}

# Ler todos os arquivos e combiná-los em um único data frame -------------------
## chirps:
chirps <- list.files(path = "C:/Tese/TIFs/CHIRPS_RESULTADOS/", 
                     pattern = "*.rds", full.names = TRUE) %>%
  lapply(read_and_label) %>%
  bind_rows()

## br_dwgC:
br_dwgd <- list.files(path = "C:/Tese/TIFs/BR_DWGD_RESULTADOS/",
                      pattern = "*.rds", full.names = TRUE) %>%
  lapply(read_and_label) %>%
  bind_rows()

## era5:
era5 <- list.files(path = "C:/Tese/TIFs/ERA5_RESULTADOS/",
                   pattern = "*.rds", full.names = TRUE) %>%
  lapply(read_and_label) %>%
  bind_rows()

## persiann:
persiann <- list.files(path = "C:/Tese/TIFs/PERSIANN_RESULTADOS/",
                       pattern = "*.rds", full.names = TRUE) %>%
  lapply(read_and_label) %>%
  bind_rows()
rm(read_and_label)
# -----------------------------------------------------------------------------
# manipulacao dos dados:
# crirps
chirps <- chirps |>
  dplyr::mutate(CLASSE = paste0(ID,"-",data))|>
  dplyr::rename(prec_chirps = precipitacao)|>
  dplyr::select(CLASSE, data, ID, prec_chirps)

# br_dwgd
br_dwgd <- br_dwgd |>
  dplyr::mutate(precipitacao = ifelse(precipitacao==0,NA,precipitacao))|>
  dplyr::mutate(CLASSE = paste0(ID,"-",data),
                precipitacao = (precipitacao * 0.006866665) + 225)|>
  dplyr::rename(prec_br_dwgd = precipitacao)|>
  dplyr::select(CLASSE, prec_br_dwgd)

# era5
era5 <- era5 |>
  dplyr::mutate(CLASSE = paste0(ID,"-",data))|>
  dplyr::rename(prec_era5 = precipitacao)|>
  dplyr::select(CLASSE, prec_era5)

# persiann
persiann <- persiann |>
  dplyr::mutate(CLASSE = paste0(ID,"-",data))|>
  dplyr::rename(prec_persiann = precipitacao)|>
  dplyr::select(CLASSE, prec_persiann)

# juntando os dados:
base_prec <- dplyr::full_join(chirps, br_dwgd, by="CLASSE")
rm(chirps, br_dwgd)
base_prec <- dplyr::full_join(base_prec, era5, by="CLASSE")
rm(era5)
base_prec <- dplyr::full_join(base_prec, persiann, by="CLASSE")
rm(persiann)
# salvando base de dados para 
base_prec <- base_prec |> dplyr::select(-CLASSE)

# filtro que elimina dados distorcidos pelo prec_br_dwgd
base_prec |> dplyr::glimpse()

# Verificar o resultado
glimpse(base_prec)
# modificacao da data ----------------------------------------------------------
base_prec <- base_prec |>
  dplyr::mutate(data = as.Date(data)) |>
  dplyr::filter(lubridate::year(data)<2023 )

# declarando estacao do ano:
library(lubridate)

# criando dummy season:
base_prec <- base_prec |>
  dplyr::mutate(day_of_year = yday(data))

# criando dummy
base_prec <- base_prec %>%
  mutate(season = ifelse(day_of_year >= 355 | day_of_year < 80, "Verao",
                         ifelse(day_of_year >= 80 & day_of_year < 172, "Outono",
                                ifelse(day_of_year >= 172 & day_of_year < 266, "Inverno",
                                       "Primavera"))))
# Verificar o resultado
glimpse(base_prec)
base_prec <- base_prec |> dplyr::select(-day_of_year)
# retirando heagonos que sofreram influencia das regiões costeiras de rios e lagos (ERROS)
summary(base_prec$ME_chirps, na.rm=T)

# salvando nova base de dados:--------------------------------------------------
hex_grid_sf <- readRDS("data_1/shapefile_18_1.rds") |>
  dplyr::rename(ID = CLASE)
# fltro para hexagonos menores que 10 mil
# filtrando dados para hexagonos maiores que 10 mil hectares: -----------------
filtro_hexa <- hex_grid_sf  |> dplyr::mutate(area = sf::st_area(geometry)/10000) 

# filtro
filtro_hexa <- filtro_hexa %>%
  mutate(area_num = units::set_units(area, "m^2") %>% as.numeric()) %>%
  filter(area_num < 10000)
# chave do filtro:
filtro_hexa <- unique(filtro_hexa$ID)
# filtrando os dados:
base_prec <- base_prec %>%
  dplyr::filter(!ID%in%filtro_hexa)
rm(hex_grid_sf, filtro_hexa)

hexa_defeito <- readRDS("data_1/hexa_defeito.rds")
hexa_defeito <- unique(hexa_defeito$ID)
base_prec<- base_prec |> 
  dplyr::filter(!ID%in%hexa_defeito)

# salvando dados 
saveRDS(base_prec, file = "data_1/base_prec.rds")


