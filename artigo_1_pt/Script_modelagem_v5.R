#-------------------------------------------------------------------------------
# Script destinado a ter etapa da analise, voltado para os indicadores ---------
#-------------------------------------------------------------------------------
# manipulacao dos dados: -------------------------------------------------------
library(ggplot2)
library(dplyr)
library(sf)
library(sp)
library(geobr)
library(hexbin)
library(tidyr)
library(fmsb)
rm(list = ls())
#-------------------------------------------------------------------------------
# Imortacao dos dados ----------------------------------------------------------
#-------------------------------------------------------------------------------
hex_grid_sf <- readRDS("data_1/shapefile_18_1.rds") |>
  dplyr::rename(ID = CLASE)
estado <- geobr::read_state()
BASE_COM <- readRDS("data_1/base_prec.rds")
# dicionario de lugares:
dicio_reg <- readRDS(file = "data_1/dicionario_regioes.rds")
BASE_COM <- dplyr::full_join(dicio_reg, BASE_COM, by="ID")
#-------------------------------------------------------------------------------
# Calculando CC para cada grupo de CLASE
#-------------------------------------------------------------------------------
# Definir a função para calcular a correlação
# fazendo para as regioes: -----------------------------------------------------
calculate_correlation <- function(data, compare_var) {
  data %>%
    dplyr::group_by(name_biome, name_region, ID) %>%
    dplyr::filter(!is.na(.data[[compare_var]]) & !is.na(.data[["prec_br_dwgd"]])) %>%
    dplyr::summarise(
      correlation = if (n() > 1) cor(.data[[compare_var]], .data[["prec_br_dwgd"]],
                                     method = "pearson", use = "complete.obs") else NA_real_,
      .groups = "drop"
    )
}

# Calculando CC por grupo para cada variável
CC_chirps <- calculate_correlation(BASE_COM, "prec_chirps") %>% 
  dplyr::rename(prec_chirps = correlation)

CC_era5 <- calculate_correlation(BASE_COM, "prec_era5") %>% 
  dplyr::rename(prec_era5 = correlation)  %>% dplyr::select(-name_biome, -name_region)

CC_persiann <- calculate_correlation(BASE_COM, "prec_persiann") %>% 
  dplyr::rename(prec_persiann = correlation) %>% dplyr::select(-name_biome, -name_region)

# juntando 
BASE <- CC_chirps %>%
  dplyr::left_join(CC_era5, by="ID") %>%
  dplyr::left_join(CC_persiann, by="ID") 

rm(CC_chirps, CC_era5, CC_persiann)
# operacoes:
## completa
BASE_completa <- BASE |>
  dplyr::summarise(CC_prec_chirps = mean(prec_chirps,na.rm=T),
                   CC_prec_era5 = mean(prec_era5,na.rm=T),
                   CC_prec_persiann = mean(prec_persiann,na.rm=T))|>
  dplyr::mutate(Periodo = "Completo") |>
  dplyr::select(Periodo, CC_prec_chirps, CC_prec_era5, CC_prec_persiann)

## regiao
BASE_regiao <- BASE |>
  dplyr::group_by(name_region)|>
  dplyr::summarise(CC_prec_chirps = mean(prec_chirps,na.rm=T),
                   CC_prec_era5 = mean(prec_era5,na.rm=T),
                   CC_prec_persiann = mean(prec_persiann,na.rm=T))|>
  dplyr::rename(Periodo = name_region) |>
  dplyr::select(Periodo, CC_prec_chirps, CC_prec_era5, CC_prec_persiann) |>
  dplyr::filter(!is.na(Periodo))

## bioma
BASE_bioma <- BASE |>
  dplyr::group_by(name_biome)|>
  dplyr::summarise(CC_prec_chirps = mean(prec_chirps,na.rm=T),
                   CC_prec_era5 = mean(prec_era5,na.rm=T),
                   CC_prec_persiann = mean(prec_persiann,na.rm=T))|>
  dplyr::rename(Periodo = name_biome) |>
  dplyr::select(Periodo, CC_prec_chirps, CC_prec_era5, CC_prec_persiann) |>
  dplyr::filter(Periodo!="Sistema Costeiro")

## juntando 
BASE <- NULL
BASE <- dplyr::bind_rows(BASE_completa, BASE_regiao, BASE_bioma)
rm(calculate_correlation, BASE_completa, BASE_regiao, BASE_bioma)

# Fazendo por estacao do ano ---------------------------------------------------
calculate_correlation <- function(data, compare_var) {
  data %>%
    dplyr::group_by(season, ID) %>%
    dplyr::filter(!is.na(.data[[compare_var]]) & !is.na(.data[["prec_br_dwgd"]])) %>%
    dplyr::summarise(
      correlation = if (n() > 1) cor(.data[[compare_var]], .data[["prec_br_dwgd"]], 
                                     method = "pearson", use = "complete.obs") else NA_real_,
      .groups = "drop"
    )
}

# Calculando CC por grupo para cada variável
CC_chirps <- calculate_correlation(BASE_COM, "prec_chirps") %>% 
  dplyr::rename(prec_chirps = correlation)

CC_era5 <- calculate_correlation(BASE_COM, "prec_era5") %>% 
  dplyr::rename(prec_era5 = correlation)  

CC_persiann <- calculate_correlation(BASE_COM, "prec_persiann") %>% 
  dplyr::rename(prec_persiann = correlation) 

# juntando 
SEASON <- CC_chirps %>%
  dplyr::left_join(CC_era5, by=c("ID","season")) %>%
  dplyr::left_join(CC_persiann, by=c("ID","season")) 

rm(CC_chirps, CC_era5, CC_persiann)

# operacoes:
## completa
SEASON <- SEASON |>
  dplyr::group_by(season)|>
  dplyr::summarise(CC_prec_chirps = mean(prec_chirps, na.rm=T),
                   CC_prec_era5 = mean(prec_era5, na.rm=T),
                   CC_prec_persiann = mean(prec_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::select(Periodo, CC_prec_chirps, CC_prec_era5, CC_prec_persiann) 
rm(calculate_correlation)

# juntanto todos os CC:
CC_BASE <- dplyr::bind_rows(BASE, SEASON)
rm(BASE, SEASON, calculate_correlation)

# Fazendo por estacoes do ano, por regiao --------------------------------------
calculate_correlation <- function(data, compare_var) {
  data %>%
    dplyr::group_by(name_region, season, ID) %>%
    dplyr::filter(!is.na(.data[[compare_var]]) & !is.na(.data[["prec_br_dwgd"]])) %>%
    dplyr::summarise(
      correlation = if (n() > 1) cor(.data[[compare_var]], .data[["prec_br_dwgd"]], 
                                     method = "pearson", use = "complete.obs") else NA_real_,
      .groups = "drop"
    )
}

# Calculando CC por grupo para cada variável
CC_chirps <- calculate_correlation(BASE_COM, "prec_chirps") %>% 
  dplyr::rename(prec_chirps = correlation)

CC_era5 <- calculate_correlation(BASE_COM, "prec_era5") %>% 
  dplyr::rename(prec_era5 = correlation)  

CC_persiann <- calculate_correlation(BASE_COM, "prec_persiann") %>% 
  dplyr::rename(prec_persiann = correlation) 

# juntando 
CC_RE_SE <- CC_chirps %>%
  dplyr::left_join(CC_era5, by=c("ID","season")) %>%
  dplyr::left_join(CC_persiann, by=c("ID","season")) 

rm(CC_chirps, CC_era5, CC_persiann)

# operacoes:
## completa
CC_RE_SE <- CC_RE_SE |>
  dplyr::group_by(name_region, season)|>
  dplyr::summarise(CC_prec_chirps = mean(prec_chirps, na.rm=T),
                   CC_prec_era5 = mean(prec_era5, na.rm=T),
                   CC_prec_persiann = mean(prec_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::select(Periodo, CC_prec_chirps, CC_prec_era5, CC_prec_persiann) |>
  dplyr::filter(!is.na(name_region))
rm(calculate_correlation)

#-------------------------------------------------------------------------------
# Obtendo os RMSE --------------------------------------------------------------
#-------------------------------------------------------------------------------
# Função para calcular RMSE
RMSE <- function(observed, predicted) {
  # Calculando os resíduos ao quadrado
  residuals_squared <- (observed - predicted)^2
  
  # Ignorando NA nos cálculos
  valid_data <- complete.cases(residuals_squared)
  
  # Calculando a média dos resíduos ao quadrado
  mean_squared_error <- mean(residuals_squared[valid_data], na.rm = TRUE)
  
  # Tirando a raiz quadrada para obter o RMSE
  rmse <- sqrt(mean_squared_error)
  
  return(rmse)
}
# geral (regiao e bioma)--------------------------------------------------------
RMSE_geral <- BASE_COM %>%
  dplyr::group_by(name_biome, name_region, ID) %>%
  dplyr::summarise(
    RMSE_chirps = RMSE(prec_chirps, prec_br_dwgd), 
    RMSE_era5 = RMSE(prec_era5, prec_br_dwgd), 
    RMSE_persiann = RMSE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
# Calculando RMSE por grupo 
RMSE_completo <- RMSE_geral %>%
  dplyr::summarise(
    RMSE_chirps = mean(RMSE_chirps, na.rm=T), 
    RMSE_era5 = mean(RMSE_era5, na.rm=T), 
    RMSE_persiann = mean(RMSE_persiann, na.rm=T)) %>%
  dplyr::mutate(Periodo = "Completo") %>%
  dplyr::select(Periodo, RMSE_chirps, RMSE_era5, RMSE_persiann)

RMSE_base_regiao <- RMSE_geral %>%
  dplyr::filter(!is.na(name_region))%>%
  dplyr::group_by(name_region) %>%
  dplyr::summarise(
    RMSE_chirps = mean(RMSE_chirps, na.rm=T), 
    RMSE_era5 = mean(RMSE_era5, na.rm=T), 
    RMSE_persiann = mean(RMSE_persiann, na.rm=T)) %>%
  dplyr::ungroup()|>
  dplyr::rename(Periodo = name_region) |>
  dplyr::select(Periodo, RMSE_chirps, RMSE_era5, RMSE_persiann) |>
  dplyr::filter(!is.na(Periodo))

RMSE_base_bioma <- RMSE_geral %>%
  dplyr::filter(!is.na(name_biome))%>%
  dplyr::group_by(name_biome) %>%
  dplyr::summarise(
    RMSE_chirps = mean(RMSE_chirps, na.rm=T), 
    RMSE_era5 = mean(RMSE_era5, na.rm=T), 
    RMSE_persiann = mean(RMSE_persiann, na.rm=T)) %>%
  dplyr::ungroup() |>
  dplyr::rename(Periodo = name_biome) |>
  dplyr::select(Periodo, RMSE_chirps, RMSE_era5, RMSE_persiann) |>
  dplyr::filter(Periodo!="Sistema Costeiro")

rm(RMSE_geral)
RMSE_BASE <- dplyr::bind_rows(RMSE_completo, RMSE_base_regiao, RMSE_base_bioma)
rm(RMSE_completo, RMSE_base_regiao, RMSE_base_bioma)

# Fazendo por estacao do ano ----------------------------------------------------
RMSE_geral <- BASE_COM %>%
  dplyr::group_by(season, ID) %>%
  dplyr::summarise(
    RMSE_chirps = RMSE(prec_chirps, prec_br_dwgd), 
    RMSE_era5 = RMSE(prec_era5, prec_br_dwgd), 
    RMSE_persiann = RMSE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
# Calculando RMSE por grupo 
RMSE_season <- RMSE_geral %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    RMSE_chirps = mean(RMSE_chirps, na.rm=T), 
    RMSE_era5 = mean(RMSE_era5, na.rm=T), 
    RMSE_persiann = mean(RMSE_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::select(Periodo, RMSE_chirps, RMSE_era5, RMSE_persiann) 

# salvando a base completa: 
RMSE_BASE <- dplyr::bind_rows(RMSE_BASE, RMSE_season)
rm(RMSE_season)

# Fazendo por estacao do ano e regiao: -----------------------------------------
RMSE_geral <- BASE_COM %>%
  dplyr::group_by(name_region, season, ID) %>%
  dplyr::summarise(
    RMSE_chirps = RMSE(prec_chirps, prec_br_dwgd), 
    RMSE_era5 = RMSE(prec_era5, prec_br_dwgd), 
    RMSE_persiann = RMSE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
# Calculando RMSE por grupo 
RMSE_RE_SE <- RMSE_geral %>%
  dplyr::group_by(name_region, season) %>%
  dplyr::summarise(
    RMSE_chirps = mean(RMSE_chirps, na.rm=T), 
    RMSE_era5 = mean(RMSE_era5, na.rm=T), 
    RMSE_persiann = mean(RMSE_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::select(name_region, Periodo, RMSE_chirps, RMSE_era5, RMSE_persiann) |>
  dplyr::filter(!is.na(name_region)) |> dplyr::filter(!is.na(Periodo))
rm(RMSE_geral,RMSE)

#-------------------------------------------------------------------------------
# Calculando o ME para cada grupo de CLASE
#-------------------------------------------------------------------------------
# Função para calcular ME
ME <- function(observed, predicted) {
  # Calculando os resíduos ao quadrado
  residuals_squared <- (observed - predicted)
  
  # Ignorando NA nos cálculos
  valid_data <- complete.cases(residuals_squared)
  
  # Calculando a média dos resíduos
  me <- mean(residuals_squared[valid_data], na.rm = TRUE)
  
  return(me)
}
# geral (regiao e bioma)--------------------------------------------------------
ME_geral <- BASE_COM %>%
  dplyr::group_by(name_biome, name_region, ID) %>%
  dplyr::summarise(
    ME_chirps = ME(prec_chirps, prec_br_dwgd), 
    ME_era5 = ME(prec_era5, prec_br_dwgd), 
    ME_persiann = ME(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
# Calculando ME por grupo 
ME_completo <- ME_geral %>%
  dplyr::summarise(
    ME_chirps = mean(ME_chirps, na.rm=T), 
    ME_era5 = mean(ME_era5, na.rm=T), 
    ME_persiann = mean(ME_persiann, na.rm=T)) %>%
  dplyr::mutate(Periodo = "Completo") %>%
  dplyr::select(Periodo, ME_chirps, ME_era5, ME_persiann)

ME_base_regiao <- ME_geral %>%
  dplyr::filter(!is.na(name_region))%>%
  dplyr::group_by(name_region) %>%
  dplyr::summarise(
    ME_chirps = mean(ME_chirps, na.rm=T), 
    ME_era5 = mean(ME_era5, na.rm=T), 
    ME_persiann = mean(ME_persiann, na.rm=T)) %>%
  dplyr::ungroup()|>
  dplyr::rename(Periodo = name_region) |>
  dplyr::select(Periodo, ME_chirps, ME_era5, ME_persiann) |>
  dplyr::filter(!is.na(Periodo))

ME_base_bioma <- ME_geral %>%
  dplyr::filter(!is.na(name_biome))%>%
  dplyr::group_by(name_biome) %>%
  dplyr::summarise(
    ME_chirps = mean(ME_chirps, na.rm=T), 
    ME_era5 = mean(ME_era5, na.rm=T), 
    ME_persiann = mean(ME_persiann, na.rm=T)) %>%
  dplyr::ungroup() |>
  dplyr::rename(Periodo = name_biome) |>
  dplyr::select(Periodo, ME_chirps, ME_era5, ME_persiann) |>
  dplyr::filter(Periodo!="Sistema Costeiro")

rm(ME_geral)
ME_BASE <- dplyr::bind_rows(ME_completo, ME_base_regiao, ME_base_bioma)
rm(ME_completo, ME_base_regiao, ME_base_bioma)

# Fazendo por estacao do ano ----------------------------------------------------
ME_geral <- BASE_COM %>%
  dplyr::group_by(season, ID) %>%
  dplyr::summarise(
    ME_chirps = ME(prec_chirps, prec_br_dwgd), 
    ME_era5 = ME(prec_era5, prec_br_dwgd), 
    ME_persiann = ME(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
# Calculando ME por grupo 
ME_season <- ME_geral %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    ME_chirps = mean(ME_chirps, na.rm=T), 
    ME_era5 = mean(ME_era5, na.rm=T), 
    ME_persiann = mean(ME_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::select(Periodo, ME_chirps, ME_era5, ME_persiann) 

# salvando a base completa: 
ME_BASE <- dplyr::bind_rows(ME_BASE, ME_season)
rm(ME_season)

# Fazendo por estacao do ano e regiao: -----------------------------------------
ME_geral <- BASE_COM %>%
  dplyr::group_by(name_region, season, ID) %>%
  dplyr::summarise(
    ME_chirps = ME(prec_chirps, prec_br_dwgd), 
    ME_era5 = ME(prec_era5, prec_br_dwgd), 
    ME_persiann = ME(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

# Calculando ME por grupo 
ME_RE_SE <- ME_geral %>%
  dplyr::group_by(name_region, season) %>%
  dplyr::summarise(
    ME_chirps = mean(ME_chirps, na.rm=T), 
    ME_era5 = mean(ME_era5, na.rm=T), 
    ME_persiann = mean(ME_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::select(name_region, Periodo, ME_chirps, ME_era5, ME_persiann) |>
  dplyr::filter(!is.na(name_region)) |> dplyr::filter(!is.na(Periodo))
rm(ME_geral,ME)
#-------------------------------------------------------------------------------
# Calculando o MAE para cada grupo de CLASE
#-------------------------------------------------------------------------------
# Função para calcular MAE
MAE <- function(observed, predicted) {
  # Calculando os resíduos ao quadrado
  residuals_squared <- abs(observed - predicted)
  
  # Ignorando NA nos cálculos
  valid_data <- complete.cases(residuals_squared)
  
  # Calculando a média dos resíduos
  mae <- mean(residuals_squared[valid_data], na.rm = TRUE)
  
  return(mae)
}
# geral (regiao e bioma)--------------------------------------------------------
MAE_geral <- BASE_COM %>%
  dplyr::group_by(name_biome, name_region, ID) %>%
  dplyr::summarise(
    MAE_chirps = MAE(prec_chirps, prec_br_dwgd), 
    MAE_era5 = MAE(prec_era5, prec_br_dwgd), 
    MAE_persiann = MAE(prec_persiann, prec_br_dwgd),#priMAEiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupaMAEnto após o summarize
  )
# Calculando MAE por grupo 
MAE_completo <- MAE_geral %>%
  dplyr::summarise(
    MAE_chirps = mean(MAE_chirps, na.rm=T), 
    MAE_era5 = mean(MAE_era5, na.rm=T), 
    MAE_persiann = mean(MAE_persiann, na.rm=T)) %>%
  dplyr::mutate(Periodo = "Completo") %>%
  dplyr::select(Periodo, MAE_chirps, MAE_era5, MAE_persiann)

MAE_base_regiao <- MAE_geral %>%
  dplyr::filter(!is.na(name_region))%>%
  dplyr::group_by(name_region) %>%
  dplyr::summarise(
    MAE_chirps = mean(MAE_chirps, na.rm=T), 
    MAE_era5 = mean(MAE_era5, na.rm=T), 
    MAE_persiann = mean(MAE_persiann, na.rm=T)) %>%
  dplyr::ungroup()|>
  dplyr::rename(Periodo = name_region) |>
  dplyr::select(Periodo, MAE_chirps, MAE_era5, MAE_persiann) |>
  dplyr::filter(!is.na(Periodo))

MAE_base_bioma <- MAE_geral %>%
  dplyr::filter(!is.na(name_biome))%>%
  dplyr::group_by(name_biome) %>%
  dplyr::summarise(
    MAE_chirps = mean(MAE_chirps, na.rm=T), 
    MAE_era5 = mean(MAE_era5, na.rm=T), 
    MAE_persiann = mean(MAE_persiann, na.rm=T)) %>%
  dplyr::ungroup() |>
  dplyr::rename(Periodo = name_biome) |>
  dplyr::select(Periodo, MAE_chirps, MAE_era5, MAE_persiann) |>
  dplyr::filter(Periodo!="Sistema Costeiro")

rm(MAE_geral)
MAE_BASE <- dplyr::bind_rows(MAE_completo, MAE_base_regiao, MAE_base_bioma)
rm(MAE_completo, MAE_base_regiao, MAE_base_bioma)

# Fazendo por estacao do ano ----------------------------------------------------
MAE_geral <- BASE_COM %>%
  dplyr::group_by(season, ID) %>%
  dplyr::summarise(
    MAE_chirps = MAE(prec_chirps, prec_br_dwgd), 
    MAE_era5 = MAE(prec_era5, prec_br_dwgd), 
    MAE_persiann = MAE(prec_persiann, prec_br_dwgd),
    .groups = "drop" 
  )
# Calculando MAE por grupo 
MAE_season <- MAE_geral %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    MAE_chirps = mean(MAE_chirps, na.rm=T), 
    MAE_era5 = mean(MAE_era5, na.rm=T), 
    MAE_persiann = mean(MAE_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::filter(!is.na(Periodo))|>
  dplyr::select(Periodo, MAE_chirps, MAE_era5, MAE_persiann) 

# salvando a base completa: 
MAE_BASE <- dplyr::bind_rows(MAE_BASE, MAE_season)
rm(MAE_season)

# Fazendo por estacao do ano e regiao: -----------------------------------------
MAE_geral <- BASE_COM %>%
  dplyr::group_by(name_region, season, ID) %>%
  dplyr::summarise(
    MAE_chirps = MAE(prec_chirps, prec_br_dwgd), 
    MAE_era5 = MAE(prec_era5, prec_br_dwgd), 
    MAE_persiann = MAE(prec_persiann, prec_br_dwgd),#priMAEiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupaMAEnto após o summarize
  )

# Calculando MAE por grupo 
MAE_RE_SE <- MAE_geral %>%
  dplyr::group_by(name_region, season) %>%
  dplyr::summarise(
    MAE_chirps = mean(MAE_chirps, na.rm=T), 
    MAE_era5 = mean(MAE_era5, na.rm=T), 
    MAE_persiann = mean(MAE_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::select(name_region, Periodo, MAE_chirps, MAE_era5, MAE_persiann) |>
  dplyr::filter(!is.na(name_region)) |> dplyr::filter(!is.na(Periodo))
rm(MAE_geral,MAE)

#-------------------------------------------------------------------------------
# Obtendo os PD ----------------------------------------------------------------
#-------------------------------------------------------------------------------
panel_data <- BASE_COM
# Manipulando os dados:
AA<-0.01
panel_data <- panel_data |>
  dplyr::mutate(prec_chirps_I = ifelse(prec_chirps > AA, 1, 0), # fazer para os demais tipo de  dados>
                prec_era5_I = ifelse(prec_era5 > AA, 1, 0),
                prec_persiann_I = ifelse(prec_persiann > AA, 1, 0),
                prec_br_dwgd_I = ifelse(prec_br_dwgd > AA, 1, 0))
# criando indicador:
panel_data <- panel_data |>
  dplyr::mutate(prec_chirps_I = ifelse(prec_chirps_I==prec_br_dwgd_I, 1, 0), # segunda coluna deve de ser o ufes
                prec_era5_I = ifelse(prec_era5_I==prec_br_dwgd_I, 1, 0),
                prec_persiann_I = ifelse(prec_persiann_I==prec_br_dwgd_I, 1, 0))

# function PD=A/(A+C):
PD <- function(variable) {
  # Calcula a soma dos valores, ignorando NA
  soma <- sum(variable, na.rm = TRUE)
  # Conta os casos completos (não-NA)
  casos_completos <- sum(complete.cases(variable))
  # Calcula a média dos valores completos
  media <- soma / casos_completos
  return(media)
}
# Supondo que BASE_COM já esteja carregado e que você queira aplicar PD por grupo em 'prec_chirps'
PD_geral <- panel_data %>%
  dplyr::group_by(name_biome, name_region, ID) %>%
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  )
# Calculando PD por grupo 
PD_completo <- PD_geral %>%
  dplyr::summarise(
    PD_chirps = mean(PD_chirps, na.rm=T), 
    PD_era5 = mean(PD_era5, na.rm=T), 
    PD_persiann = mean(PD_persiann, na.rm=T)) %>%
  dplyr::mutate(Periodo = "Completo") %>%
  dplyr::select(Periodo, PD_chirps, PD_era5, PD_persiann)

PD_base_regiao <- PD_geral %>%
  dplyr::filter(!is.na(name_region))%>%
  dplyr::group_by(name_region) %>%
  dplyr::summarise(
    PD_chirps = mean(PD_chirps, na.rm=T), 
    PD_era5 = mean(PD_era5, na.rm=T), 
    PD_persiann = mean(PD_persiann, na.rm=T)) %>%
  dplyr::ungroup()|>
  dplyr::rename(Periodo = name_region) |>
  dplyr::select(Periodo, PD_chirps, PD_era5, PD_persiann) |>
  dplyr::filter(!is.na(Periodo))

PD_base_bioma <- PD_geral %>%
  dplyr::filter(!is.na(name_biome))%>%
  dplyr::group_by(name_biome) %>%
  dplyr::summarise(
    PD_chirps = mean(PD_chirps, na.rm=T), 
    PD_era5 = mean(PD_era5, na.rm=T), 
    PD_persiann = mean(PD_persiann, na.rm=T)) %>%
  dplyr::ungroup() |>
  dplyr::rename(Periodo = name_biome) |>
  dplyr::select(Periodo, PD_chirps, PD_era5, PD_persiann) |>
  dplyr::filter(Periodo!="Sistema Costeiro")

rm(PD_geral)
PD_BASE <- dplyr::bind_rows(PD_completo, PD_base_regiao, PD_base_bioma)
rm(PD_completo, PD_base_regiao, PD_base_bioma)


# Fazendo por estacao do ano ----------------------------------------------------
PD_geral <- panel_data %>%
  dplyr::group_by(season, ID) %>%
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  )
# Calculando PD por grupo 
PD_season <- PD_geral %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    PD_chirps = mean(PD_chirps, na.rm=T), 
    PD_era5 = mean(PD_era5, na.rm=T), 
    PD_persiann = mean(PD_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::filter(!is.na(Periodo))|>
  dplyr::select(Periodo, PD_chirps, PD_era5, PD_persiann) 

# salvando a base completa: 
PD_BASE <- dplyr::bind_rows(PD_BASE, PD_season)
rm(PD_season)

# Fazendo por estacao do ano e regiao: -----------------------------------------
PD_geral <- panel_data %>%
  dplyr::group_by(name_region, season, ID) %>%
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  )

# Calculando PD por grupo 
PD_RE_SE <- PD_geral %>%
  dplyr::group_by(name_region, season) %>%
  dplyr::summarise(
    PD_chirps = mean(PD_chirps, na.rm=T), 
    PD_era5 = mean(PD_era5, na.rm=T), 
    PD_persiann = mean(PD_persiann, na.rm=T))|>
  dplyr::rename(Periodo = season) |>
  dplyr::select(name_region, Periodo, PD_chirps, PD_era5, PD_persiann) |>
  dplyr::filter(!is.na(name_region)) |> dplyr::filter(!is.na(Periodo))
rm(PD_geral,PD)
#-------------------------------------------------------------------------------
# juntando bases em uma tabela so
#-------------------------------------------------------------------------------
TABELA <- dplyr::full_join(CC_BASE, ME_BASE,by="Periodo")
TABELA <- dplyr::full_join(TABELA, MAE_BASE,by="Periodo")
TABELA <- dplyr::full_join(TABELA, RMSE_BASE,by="Periodo")
TABELA <- dplyr::full_join(TABELA, PD_BASE,by="Periodo")

TABELA <- TABELA |>
  dplyr::filter(!is.na(Periodo))|>
  dplyr::filter(Periodo%in%c("Inverno", "Outono" , "Primavera", "Verao"))|>
  dplyr::mutate(name_region ="Brasil")|>
  dplyr::select(name_region, Periodo, CC_prec_chirps, CC_prec_era5, CC_prec_persiann, ME_chirps, ME_era5,
                ME_persiann, MAE_chirps, MAE_era5, MAE_persiann, RMSE_chirps, RMSE_era5, RMSE_persiann,
                PD_chirps, PD_era5, PD_persiann)

TABELA_1 <- dplyr::full_join(CC_RE_SE, ME_RE_SE,by=c("Periodo","name_region"))
TABELA_1 <- dplyr::full_join(TABELA_1, MAE_RE_SE,by=c("Periodo","name_region"))
TABELA_1 <- dplyr::full_join(TABELA_1, RMSE_RE_SE,by=c("Periodo","name_region"))
TABELA_1 <- dplyr::full_join(TABELA_1, PD_RE_SE,by=c("Periodo","name_region"))

# juntando as tabelas : 

TABE <- dplyr::bind_rows(TABELA, TABELA_1)
rm(AA, BASE_COM, CC_BASE, CC_RE_SE, dicio_reg, estado, filtro_hexa,
   hex_grid_sf, MAE_BASE, MAE_RE_SE, ME_BASE, ME_RE_SE, panel_data,  
   PD_BASE, PD_RE_SE, RMSE_BASE, RMSE_RE_SE)

TABE|> dplyr::glimpse()

TABE1 <- TABE |>
  tidyr::pivot_longer(
    cols = c(CC_prec_chirps, CC_prec_era5, CC_prec_persiann, ME_chirps, ME_era5,
             ME_persiann, MAE_chirps, MAE_era5, MAE_persiann, RMSE_chirps, RMSE_era5, RMSE_persiann,
             PD_chirps, PD_era5, PD_persiann),
    names_to = "Produto",
    values_to = "Values"
  )


openxlsx::write.xlsx(TABE,file = "results/tabela_3A.xlsx")
##-------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvanced)
library(tidyverse)
library(fmsb)
library(ggspider)
library(ggvanced)
# importando dados:
TABE <- read_excel("results/tabela_3A.xlsx")

# realizando transformacao:
TABE1 <- TABE |>
  tidyr::pivot_longer(
    cols = c(CC_prec_chirps, CC_prec_era5, CC_prec_persiann, ME_chirps, ME_era5,
             ME_persiann, MAE_chirps, MAE_era5, MAE_persiann, RMSE_chirps, RMSE_era5, RMSE_persiann,
             PD_chirps, PD_era5, PD_persiann),
    names_to = "Produto",
    values_to = "Values"
  ) |>
  mutate(name_region = ifelse(name_region=="Centro Oeste", "CO",
                              ifelse(name_region=="Brasil","BR",
                                     ifelse(name_region=="Norte","N",
                                            ifelse(name_region=="Nordeste","NE",
                                                   ifelse(name_region=="Sudeste", "SE",
                                                          ifelse(name_region=="Sul","S",name_region)))))))

# Manipulacao para o CC ------------------------------------------------------

# Fazendo alteracoes
tabe2 <- TABE1 |>
  tidyr::pivot_wider(names_from = "name_region",
                     values_from = "Values") |>
  filter(Produto %in% c("CC_prec_chirps", "CC_prec_era5", "CC_prec_persiann")) |>
  mutate(Produto = ifelse(Produto=="CC_prec_chirps","CHIRPS",
                          ifelse(Produto=="CC_prec_era5", "ERA5", 
                                 ifelse(Produto=="CC_prec_persiann", "PERSIANN",Produto))))

# Adicionar linhas de zeros e uns
tabe2 <- tabe2 %>%
  bind_rows(
    tibble(
      Produto = "Zeros",
      BR = 0,
      CO = 0,
      NE = 0,
      N = 0,
      SE = 0,
      S = 0
    ),
    tibble(
      Produto = "Ones",
      BR = 1,
      CO = 1,
      NE = 1,
      N = 1,
      SE = 1,
      S = 1
    )
  )

cc_Inverno <-tabe2 %>%
  dplyr::filter( Periodo == "Inverno" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider(       axis_name_font_size = 4,                
                  axis_label_font_size = 3.5,                  
                  axis_label_font_face="bold",
                  subset = c("CHIRPS", "ERA5", "PERSIANN"), 
                  central_distance = 0.1,
                  axis_name_offset = 0.1,
                  draw_axis =T,
                  n_labels = 5,
                  background_color = "gray99",
                  fill_opacity = 0.03,
                  polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "Winter")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()+
  annotate("text", 
           x = -1.1, y = 0,          
           label = "CC", 
           angle = 90,                
           size = 5,                 
           fontface = "bold") 

cc_Verao <-tabe2 %>%
  dplyr::filter( Periodo == "Verao" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,              
            axis_label_font_size = 3.5,              
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "Summer")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

cc_Primavera <-tabe2 %>%
  dplyr::filter( Periodo == "Primavera" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,                
            axis_label_font_size = 3.5,               
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "Spring")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

cc_Outono <-tabe2 %>%
  dplyr::filter( Periodo == "Outono" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,                 
            axis_label_font_size = 3.5,                 
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "Autumn")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

CC_estacao <- ggpubr::ggarrange(cc_Inverno, cc_Primavera, cc_Verao, cc_Outono,
                                ncol = 4, nrow = 1)


# Manipulacao para o ME ------------------------------------------------------
TABE1 |> dplyr::filter(Produto %in% c("ME_chirps", "ME_era5", "ME_persiann"))|>
  summarise(minimo = min(Values,na.rm = T),
            maximo = max(Values,na.rm = T)) |> View()
# Fazendo alteracoes
tabe3 <- TABE1 |>
  tidyr::pivot_wider(names_from = "name_region",
                     values_from = "Values") |>
  filter(Produto %in% c("ME_chirps", "ME_era5", "ME_persiann")) |>
  mutate(Produto = ifelse(Produto=="ME_chirps","CHIRPS",
                          ifelse(Produto=="ME_era5", "ERA5", 
                                 ifelse(Produto=="ME_persiann", "PERSIANN",Produto))))

# Adicionar linhas de zeros
tabe3 <- tabe3 %>%
  bind_rows(
    tibble(
      Produto = "Zeros",
      BR = -1,
      CO = -1,
      NE = -1,
      N = -1,
      SE = -1,
      S = -1
    ),
    tibble(
      Produto = "Ones",
      BR = 1,
      CO = 1,
      NE = 1,
      N = 1,
      SE = 1,
      S = 1
    )
  )

me_Inverno <-tabe3 %>%
  dplyr::filter( Periodo == "Inverno" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider(       axis_name_font_size = 4,                 
                  axis_label_font_size = 3.5,                 
                  axis_label_font_face="bold",
                  subset = c("CHIRPS", "ERA5", "PERSIANN"), 
                  central_distance = 0.1,
                  axis_name_offset = 0.1,
                  draw_axis =T,
                  n_labels = 5,
                  background_color = "gray99",
                  fill_opacity = 0.03,
                  polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()+
  annotate("text", 
           x = -1.1, y = 0,          
           label = "ME", 
           angle = 90,                
           size = 5,                 
           fontface = "bold") 

me_Verao <-tabe3 %>%
  dplyr::filter( Periodo == "Verao" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,                 
            axis_label_font_size = 3.5,                  
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

me_Primavera <-tabe3 %>%
  dplyr::filter( Periodo == "Primavera" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,                 
            axis_label_font_size = 3.5,                  
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

me_Outono <-tabe3 %>%
  dplyr::filter( Periodo == "Outono" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,                
            axis_label_font_size = 3.5,                
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

me_estacao <- ggpubr::ggarrange(me_Inverno, me_Primavera, me_Verao, me_Outono,
                                ncol = 4, nrow = 1)
# Manipulacao para o MAE ------------------------------------------------------
TABE1 |> dplyr::filter(Produto %in% c("MAE_chirps", "MAE_era5", "MAE_persiann"))|>
  summarise(minimo = min(Values,na.rm = T),
            maximo = max(Values,na.rm = T)) |> View()
# Fazendo alteracoes
tabe4 <- TABE1 |>
  tidyr::pivot_wider(names_from = "name_region",
                     values_from = "Values") |>
  filter(Produto %in% c("MAE_chirps", "MAE_era5", "MAE_persiann")) |>
  mutate(Produto = ifelse(Produto=="MAE_chirps","CHIRPS",
                          ifelse(Produto=="MAE_era5", "ERA5", 
                                 ifelse(Produto=="MAE_persiann", "PERSIANN",Produto))))

# Adicionar linhas de zeros
tabe4 <- tabe4 %>%
  bind_rows(
    tibble(
      Produto = "Zeros",
      BR = 0,
      CO = 0,
      NE = 0,
      N = 0,
      SE = 0,
      S = 0
    ),
    tibble(
      Produto = "Ones",
      BR = 10,
      CO = 10,
      NE = 10,
      N = 10,
      SE = 10,
      S = 10
    )
  )

mae_Inverno <-tabe4 %>%
  dplyr::filter( Periodo == "Inverno" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider(       axis_name_font_size = 4,      
                  axis_label_font_size = 3.5,      
                  axis_label_font_face="bold",
                  subset = c("CHIRPS", "ERA5", "PERSIANN"), 
                  central_distance = 0.1,
                  axis_name_offset = 0.1,
                  draw_axis =T,
                  n_labels = 5,
                  background_color = "gray99",
                  fill_opacity = 0.03,
                  polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()+
  annotate("text", 
           x = -1.1, y = 0,          
           label = "MAE", 
           angle = 90,                
           size = 5,                 
           fontface = "bold") 

mae_Verao <-tabe4 %>%
  dplyr::filter( Periodo == "Verao" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4, 
            axis_label_font_size = 3.5,    
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

mae_Primavera <-tabe4 %>%
  dplyr::filter( Periodo == "Primavera" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,              
            axis_label_font_size = 3.5,           
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

mae_Outono <-tabe4 %>%
  dplyr::filter( Periodo == "Outono" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,                
            axis_label_font_size = 3.5,                     
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

mae_estacao <- ggpubr::ggarrange(mae_Inverno, mae_Primavera, mae_Verao, mae_Outono,
                                 ncol = 4, nrow = 1)
# Manipulacao para o RMSE ------------------------------------------------------
TABE1 |> dplyr::filter(Produto %in% c("RMSE_chirps", "RMSE_era5", "RMSE_persiann"))|>
  summarise(minimo = min(Values,na.rm = T),
            maximo = max(Values,na.rm = T)) |> View()
# Fazendo alteracoes
tabe5 <- TABE1 |>
  tidyr::pivot_wider(names_from = "name_region",
                     values_from = "Values") |>
  filter(Produto %in% c("RMSE_chirps", "RMSE_era5", "RMSE_persiann")) |>
  mutate(Produto = ifelse(Produto=="RMSE_chirps","CHIRPS",
                          ifelse(Produto=="RMSE_era5", "ERA5", 
                                 ifelse(Produto=="RMSE_persiann", "PERSIANN",Produto))))

# Adicionar linhas de zeros
tabe5 <- tabe5 %>%
  bind_rows(
    tibble(
      Produto = "Zeros",
      BR = 0,
      CO = 0,
      NE = 0,
      N = 0,
      SE = 0,
      S = 0
    ),
    tibble(
      Produto = "Ones",
      BR = 13,
      CO = 13,
      NE = 13,
      N = 13,
      SE = 13,
      S = 13
    )
  )

rmse_Inverno <-tabe5 %>%
  dplyr::filter( Periodo == "Inverno" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider(       axis_name_font_size = 4,          
                  axis_label_font_size = 3.5,    
                  axis_label_font_face="bold",
                  subset = c("CHIRPS", "ERA5", "PERSIANN"), 
                  central_distance = 0.1,
                  axis_name_offset = 0.1,
                  draw_axis =T,
                  n_labels = 5,
                  background_color = "gray99",
                  fill_opacity = 0.03,
                  polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()+
  annotate("text", 
           x = -1.1, y = 0,          
           label = "RMSE", 
           angle = 90,                
           size = 5,                 
           fontface = "bold") 

rmse_Verao <-tabe5 %>%
  dplyr::filter( Periodo == "Verao" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,                  
            axis_label_font_size = 3.5,                  
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

rmse_Primavera <-tabe5 %>%
  dplyr::filter( Periodo == "Primavera" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,        
            axis_label_font_size = 3.5,        
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

rmse_Outono <-tabe5 %>%
  dplyr::filter( Periodo == "Outono" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,       
            axis_label_font_size = 3.5,        
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

rmse_estacao <- ggpubr::ggarrange(rmse_Inverno, rmse_Primavera, rmse_Verao, rmse_Outono,
                                  ncol = 4, nrow = 1)
# Manipulacao para o PD ------------------------------------------------------
TABE1 |> dplyr::filter(Produto %in% c("PD_chirps", "PD_era5", "PD_persiann"))|>
  summarise(minimo = min(Values,na.rm = T),
            maximo = max(Values,na.rm = T)) |> View()
# Fazendo alteracoes
tabe6 <- TABE1 |>
  tidyr::pivot_wider(names_from = "name_region",
                     values_from = "Values") |>
  filter(Produto %in% c("PD_chirps", "PD_era5", "PD_persiann")) |>
  mutate(Produto = ifelse(Produto=="PD_chirps","CHIRPS",
                          ifelse(Produto=="PD_era5", "ERA5", 
                                 ifelse(Produto=="PD_persiann", "PERSIANN",Produto))))|>
  dplyr::rename(`SSP:` = Produto)

# Adicionar linhas de zeros
tabe6 <- tabe6 %>%
  bind_rows(
    tibble(
      `SSP:` = "Zeros",
      BR = 0,
      CO = 0,
      NE = 0,
      N = 0,
      SE = 0,
      S = 0
    ),
    tibble(
      `SSP:` = "Ones",
      BR = 1,
      CO = 1,
      NE = 1,
      N = 1,
      SE = 1,
      S = 1
    )
  )

pd_Inverno <-tabe6 %>%
  dplyr::filter( Periodo == "Inverno" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider(       axis_name_font_size = 4,      
                  axis_label_font_size = 3.5, 
                  axis_label_font_face="bold",
                  subset = c("CHIRPS", "ERA5", "PERSIANN"), 
                  central_distance = 0.1,
                  axis_name_offset = 0.1,
                  draw_axis =T,
                  n_labels = 5,
                  background_color = "gray99",
                  fill_opacity = 0.03,
                  polygon = TRUE)+  
  labs(x = "PD", y = "PD", fill = "", title = "")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()+
  annotate("text", 
           x = -1.1, y = 0,          
           label = "PD", 
           angle = 90,                
           size = 5,                 
           fontface = "bold") 

pd_Verao <-tabe6 %>%
  dplyr::filter( Periodo == "Verao" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,                     
            axis_label_font_size = 3.5,                     
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

pd_Primavera <-tabe6 %>%
  dplyr::filter( Periodo == "Primavera" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4,               
            axis_label_font_size = 3.5,   
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

pd_Outono <-tabe6 %>%
  dplyr::filter( Periodo == "Outono" | is.na(Periodo)) %>% select(-Periodo) %>%
  ggspider( axis_name_font_size = 4, 
            axis_label_font_size = 3.5,   
            axis_label_font_face="bold",
            subset = c("CHIRPS", "ERA5", "PERSIANN"), 
            central_distance = 0.1,
            draw_axis =T,
            n_labels = 5,
            background_color = "gray99",
            fill_opacity = 0.03,
            polygon = TRUE)+  
  labs(x = "", y = "", fill = "", title = "")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()

pd_estacao <- ggpubr::ggarrange(pd_Inverno, pd_Primavera, pd_Verao, pd_Outono,
                                ncol = 4, nrow = 1, common.legend = T ,legend = c("bottom"))
#-----------------------------------------------------------------------------
# juntando os mapas:
figuras_brasil <- ggpubr::ggarrange(CC_estacao, me_estacao, mae_estacao, rmse_estacao, pd_estacao,
                                    ncol = 1, nrow = 5, common.legend = T ,legend = c("bottom"))


ggsave("results/figura_8_en.png", plot = figuras_brasil,
       width = 11, height = 16, dpi = 300)

