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
BASE_COM |> dplyr::glimpse()
#-------------------------------------------------------------------------------
# Calculando CC para cada grupo de CLASE
#-------------------------------------------------------------------------------
# Definir a função para calcular a correlação
calculate_correlation <- function(data, compare_var) {
  data %>%
    summarise(
      correlation = if (all(is.na(.data[[compare_var]])) | all(is.na(.data[["prec_br_dwgd"]]))) {
        NA_real_
      } else {
        valid_data <- complete.cases(.data[[compare_var]], .data[["prec_br_dwgd"]])
        if (sum(valid_data) > 1) {
          tryCatch(
            cor(.data[[compare_var]][valid_data], .data[["prec_br_dwgd"]][valid_data], use = "complete.obs"),
            error = function(e) NA_real_
          )
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    ) %>%
    pull(correlation)
}


# Calculando CC por grupo
CC_completo <- BASE_COM %>%
  dplyr::summarise(
    CC_chirps = calculate_correlation(cur_data(), "prec_chirps"), 
    CC_era5 = calculate_correlation(cur_data(), "prec_era5"), 
    CC_persiann = calculate_correlation(cur_data(), "prec_persiann"),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

CC_base_season <- BASE_COM %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    CC_chirps = calculate_correlation(cur_data(), "prec_chirps"), 
    CC_era5 = calculate_correlation(cur_data(), "prec_era5"), 
    CC_persiann = calculate_correlation(cur_data(), "prec_persiann"),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

CC_base_regiao <- BASE_COM %>%
  dplyr::filter(!is.na(name_region))%>%
  dplyr::group_by(name_region) %>%
  dplyr::summarise(
    CC_chirps = calculate_correlation(cur_data(), "prec_chirps"), 
    CC_era5 = calculate_correlation(cur_data(), "prec_era5"), 
    CC_persiann = calculate_correlation(cur_data(), "prec_persiann"),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

CC_base_bioma <- BASE_COM %>%
  dplyr::filter(!is.na(name_biome))%>%
  dplyr::group_by(name_biome) %>%
  dplyr::summarise(
    CC_chirps = calculate_correlation(cur_data(), "prec_chirps"), 
    CC_era5 = calculate_correlation(cur_data(), "prec_era5"), 
    CC_persiann = calculate_correlation(cur_data(), "prec_persiann"),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )


#junatando as bases
CC_base <- dplyr::bind_rows(dplyr::mutate(CC_completo,Periodo="Completo"),
                            dplyr::rename(CC_base_regiao,Periodo=name_region),
                            dplyr::rename(CC_base_bioma,Periodo=name_biome),
                            dplyr::rename(CC_base_season,Periodo=season))

rm(CC_completo, CC_base_season, CC_base_regiao, CC_base_bioma)
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

# Calculando RMSE por grupo
RMSE_completo <- BASE_COM %>%
  dplyr::summarise(
    RMSE_chirps = RMSE(prec_chirps, prec_br_dwgd), 
    RMSE_era5 = RMSE(prec_era5, prec_br_dwgd), 
    RMSE_persiann = RMSE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

RMSE_base_season <- BASE_COM %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    RMSE_chirps = RMSE(prec_chirps, prec_br_dwgd), 
    RMSE_era5 = RMSE(prec_era5, prec_br_dwgd), 
    RMSE_persiann = RMSE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

RMSE_base_regiao <- BASE_COM %>%
  dplyr::filter(!is.na(name_region))%>%
  dplyr::group_by(name_region) %>%
  dplyr::summarise(
    RMSE_chirps = RMSE(prec_chirps, prec_br_dwgd), 
    RMSE_era5 = RMSE(prec_era5, prec_br_dwgd), 
    RMSE_persiann = RMSE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

RMSE_base_bioma <- BASE_COM %>%
  dplyr::filter(!is.na(name_biome))%>%
  dplyr::group_by(name_biome) %>%
  dplyr::summarise(
    RMSE_chirps = RMSE(prec_chirps, prec_br_dwgd), 
    RMSE_era5 = RMSE(prec_era5, prec_br_dwgd), 
    RMSE_persiann = RMSE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )


#junatando as bases
RMSE_base <- dplyr::bind_rows(dplyr::mutate(RMSE_completo,Periodo="Completo"),
                              dplyr::rename(RMSE_base_regiao,Periodo=name_region),
                              dplyr::rename(RMSE_base_bioma,Periodo=name_biome),
                              dplyr::rename(RMSE_base_season,Periodo=season))

rm(RMSE_completo, RMSE_base_season, RMSE_base_regiao, RMSE_base_bioma)
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
# Calculando ME por grupo
ME_completo <- BASE_COM %>%
  dplyr::summarise(
    ME_chirps = ME(prec_chirps, prec_br_dwgd), 
    ME_era5 = ME(prec_era5, prec_br_dwgd), 
    ME_persiann = ME(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

ME_base_season <- BASE_COM %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    ME_chirps = ME(prec_chirps, prec_br_dwgd), 
    ME_era5 = ME(prec_era5, prec_br_dwgd), 
    ME_persiann = ME(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

ME_base_regiao <- BASE_COM %>%
  dplyr::filter(!is.na(name_region))%>%
  dplyr::group_by(name_region) %>%
  dplyr::summarise(
    ME_chirps = ME(prec_chirps, prec_br_dwgd), 
    ME_era5 = ME(prec_era5, prec_br_dwgd), 
    ME_persiann = ME(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

ME_base_bioma <- BASE_COM %>%
  dplyr::filter(!is.na(name_biome))%>%
  dplyr::group_by(name_biome) %>%
  dplyr::summarise(
    ME_chirps = ME(prec_chirps, prec_br_dwgd), 
    ME_era5 = ME(prec_era5, prec_br_dwgd), 
    ME_persiann = ME(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )


#junatando as bases
ME_base <- dplyr::bind_rows(dplyr::mutate(ME_completo,Periodo="Completo"),
                            dplyr::rename(ME_base_regiao,Periodo=name_region),
                            dplyr::rename(ME_base_bioma,Periodo=name_biome),
                            dplyr::rename(ME_base_season,Periodo=season))
rm(ME_completo, ME_base_season, ME_base_regiao, ME_base_bioma)
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
# Calculando MAE por grupo
MAE_completo <- BASE_COM %>%
  dplyr::summarise(
    MAE_chirps = MAE(prec_chirps, prec_br_dwgd), 
    MAE_era5 = MAE(prec_era5, prec_br_dwgd), 
    MAE_persiann = MAE(prec_persiann, prec_br_dwgd),#priMAEiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupaMAEnto após o summarize
  )

MAE_base_season <- BASE_COM %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    MAE_chirps = MAE(prec_chirps, prec_br_dwgd), 
    MAE_era5 = MAE(prec_era5, prec_br_dwgd), 
    MAE_persiann = MAE(prec_persiann, prec_br_dwgd),#priMAEiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupaMAEnto após o summarize
  )

MAE_base_regiao <- BASE_COM %>%
  dplyr::filter(!is.na(name_region))%>%
  dplyr::group_by(name_region) %>%
  dplyr::summarise(
    MAE_chirps = MAE(prec_chirps, prec_br_dwgd), 
    MAE_era5 = MAE(prec_era5, prec_br_dwgd), 
    MAE_persiann = MAE(prec_persiann, prec_br_dwgd),#priMAEiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupaMAEnto após o summarize
  )

MAE_base_bioma <- BASE_COM %>%
  dplyr::filter(!is.na(name_biome))%>%
  dplyr::group_by(name_biome) %>%
  dplyr::summarise(
    MAE_chirps = MAE(prec_chirps, prec_br_dwgd), 
    MAE_era5 = MAE(prec_era5, prec_br_dwgd), 
    MAE_persiann = MAE(prec_persiann, prec_br_dwgd),#priMAEiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupaMAEnto após o summarize
  )


#junatando as bases
MAE_base <- dplyr::bind_rows(dplyr::mutate(MAE_completo,Periodo="Completo"),
                             dplyr::rename(MAE_base_regiao,Periodo=name_region),
                             dplyr::rename(MAE_base_bioma,Periodo=name_biome),
                             dplyr::rename(MAE_base_season,Periodo=season))
rm(MAE_completo, MAE_base_season, MAE_base_regiao, MAE_base_bioma)
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
PD_completo <- panel_data %>%
  summarise(
    PD_prec_chirps = PD(prec_chirps_I),
    PD_prec_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I)
  )

PD_season <- panel_data %>%
  group_by(season) %>%
  summarise(
    PD_prec_chirps = PD(prec_chirps_I),
    PD_prec_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

PD_bioma <- panel_data %>%
  dplyr::filter(!is.na(name_biome))%>%
  dplyr::group_by(name_biome) %>%
  summarise(
    PD_prec_chirps = PD(prec_chirps_I),
    PD_prec_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

PD_region <- panel_data %>%
  dplyr::filter(!is.na(name_region))%>%
  dplyr::group_by(name_region) %>%
  summarise(
    PD_prec_chirps = PD(prec_chirps_I),
    PD_prec_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
#junatando as bases
PD_base <- dplyr::bind_rows(dplyr::mutate(PD_completo, Periodo="Completo"),
                            dplyr::rename(PD_bioma, Periodo=name_biome),
                            dplyr::rename(PD_region, Periodo=name_region),
                            dplyr::rename(PD_season, Periodo=season))
rm(PD_completo, PD_bioma, PD_region, PD_season)
#-------------------------------------------------------------------------------
# juntando bases em uma tabela so
#-------------------------------------------------------------------------------
CC_base <- CC_base |> dplyr::select(Periodo, CC_chirps, CC_era5, CC_persiann)
TABELA <- dplyr::full_join(CC_base, ME_base,by="Periodo")
TABELA <- dplyr::full_join(TABELA, MAE_base,by="Periodo")
TABELA <- dplyr::full_join(TABELA, RMSE_base,by="Periodo")
TABELA <- dplyr::full_join(TABELA, PD_base,by="Periodo")
openxlsx::write.xlsx(TABELA,file = "results/tabela_2A.xlsx")
# deletando
rm(CC_base, ME_base, MAE_base, RMSE_base, PD_base, TABELA)