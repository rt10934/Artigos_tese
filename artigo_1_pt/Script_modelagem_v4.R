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
library(ggspatial)
rm(list = ls())
#-------------------------------------------------------------------------------
# Imortacao dos dados ----------------------------------------------------------
#-------------------------------------------------------------------------------
hex_grid_sf <- readRDS("data_1/shapefile_18_1.rds") |>
  dplyr::rename(ID = CLASE)
estado <- geobr::read_state()
BASE_COM <- readRDS("data_1/base_prec.rds")
#-------------------------------------------------------------------------------
# Calculando a correlatio para cada grupo de CLASE
#-------------------------------------------------------------------------------

# Função para calcular correlação entre duas variáveis
calculate_correlation <- function(data, target_var, compare_var) {
  data %>%
    group_by(ID) %>%
    summarise(
      correlation = if (all(is.na(.data[[compare_var]])) | all(is.na(.data[[target_var]]))) {
        NA_real_
      } else {
        valid_data <- complete.cases(.data[[compare_var]], .data[[target_var]])
        if (sum(valid_data) > 1) {
          tryCatch(
            cor(.data[[compare_var]][valid_data], .data[[target_var]][valid_data], use = "complete.obs"),
            error = function(e) NA_real_
          )
        } else {
          NA_real_
        }
      },
      .groups = "drop"  # Descarta o agrupamento após o summarize
    ) %>%
    rename(!!compare_var := correlation)
}


# Calcular correlação para prec_chirps
correlation_chirps <- calculate_correlation(BASE_COM, "prec_br_dwgd", "prec_chirps")

# Calcular correlação para prec_era5
correlation_era5 <- calculate_correlation(BASE_COM, "prec_br_dwgd", "prec_era5")

# Calcular correlação para prec_persiann
correlation_persiann <- calculate_correlation(BASE_COM, "prec_br_dwgd", "prec_persiann")

# Juntar os resultados em um único data frame
correlation_by_clase <- correlation_chirps %>%
  left_join(correlation_era5, by = "ID") %>%
  left_join(correlation_persiann, by = "ID")

# Merge
correlation_by_clase <- dplyr::full_join(hex_grid_sf, correlation_by_clase, 
                                         by="ID")
correlation_by_clase |> dplyr::glimpse()


# Função para criar o gráfico de correlação
create_correlation_plot <- function(data, correlation_var, title) {
  data %>%
    mutate(
      Categoria = cut(
        .data[[correlation_var]],
        breaks = c(0, 0.25, 0.30, 0.35, 0.40, 0.45, 0.5, 0.55, 0.6, 0.65, 0.70, 0.75, 0.9),
        labels = c("[0 - 0,25]", "[0,25 - 0,30]", "[0,30 - 0,35]", "[0,35 - 0,40]",
                   "[0,40 - 0,45]", "[0,45 - 0,5]", "[0,5 - 0,55]", "[0,55 - 0,6]",
                   "[0,6 - 0,65]", "[0,65 - 0,7]", "[0,7 - 0,75]", "[0,75 - 0,9]"),
        include.lowest = TRUE
      )
    ) %>%
    mutate(
      Categoria = factor(Categoria, levels = c(
        "[0 - 0,25]", "[0,25 - 0,30]", "[0,30 - 0,35]", "[0,35 - 0,40]",
        "[0,40 - 0,45]", "[0,45 - 0,5]", "[0,5 - 0,55]", "[0,55 - 0,6]",
        "[0,6 - 0,65]", "[0,65 - 0,7]", "[0,7 - 0,75]", "[0,75 - 0,9]"
      ))
    ) %>%
    ggplot() +
    geom_sf(aes(fill = Categoria), color = NA, show.legend = TRUE) +
    ggtitle(title) +
    labs(fill = "CC") +
    scale_fill_manual(values = c(
      "[0 - 0,25]" = "#F1EDEC", "[0,25 - 0,30]" = "#E6D2CC",
      "[0,30 - 0,35]" = "#DDB7AB", "[0,35 - 0,40]" = "#D59C88",
      "[0,40 - 0,45]" = "#CD8168", "[0,45 - 0,5]" = "#C56749",
      "[0,5 - 0,55]" = "#BB4A30", "[0,55 - 0,6]" = "#AD2C24",
      "[0,6 - 0,65]" = "#971428", "[0,65 - 0,7]" = "#780E28",
      "[0,7 - 0,75]" = "#5A0E1F", "[0,75 - 0,9]" = "#3C0912"
    )) +
    annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))+
    geom_sf(data = estado, aes(), color = "#151D44", fill = NA)
}

# Lista das variáveis de correlação e títulos

CC_chirps <- create_correlation_plot(correlation_by_clase, "prec_chirps", "CHIRPS")
CC_era5 <- create_correlation_plot(correlation_by_clase, "prec_era5", "ERA5")
CC_persiann <- create_correlation_plot(correlation_by_clase, "prec_persiann", "PERSIANN")
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
RMSE_base <- BASE_COM %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    RMSE_chirps = RMSE(prec_chirps, prec_br_dwgd), 
    RMSE_era5 = RMSE(prec_era5, prec_br_dwgd), 
    RMSE_persiann = RMSE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
# Merge
RMSE_base <- dplyr::full_join(hex_grid_sf, RMSE_base, by="ID")

# Função para criar o gráfico
create_rmse_plot <- function(data, rmse_var, title) {
  data %>%
    mutate(
      Categoria = cut(
        .data[[rmse_var]],
        breaks = c(0, 5, 7.5, 10, 12.5, 15, 17.5, 100),
        labels = c("[0 - 5]", "[5 - 7.5]", "[7.5 - 10]", "[10 - 12.5]",
                   "[12.5 - 15]", "[15 - 17.5]", "[17.5>]"),
        include.lowest = TRUE
      )
    ) %>%
    mutate(
      Categoria = factor(Categoria, levels = c(
        "[0 - 5]", "[5 - 7.5]", "[7.5 - 10]", "[10 - 12.5]",
        "[12.5 - 15]", "[15 - 17.5]", "[17.5>]"
      ))
    ) %>%
    ggplot() +
    geom_sf(aes(fill = Categoria), color = NA, show.legend = TRUE) +
    ggtitle(title) +
    labs(fill = "RMSE") +
    scale_fill_manual(values = c(
      "[0 - 5]" = "#F1EDEC", "[5 - 7.5]" = "#DDB7AB",
      "[7.5 - 10]" = "#D59C88", "[10 - 12.5]" = "#CD8168",
      "[12.5 - 15]" = "#AD2C24", "[15 - 17.5]" = "#971428",
      "[17.5>]" = "#3C0912"
    )) +
    annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))+
    geom_sf(data = estado, aes(), color = "#151D44", fill = NA)
}

# Criar os gráficos para cada conjunto de dados
rmse_chirps <- create_rmse_plot(RMSE_base, "RMSE_chirps", "CHIRPS")
rmse_era5 <- create_rmse_plot(RMSE_base, "RMSE_era5", "ERA5")
rmse_persiann <- create_rmse_plot(RMSE_base, "RMSE_persiann", "PERSIANN")
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
ME_base <- BASE_COM %>%
  group_by(ID) %>%
  summarise(
    ME_chirps = ME(prec_chirps, prec_br_dwgd), 
    ME_era5 = ME(prec_era5, prec_br_dwgd), 
    ME_persiann = ME(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
# Merge
ME_base <- dplyr::full_join(hex_grid_sf, ME_base, by="ID")
ME_base |> dplyr::glimpse()

# Função para criar mapas de ME
create_me_map <- function(data, me_var, title) {
  data %>%
    mutate(
      Categoria = cut(
        .data[[me_var]],
        breaks = c(-1000, -10, -5, -2.5, 0, 2.5, 5, 10, 1000),
        labels = c("[< -10]", "[-10 -| -5]", "[-5 -| -2.5]", "[-2.5 -| 0]",
                   "[0 -| 2.5]", "[2.5 -| 5]", "[5 -| 10]", "[10 >]"),
        include.lowest = TRUE
      )
    ) %>%
    mutate(
      Categoria = factor(Categoria, levels = c(
        "[< -10]", "[-10 -| -5]", "[-5 -| -2.5]", "[-2.5 -| 0]",
        "[0 -| 2.5]", "[2.5 -| 5]", "[5 -| 10]", "[10 >]"
      ))
    ) %>%
    ggplot() +
    geom_sf(aes(fill = Categoria), color = NA, show.legend = TRUE) +
    ggtitle(title) +
    labs(fill = "ME") +
    scale_fill_manual(values = c(
      "[< -10]"="#D73027","[-10 -| -5]"="#F46D43",
      "[-5 -| -2.5]"="#FDAE61","[-2.5 -| 0]"="#FEE090",
      "[0 -| 2.5]"="#E0F3F8","[2.5 -| 5]"="#ABD9E9",
      "[5 -| 10]"="#74ADD1","[10 >]"="#4575B4"
    ), drop = FALSE) +
    annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))+
    geom_sf(data = estado, aes(), color = "#221F1B", fill = NA)
}

# Aplicar a função para criar os mapas
me_chirps <- create_me_map(ME_base, "ME_chirps", "CHIRPS")
me_era5 <- create_me_map(ME_base, "ME_era5", "ERA5")
me_persiann <- create_me_map(ME_base, "ME_persiann", "PERSIANN")
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

# Calculando ME por grupo
MAE_base <- BASE_COM %>%
  group_by(ID) %>%
  summarise(
    MAE_chirps = MAE(prec_chirps, prec_br_dwgd), 
    MAE_era5 = MAE(prec_era5, prec_br_dwgd), 
    MAE_persiann = MAE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
# Merge
MAE_base <- dplyr::full_join(hex_grid_sf, MAE_base, by="ID")


# Função para criar mapas de MAE
create_mae_map <- function(data, mae_var, title) {
  data %>%
    mutate(
      Categoria = cut(
        .data[[mae_var]],
        breaks = c(0, 2.5, 5, 7.5, 10, 12.5, 15, 100),
        labels = c("[0 - 2.5]", "[2.5 - 5]", "[5 - 7.5]", "[7.5 - 10]",
                   "[10 - 12.5]", "[12.5 - 15]", "[15>]"),
        include.lowest = TRUE
      )
    ) %>%
    mutate(
      Categoria = factor(Categoria, levels = c(
        "[0 - 2.5]", "[2.5 - 5]", "[5 - 7.5]", "[7.5 - 10]",
        "[10 - 12.5]", "[12.5 - 15]", "[15>]"
      ))
    ) %>%
    ggplot() +
    geom_sf(aes(fill = Categoria), color = NA, show.legend = TRUE) +
    ggtitle(title) +
    labs(fill = "MAE") +
    scale_fill_manual(values = c(
      "[0 - 2.5]" = "#F1EDEC", "[2.5 - 5]" = "#DDB7AB",
      "[5 - 7.5]" = "#D59C88", "[7.5 - 10]" = "#CD8168",
      "[10 - 12.5]" = "#AD2C24", "[12.5 - 15]" = "#971428",
      "[15>]" = "#3C0912"
    )) +
    annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))+
    geom_sf(data = estado, aes(), color = "#151D44", fill = NA)
}

# Aplicar a função para criar os mapas
mae_chirps <- create_mae_map(MAE_base, "MAE_chirps", "CHIRPS")
mae_era5 <- create_mae_map(MAE_base, "MAE_era5", "ERA5")
mae_persiann <- create_mae_map(MAE_base, "MAE_persiann", "PERSIANN")

#-------------------------------------------------------------------------------
# Calculando a PD para cada grupo de CLASE
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
resultados <- panel_data %>%
  group_by(ID) %>%
  summarise(
    PD_prec_chirps = PD(prec_chirps_I),
    PD_prec_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )
# Merge
resultados <- dplyr::full_join(hex_grid_sf, resultados, by="ID")
# Função para criar mapas de PD
create_pd_map <- function(data, pd_var, title) {
  data %>%
    mutate(
      Categoria = cut(
        .data[[pd_var]],
        breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
        labels = c("[0 - 0.1]", "[0.1 - 0.2]", "[0.2 - 0.3]", "[0.3 - 0.4]",
                   "[0.4 - 0.5]", "[0.5 - 0.6]", "[0.6 - 0.7]", "[0.7 - 0.8]",
                   "[0.8 - 0.9]", "[0.9 - 1]"),
        include.lowest = TRUE
      )
    ) %>%
    mutate(
      Categoria = factor(Categoria, levels = c(
        "[0 - 0.1]", "[0.1 - 0.2]", "[0.2 - 0.3]", "[0.3 - 0.4]",
        "[0.4 - 0.5]", "[0.5 - 0.6]", "[0.6 - 0.7]", "[0.7 - 0.8]",
        "[0.8 - 0.9]", "[0.9 - 1]"
      ))
    ) %>%
    ggplot() +
    geom_sf(aes(fill = Categoria), color = NA, show.legend = TRUE) +
    ggtitle(title) +
    labs(fill = "PD") +
    scale_fill_manual(values = c(
      "[0 - 0.1]" = "#F1EDEC", "[0.1 - 0.2]" = "#E6D2CC",
      "[0.2 - 0.3]" = "#DDB7AB", "[0.3 - 0.4]" = "#D59C88",
      "[0.4 - 0.5]" = "#CD8168", "[0.5 - 0.6]" = "#C56749",
      "[0.6 - 0.7]" = "#AD2C24", "[0.7 - 0.8]" = "#971428",
      "[0.8 - 0.9]" = "#5A0E1F", "[0.9 - 1]" = "#3C0912"
    )) +
    annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))+
    geom_sf(data = estado, aes(), color = "#151D44", fill = NA)
}

# Aplicar a função para criar os mapas
pd_chirps <- create_pd_map(resultados, "PD_prec_chirps", "CHIRPS")
pd_era5 <- create_pd_map(resultados, "PD_prec_era5", "ERA5")
pd_persiann <- create_pd_map(resultados, "PD_persiann", "PERSIANN")
# ------------------------------------------------------------------------------
# Para o artigo ----------------------------------------------------------------

custom_theme <- theme(
  legend.title = element_text(size = 8, hjust = 0.5),   # Tamanho do título da legenda
  legend.text = element_text(size = 8),     # Tamanho do texto da legenda
  legend.key.size = unit(1, "lines"), 
  plot.title = element_text(size = 8, hjust = 0.5)      # Tamanho do título do gráfico
)

# Aplicar o tema personalizado aos mapas:
CC_chirps <- CC_chirps  + custom_theme
CC_era5 <- CC_era5  + custom_theme
CC_persiann<- CC_persiann + custom_theme
rmse_chirps <- rmse_chirps + custom_theme
rmse_era5 <- rmse_era5 + custom_theme
rmse_persiann <- rmse_persiann + custom_theme
me_chirps <- me_chirps +custom_theme
me_era5 <- me_era5 +custom_theme
me_persiann <- me_persiann +custom_theme
mae_chirps <- mae_chirps +custom_theme
mae_era5 <- mae_era5 +custom_theme
mae_persiann <- mae_persiann +custom_theme
pd_chirps <- pd_chirps +custom_theme
pd_era5 <- pd_era5 +custom_theme
pd_persiann <- pd_persiann +custom_theme
#  juntando os mapas por colunas:
cc <- ggpubr::ggarrange(CC_chirps, CC_era5, CC_persiann,ncol = 3, nrow = 1, common.legend = T ,legend = c("right"))
rmse <- ggpubr::ggarrange(rmse_chirps, rmse_era5, rmse_persiann,ncol = 3, nrow = 1, common.legend = T ,legend = c("right"))
me <- ggpubr::ggarrange(me_chirps, me_era5, me_persiann,ncol = 3, nrow = 1, common.legend = T, legend = c("right"))
mae <- ggpubr::ggarrange(mae_chirps, mae_era5, mae_persiann,ncol = 3, nrow = 1, common.legend = T, legend = c("right"))
pd <- ggpubr::ggarrange(pd_chirps, pd_era5, pd_persiann,ncol = 3, nrow = 1, common.legend = T, legend = c("right"))

# Juntando os mapas:
total <- ggpubr::ggarrange(cc, me, mae,rmse, pd, ncol = 1, nrow = 5 )
# salvando os mapas
ggsave("results/figura_6.png", plot = total,
       width = 11, height = 16, dpi = 300)


# para apresentacao ------------------------------------------------------------
custom_theme1 <- theme(
  legend.title = element_text(size = 8, hjust = 0.5),   # Tamanho do título da legenda
  legend.text = element_text(size = 6),     # Tamanho do texto da legenda
  legend.key.size = unit(0.5, "lines"))
# Aplicar o tema personalizado aos mapas:
CC_chirps1 <- CC_chirps  + custom_theme1
CC_era51 <- CC_era5  + custom_theme1
CC_persiann1 <- CC_persiann + custom_theme1
rmse_chirps1 <- rmse_chirps + custom_theme1
rmse_era51 <- rmse_era5 + custom_theme1
rmse_persiann1 <- rmse_persiann + custom_theme1
me_chirps1 <- me_chirps +custom_theme1
me_era51 <- me_era5 +custom_theme1
me_persiann1 <- me_persiann +custom_theme1
mae_chirps1 <- mae_chirps +custom_theme1
mae_era51 <- mae_era5 +custom_theme1
mae_persiann1 <- mae_persiann +custom_theme1
pd_chirps1 <- pd_chirps +custom_theme1
pd_era51 <- pd_era5 +custom_theme1
pd_persiann1 <- pd_persiann +custom_theme1

cc1 <- ggpubr::ggarrange(CC_chirps1, CC_era51, CC_persiann1,ncol = 1, nrow = 3, common.legend = T ,legend = c("top"))
rmse1 <- ggpubr::ggarrange(rmse_chirps1, rmse_era51, rmse_persiann1,ncol = 1, nrow = 3, common.legend = T ,legend = c("top"))
me1 <- ggpubr::ggarrange(me_chirps1, me_era51, me_persiann1,ncol = 1, nrow = 3, common.legend = T, legend = c("top"))
mae1 <- ggpubr::ggarrange(mae_chirps1, mae_era51, mae_persiann1,ncol = 1, nrow = 3, common.legend = T, legend = c("top"))
pd1 <- ggpubr::ggarrange(pd_chirps1, pd_era51, pd_persiann1,ncol = 1, nrow = 3, common.legend = T, legend = c("top"))

# Juntando os mapas:
total1 <- ggpubr::ggarrange(cc1, me1, mae1,rmse1, pd1, ncol = 5, nrow = 1 )
# salvando os mapas
ggsave("results/total_apresen.png", plot = total1,
       width = 16, height = 9, dpi = 300)