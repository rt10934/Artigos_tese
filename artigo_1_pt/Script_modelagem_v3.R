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
library(ggridges)
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
# Construcao das funcoes e estimacoes para os gerais ---------------------------
#-------------------------------------------------------------------------------
# CC ---------------------------------------------------------------------------
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

#  RMSE ------------------------------------------------------------------------
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
# geral 
RMSE_geral <- BASE_COM %>%
  dplyr::group_by(name_biome, name_region, ID) %>%
  dplyr::summarise(
    RMSE_chirps = RMSE(prec_chirps, prec_br_dwgd), 
    RMSE_era5 = RMSE(prec_era5, prec_br_dwgd), 
    RMSE_persiann = RMSE(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

# ME----------------------------------------------------------------------------
ME <- function(observed, predicted) {
  # Calculando os resíduos ao quadrado
  residuals_squared <- (observed - predicted)
  
  # Ignorando NA nos cálculos
  valid_data <- complete.cases(residuals_squared)
  
  # Calculando a média dos resíduos
  me <- mean(residuals_squared[valid_data], na.rm = TRUE)
  
  return(me)
}
# geral (regiao e bioma)
ME_geral <- BASE_COM %>%
  dplyr::group_by(name_biome, name_region, ID) %>%
  dplyr::summarise(
    ME_chirps = ME(prec_chirps, prec_br_dwgd), 
    ME_era5 = ME(prec_era5, prec_br_dwgd), 
    ME_persiann = ME(prec_persiann, prec_br_dwgd),#primeiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupamento após o summarize
  )

# MAE --------------------------------------------------------------------------
MAE <- function(observed, predicted) {
  # Calculando os resíduos ao quadrado
  residuals_squared <- abs(observed - predicted)
  
  # Ignorando NA nos cálculos
  valid_data <- complete.cases(residuals_squared)
  
  # Calculando a média dos resíduos
  mae <- mean(residuals_squared[valid_data], na.rm = TRUE)
  
  return(mae)
}
# geral
MAE_geral <- BASE_COM %>%
  dplyr::group_by(name_biome, name_region, ID) %>%
  dplyr::summarise(
    MAE_chirps = MAE(prec_chirps, prec_br_dwgd), 
    MAE_era5 = MAE(prec_era5, prec_br_dwgd), 
    MAE_persiann = MAE(prec_persiann, prec_br_dwgd),#priMAEiro base, segundo ufes
    .groups = "drop"  # Descarta o agrupaMAEnto após o summarize
  )

# PD ---------------------------------------------------------------------------
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

################################################################################
# TABELAS para todos os grupos:
################################################################################
# ------------------------------------------------------------------------------
# Tabela para o brasil como um todo: -------------------------------------------
#-------------------------------------------------------------------------------
## completa CC
indicador_CC <- BASE |>
  dplyr::summarise(CHIRPS = mean(prec_chirps,na.rm=T),
                   ERA5 = mean(prec_era5,na.rm=T),
                   PERSIANN = mean(prec_persiann,na.rm=T))|>
  dplyr::mutate(Indicador = "CC") |>
  dplyr::select(Indicador, CHIRPS, ERA5, PERSIANN)

# completo RMSE
indicador_RMSE <- RMSE_geral %>%
  dplyr::summarise(
    CHIRPS = mean(RMSE_chirps, na.rm=T), 
    ERA5 = mean(RMSE_era5, na.rm=T), 
    PERSIANN = mean(RMSE_persiann, na.rm=T)) %>%
  dplyr::mutate(Indicador = "RMSE") %>%
  dplyr::select(Indicador, CHIRPS, ERA5, PERSIANN)

# completo ME
indicador_ME <- ME_geral %>%
  dplyr::summarise(
    CHIRPS = mean(ME_chirps, na.rm=T), 
    ERA5 = mean(ME_era5, na.rm=T), 
    PERSIANN = mean(ME_persiann, na.rm=T)) %>%
  dplyr::mutate(Indicador = "ME") %>%
  dplyr::select(Indicador, CHIRPS, ERA5, PERSIANN)

# completo MAE
indicador_MAE <- MAE_geral %>%
  dplyr::summarise(
    CHIRPS = mean(MAE_chirps, na.rm=T), 
    ERA5 = mean(MAE_era5, na.rm=T), 
    PERSIANN = mean(MAE_persiann, na.rm=T)) %>%
  dplyr::mutate(Indicador = "MAE") %>%
  dplyr::select(Indicador, CHIRPS, ERA5, PERSIANN)

# completo PD 
indicador_PD <- PD_geral %>%
  dplyr::summarise(
    CHIRPS = mean(PD_chirps, na.rm=T), 
    ERA5 = mean(PD_era5, na.rm=T), 
    PERSIANN = mean(PD_persiann, na.rm=T)) %>%
  dplyr::mutate(Indicador = "PD") %>%
  dplyr::select(Indicador, CHIRPS, ERA5, PERSIANN)

# juntando tabelas
Tabela_Brasil <- dplyr::bind_rows(indicador_CC, indicador_ME, indicador_MAE, 
                                  indicador_RMSE, indicador_PD)
openxlsx::write.xlsx(Tabela_Brasil,file = "results/Tabela_Brasil.xlsx")
rm(indicador_CC, indicador_ME, indicador_MAE, indicador_RMSE, indicador_PD)
#-------------------------------------------------------------------------------
# Tabela por Regiao e bioma 

unique(PD_geral$name_biome)

# Bottom Right
ggplot(mpg, aes(x=class, y=hwy, fill=class)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

# Mapas Brasil -----------------------------------------------------------------
CC_long <- BASE %>%
  dplyr::select(ID, name_biome, name_region, prec_chirps, prec_era5, prec_persiann) %>%
  dplyr::rename(CHIRPS = prec_chirps, ERA5 = prec_era5, PERSIANN = prec_persiann)%>%
  tidyr::pivot_longer(cols = c(CHIRPS, ERA5, PERSIANN),
                      names_to = "Metodo", values_to = "CC")

ME_long <- ME_geral %>%
  dplyr::select(ID, name_biome, name_region, ME_chirps, ME_era5, ME_persiann) %>%
  dplyr::rename(CHIRPS = ME_chirps, ERA5 = ME_era5, PERSIANN = ME_persiann)%>%
  tidyr::pivot_longer(cols = c(CHIRPS, ERA5, PERSIANN),
                      names_to = "Metodo", values_to = "ME")%>%
  dplyr::select(-name_biome, -name_region)

RMSE_long <- RMSE_geral %>%
  dplyr::select(ID, name_biome, name_region, RMSE_chirps, RMSE_era5, RMSE_persiann) %>%
  dplyr::rename(CHIRPS = RMSE_chirps, ERA5 = RMSE_era5, PERSIANN = RMSE_persiann)%>%
  tidyr::pivot_longer(cols = c(CHIRPS, ERA5, PERSIANN),
                      names_to = "Metodo", values_to = "RMSE")%>%
  dplyr::select(-name_biome, -name_region)

MAE_long <- MAE_geral %>%
  dplyr::select(ID, name_biome, name_region, MAE_chirps, MAE_era5, MAE_persiann) %>%
  dplyr::rename(CHIRPS = MAE_chirps, ERA5 = MAE_era5, PERSIANN = MAE_persiann)%>%
  tidyr::pivot_longer(cols = c(CHIRPS, ERA5, PERSIANN),
                      names_to = "Metodo", values_to = "MAE")%>%
  dplyr::select(-name_biome, -name_region)

PD_long <- PD_geral %>%
  dplyr::select(ID, name_biome, name_region, PD_chirps, PD_era5, PD_persiann) %>%
  dplyr::rename(CHIRPS = PD_chirps, ERA5 = PD_era5, PERSIANN = PD_persiann) %>%
  tidyr::pivot_longer(cols = c(CHIRPS, ERA5, PERSIANN),
                      names_to = "Metodo", values_to = "PD") %>%
  dplyr::select(-name_biome, -name_region)

# JUNTANDO OS DADOS:
Base_Mapas <- dplyr::full_join(CC_long, ME_long, by=c("ID", "Metodo"))
Base_Mapas <- dplyr::full_join(Base_Mapas, RMSE_long, by=c("ID", "Metodo"))
Base_Mapas <- dplyr::full_join(Base_Mapas, MAE_long, by=c("ID", "Metodo"))
Base_Mapas <- dplyr::full_join(Base_Mapas, PD_long, by=c("ID", "Metodo"))

# fazendo mapas:
# CC
CC_fig <- Base_Mapas %>%
  ggplot(aes(x = CC, y = Metodo, fill = Metodo)) +
  geom_density_ridges() +
  theme_minimal() +
  labs(x = "", y = "", fill = "", title = "CC") +  # Título para a legenda
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",     # Azul para CHIRPS
                               "ERA5" = "#56DE52",  # Verde para ERA5-Land
                               "PERSIANN" = "#6EC7D5")) +  # Vermelho para PERSIANN
  theme(legend.position = "bottom",   # Posiciona a legenda à direita
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", color = "black"))+    # Tamanho do texto das facetas
  xlim(0.2,0.85)

# ME
ME_fig <- Base_Mapas %>%
  ggplot(aes(x = ME, y = Metodo, fill = Metodo)) +
  geom_density_ridges() +
  geom_vline(xintercept = 0, color = "black", size = 0.8) +  # Linha vertical em x = 0
  theme_minimal() +
  labs(x = "", y = "", fill = "", title = "ME") +  # Título para a legenda
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",     # Azul para CHIRPS
                               "ERA5" = "#56DE52",  # Verde para ERA5-Land
                               "PERSIANN" = "#6EC7D5")) +  # Vermelho para PERSIANN
  theme(legend.position = "bottom",   # Posiciona a legenda à direita
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", color = "black"))+    # Tamanho do texto das facetas
  xlim(-2,2)

# MAE
MAE_fig <- Base_Mapas %>%
  ggplot(aes(x = MAE, y = Metodo, fill = Metodo)) +
  geom_density_ridges() +
  theme_minimal() +
  labs(x = "", y = "", fill = "", title = "MAE") +  # Título para a legenda
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",     # Azul para CHIRPS
                               "ERA5" = "#56DE52",  # Verde para ERA5-Land
                               "PERSIANN" = "#6EC7D5")) +  # Vermelho para PERSIANN
  theme(legend.position = "bottom",   # Posiciona a legenda à direita
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", color = "black"))+    # Tamanho do texto das facetas
  xlim(0,11)

# RMSE
RMSE_fig <- Base_Mapas %>%
  ggplot(aes(x = RMSE, y = Metodo, fill = Metodo)) +
  geom_density_ridges() +
  theme_minimal() +
  labs(x = "", y = "", fill = "", title = "RMSE") +  # Título para a legenda
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",     # Azul para CHIRPS
                               "ERA5" = "#56DE52",  # Verde para ERA5-Land
                               "PERSIANN" = "#6EC7D5")) +  # Vermelho para PERSIANN
  theme(legend.position = "bottom",   # Posiciona a legenda à direita
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", color = "black"))+    # Tamanho do texto das facetas
  xlim(0,20)

# PD
PD_fig <- Base_Mapas %>%
  ggplot(aes(x = PD, y = Metodo, fill = Metodo)) +
  geom_density_ridges() +
  theme_minimal() +
  labs(x = "", y = "", fill = "", title = "PD") +  # Título para a legenda
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",     # Azul para CHIRPS
                               "ERA5" = "#56DE52",  # Verde para ERA5-Land
                               "PERSIANN" = "#6EC7D5")) +  # Vermelho para PERSIANN
  theme(legend.position = "bottom",   # Posiciona a legenda à direita
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", color = "black"))+    # Tamanho do texto das facetas
  xlim(0.25,1)
PD_fig
# juntando os mapas:
figuras_brasil <- ggpubr::ggarrange(CC_fig, ME_fig, MAE_fig, RMSE_fig, PD_fig,
                                    ncol = 5, nrow = 1, common.legend = T ,legend = c("bottom"))

ggsave("results/figura_5.png", plot = figuras_brasil,
       width = 12, height = 3.4, dpi = 300)
rm(CC_fig, ME_fig, MAE_fig, RMSE_fig, PD_fig)
# ------------------------------------------------------------------------------
# Tabela para os biomas e regioes como um todo: --------------------------------
#-------------------------------------------------------------------------------
# montando base:
Base_Mapa_bioma <- Base_Mapas |>
  dplyr::filter(!name_biome%in%c("Sistema Costeiro", NA)) |>
  dplyr::select(ID, name_biome, Metodo, CC, ME, RMSE, MAE, PD) |>
  dplyr::rename(CHAVE = name_biome)

Base_Mapa_regiao <- Base_Mapas |>
  dplyr::filter(!is.na(name_region)) |>
  dplyr::select(ID, name_region, Metodo, CC, ME, RMSE, MAE, PD) |>
  dplyr::rename(CHAVE = name_region)
# juntando as bases:
Base_Mapa_1 <- dplyr::bind_rows(Base_Mapa_bioma, Base_Mapa_regiao)
Base_Mapa_1 <- Base_Mapa_1 |>
  dplyr::mutate(CHAVE = ifelse(CHAVE=="Centro Oeste", "Centro-Oeste",CHAVE))
# mantendo na ordem:
Base_Mapa_1$CHAVE <- factor(Base_Mapa_1$CHAVE, 
                            levels = c("Amazônia", "Caatinga", "Cerrado", "Mata Atlântica",
                                       "Pampa", "Pantanal", "Centro-Oeste", "Nordeste",
                                       "Norte", "Sudeste", "Sul"))

# Bottom Right
CC_fig <- ggplot(Base_Mapa_1, aes(x=CHAVE , y=CC, fill=Metodo)) + 
  geom_boxplot(width = 0.8, size = 0.2, alpha = 2, outlier.size=0.7)+
  theme_minimal() +
  labs(x = "", y = "CC", fill = "")+
  scale_fill_brewer(palette="Dark2") +
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",    
                               "ERA5" = "#56DE52",  
                               "PERSIANN" = "#6EC7D5")) +  
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 5),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_blank())

ME_fig <- ggplot(Base_Mapa_1, aes(x=CHAVE , y=ME, fill=Metodo)) + 
  geom_boxplot(width = 0.8, size = 0.2, alpha = 2, outlier.size=0.7)+
  theme_minimal() +
  labs(x = "", y = "ME", fill = "")+
  scale_fill_brewer(palette="Dark2") +
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",    
                               "ERA5" = "#56DE52",  
                               "PERSIANN" = "#6EC7D5"))  +  
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 5),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_blank())

MAE_fig <- ggplot(Base_Mapa_1, aes(x=CHAVE , y=MAE, fill=Metodo)) + 
  geom_boxplot(width = 0.8, size = 0.2, alpha = 2, outlier.size=0.7)+
  theme_minimal() +
  labs(x = "", y = "MAE", fill = "")+
  scale_fill_brewer(palette="Dark2") +
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",    
                               "ERA5" = "#56DE52",  
                               "PERSIANN" = "#6EC7D5"))  +  
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 5),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_blank())

RMSE_fig <- ggplot(Base_Mapa_1, aes(x=CHAVE , y=RMSE, fill=Metodo)) + 
  geom_boxplot(width = 0.8, size = 0.2, alpha = 2, outlier.size=0.7)+
  theme_minimal() +
  labs(x = "", y = "RMSE", fill = "")+
  scale_fill_brewer(palette="Dark2") +
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",    
                               "ERA5" = "#56DE52",  
                               "PERSIANN" = "#6EC7D5"))  +  
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 5),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_blank())

PD_fig <- ggplot(Base_Mapa_1, aes(x=CHAVE , y=PD, fill=Metodo)) + 
  geom_boxplot(width = 0.8, size = 0.2, alpha = 2, outlier.size=0.7)+
  theme_minimal() +
  labs(x = "", y = "PD", fill = "")+
  scale_fill_brewer(palette="Dark2") +
  scale_fill_manual(values = c("CHIRPS" = "#D06F66",    
                               "ERA5" = "#56DE52",  
                               "PERSIANN" = "#6EC7D5"))  +  
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 5),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"))

# Ajuste das margens em cada gráfico antes de combinar
CC_fig <- CC_fig + theme(plot.margin = unit(c(0, 0.2, 0, 0.2), "cm"))  # Ajustar as margens
ME_fig <- ME_fig + theme(plot.margin = unit(c(0, 0.2, 0, 0.2), "cm"))
MAE_fig <- MAE_fig + theme(plot.margin = unit(c(0, 0.2, 0, 0.2), "cm"))
RMSE_fig <- RMSE_fig + theme(plot.margin = unit(c(0, 0.2, 0, 0.2), "cm"))
PD_fig <- PD_fig + theme(plot.margin = unit(c(0, 0.2, 0, 0.2), "cm"))

# Combinar gráficos com espaço reduzido entre as figuras
figuras_regios <- ggpubr::ggarrange(CC_fig, ME_fig, MAE_fig, RMSE_fig, PD_fig,
                                    ncol = 1, nrow = 5, common.legend = T, legend = c("bottom"),
                                    align = "v", heights = c(1, 1, 1, 1, 1))  # Ajustar proporção de alturas

ggsave("results/figura_7.png", plot = figuras_regios,
       width = 11, height = 12, dpi = 300)

