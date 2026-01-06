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


#-------------------------------------------------------------------------------
# Obtendo os PD ----------------------------------------------------------------
#-------------------------------------------------------------------------------
panel_data <- BASE_COM
# Manipulando os dados:
AA<-1
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

## fazendo a 1%:
PD_BR_1 <- panel_data %>%
  dplyr::group_by(ID) %>% # parei aqui
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  ) %>%
  summarise(
    PD_chirps = mean(PD_chirps, na.rm = T),
    PD_era5 = mean(PD_era5, na.rm = T),
    PD_persiann= mean(PD_persiann, na.rm = T)) %>% 
  dplyr::mutate(name_region="Brasil", Corte="1%") %>%
  dplyr::select(Corte, name_region, PD_chirps, PD_era5, PD_persiann)


PD_regiao_1 <- panel_data %>%
  dplyr::group_by( name_region, ID) %>% # parei aqui
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  ) %>%
  dplyr::group_by( name_region) %>% # parei aqui
  summarise(
    PD_chirps = mean(PD_chirps, na.rm = T),
    PD_era5 = mean(PD_era5, na.rm = T),
    PD_persiann= mean(PD_persiann, na.rm = T)) %>% 
  dplyr::mutate(Corte="1%") %>%
  dplyr::select(Corte, name_region, PD_chirps, PD_era5, PD_persiann)

## fazendo a 5%:
# Manipulando os dados:
AA<-5
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

PD_BR_5 <- panel_data %>%
  dplyr::group_by(ID) %>% # parei aqui
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  ) %>%
  summarise(
    PD_chirps = mean(PD_chirps, na.rm = T),
    PD_era5 = mean(PD_era5, na.rm = T),
    PD_persiann= mean(PD_persiann, na.rm = T)) %>% 
  dplyr::mutate(name_region="Brasil", Corte="5%") %>%
  dplyr::select(Corte, name_region, PD_chirps, PD_era5, PD_persiann)

PD_regiao_5 <- panel_data %>%
  dplyr::group_by( name_region, ID) %>% # parei aqui
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  ) %>%
  dplyr::group_by( name_region) %>% # parei aqui
  summarise(
    PD_chirps = mean(PD_chirps, na.rm = T),
    PD_era5 = mean(PD_era5, na.rm = T),
    PD_persiann= mean(PD_persiann, na.rm = T)) %>% 
  dplyr::mutate(Corte="5%") %>%
  dplyr::select(Corte, name_region, PD_chirps, PD_era5, PD_persiann)

## fazendo a 10%:
# Manipulando os dados:
AA<-10
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

PD_BR_10 <- panel_data %>%
  dplyr::group_by(ID) %>% # parei aqui
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  ) %>%
  summarise(
    PD_chirps = mean(PD_chirps, na.rm = T),
    PD_era5 = mean(PD_era5, na.rm = T),
    PD_persiann= mean(PD_persiann, na.rm = T))%>% 
  dplyr::mutate(name_region="Brasil", Corte="10%") %>%
  dplyr::select(Corte, name_region, PD_chirps, PD_era5, PD_persiann)

PD_regiao_10 <- panel_data %>%
  dplyr::group_by( name_region, ID) %>% # parei aqui
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  ) %>%
  dplyr::group_by( name_region) %>% # parei aqui
  summarise(
    PD_chirps = mean(PD_chirps, na.rm = T),
    PD_era5 = mean(PD_era5, na.rm = T),
    PD_persiann= mean(PD_persiann, na.rm = T)) %>% 
  dplyr::mutate(Corte="10%") %>%
  dplyr::select(Corte, name_region, PD_chirps, PD_era5, PD_persiann)

## fazendo a 20%:
# Manipulando os dados:
AA<-20
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

PD_BR_20 <- panel_data %>%
  dplyr::group_by(ID) %>% # parei aqui
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  ) %>%
  summarise(
    PD_chirps = mean(PD_chirps, na.rm = T),
    PD_era5 = mean(PD_era5, na.rm = T),
    PD_persiann= mean(PD_persiann, na.rm = T))%>% 
  dplyr::mutate(name_region="Brasil", Corte="20%") %>%
  dplyr::select(Corte, name_region, PD_chirps, PD_era5, PD_persiann)

PD_regiao_20 <- panel_data %>%
  dplyr::group_by( name_region, ID) %>% # parei aqui
  summarise(
    PD_chirps = PD(prec_chirps_I),
    PD_era5 = PD(prec_era5_I),
    PD_persiann= PD(prec_persiann_I),
    .groups = "drop"
  ) %>%
  dplyr::group_by( name_region) %>% # parei aqui
  summarise(
    PD_chirps = mean(PD_chirps, na.rm = T),
    PD_era5 = mean(PD_era5, na.rm = T),
    PD_persiann= mean(PD_persiann, na.rm = T)) %>% 
  dplyr::mutate(Corte="20%") %>%
  dplyr::select(Corte, name_region, PD_chirps, PD_era5, PD_persiann)

base_PD_todos <- dplyr::bind_rows(PD_BR_1, PD_regiao_1, PD_BR_5,
                                  PD_regiao_5, PD_BR_10, PD_regiao_10,  
                                  PD_BR_20, PD_regiao_20)


base_PD_todos <- base_PD_todos |> dplyr::filter(!is.na(name_region))
openxlsx::write.xlsx(base_PD_todos,file = "results/tabela_4A.xlsx")

base_PD_todos |> dplyr::glimpse()
# Manipulacao para o PD ------------------------------------------------------
library(ggvanced)
library(tidyverse)

TABE1 <- base_PD_todos
TABE1 <- TABE1 |>
  tidyr::pivot_longer(
    cols = c(PD_chirps, PD_era5, PD_persiann),
    names_to = "Produto",
    values_to = "Values"
  )|>
  mutate(name_region = ifelse(name_region=="Centro Oeste", "CO",
                              ifelse(name_region=="Brasil","BR",
                                     ifelse(name_region=="Norte","N",
                                            ifelse(name_region=="Nordeste","NE",
                                                   ifelse(name_region=="Sudeste", "SE",
                                                          ifelse(name_region=="Sul","S",name_region)))))))

# Fazendo alteracoes
tabe6 <- TABE1 |>
  tidyr::pivot_wider(names_from = "name_region",
                     values_from = "Values")|>
  mutate(Produto = ifelse(Produto=="PD_chirps","CHIRPS",
                          ifelse(Produto=="PD_era5", "ERA5", 
                                 ifelse(Produto=="PD_persiann", "PERSIANN",Produto))))|>
  dplyr::rename(`SSP:` = Produto)

# Adicionar linhas de zeros
tabe6 <- tabe6 %>%
  bind_rows(
    tibble(
      `SSP:` = "Zeros",
      BR = 0.6,
      CO = 0.6,
      NE = 0.6,
      N = 0.6,
      SE = 0.6,
      S = 0.6
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

pd_1 <-tabe6 %>%
  dplyr::filter( Corte == "1%" | is.na(Corte)) %>% select(-Corte) %>%
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
  labs(x = "", y = "", fill = "", title = "1 mm/day")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()+
  annotate("text", 
           x = -1.1, y = 0,          
           label = "", 
           angle = 90,                
           size = 5,                 
           fontface = "bold") 




pd_5 <-tabe6 %>%
  dplyr::filter( Corte == "5%" | is.na(Corte)) %>% select(-Corte) %>%
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
  labs(x = "", y = "", fill = "", title = "5 mm/day")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()+
  annotate("text", 
           x = -1.1, y = 0,          
           label = "", 
           angle = 90,                
           size = 5,                 
           fontface = "bold") 

pd_10 <-tabe6 %>%
  dplyr::filter( Corte == "10%" | is.na(Corte)) %>% select(-Corte) %>%
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
  labs(x = "", y = "", fill = "", title = "10 mm/day")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()+
  annotate("text", 
           x = -1.1, y = 0,          
           label = "", 
           angle = 90,                
           size = 5,                 
           fontface = "bold") 

pd_20 <-tabe6 %>%
  dplyr::filter( Corte == "20%" | is.na(Corte)) %>% select(-Corte) %>%
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
  labs(x = "", y = "", fill = "", title = "20 mm/day")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))+
  coord_equal()+
  annotate("text", 
           x = -1.1, y = 0,          
           label = "", 
           angle = 90,                
           size = 5,                 
           fontface = "bold") 

pd_estacao <- ggpubr::ggarrange(pd_1, pd_5, pd_10, pd_20,
                                ncol = 4, nrow = 1, common.legend = T ,legend = c("bottom"))

ggsave("results/figuras_9_en.png", plot = pd_estacao,
       width = 11, height = 4, dpi = 300)
