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
# ESTATISTICAS DESCRITIVAS 
#-------------------------------------------------------------------------------
BASE_DATA <- BASE_COM 

BASE_DATA <- BASE_DATA |>
  dplyr::group_by(ID, lubridate::year(data))|>
  summarise(prec_chirps = sum(prec_chirps, na.rm = T),
            prec_br_dwgd = sum(prec_br_dwgd, na.rm = T),
            prec_era5 = sum(prec_era5, na.rm = T),
            prec_persiann = sum(prec_persiann, na.rm = T)) |>
  dplyr::ungroup()

BASE_DATA <- BASE_DATA |>
  dplyr::group_by(ID)|>
  summarise(prec_chirps = mean(prec_chirps, na.rm = T),
            prec_br_dwgd = mean(prec_br_dwgd, na.rm = T),
            prec_era5 = mean(prec_era5, na.rm = T),
            prec_persiann = mean(prec_persiann, na.rm = T))

BASE_DATA |> glimpse()
BASE_DATA <- dplyr::full_join(hex_grid_sf, BASE_DATA, by="ID")
#------------------------------------------------------------------------------
# CHIRPS_19_20 
CHIRPS<-BASE_DATA |>
  dplyr::mutate(Categoria=
                  cut(prec_chirps,
                      breaks =c(1, 500, 1000, 1250, 1500, 1750,
                                2000, 2250, 2500, 3000, 4000),
                      labels = c("[0 - 500]","[500 -1000]",
                                 "[1000 - 1250]", "[1250 - 1500]",
                                 "[1500 - 1750]", "[1750 - 2000]",
                                 "[2000 - 2250]", "[2250 - 2500]",
                                 "[2500 - 3000]", "[3000 - 4000]")))|>
  ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes( fill=Categoria),
                                       color="NA",show.legend = TRUE)+
  ggplot2::ggtitle("CHIRPS")+
  ggplot2::labs(fill="precipitation")+
  ggplot2::scale_fill_manual(values = c("#DBDECF","#B6CBAF","#8DB997",
                                        "#5DA786","#2A937F","#117D79",
                                        "#18656F","#1C4D61","#1A3651",
                                        "#151D44"))+
  annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))+
  ggplot2::geom_sf(data = estado, mapping = ggplot2::aes(), color = "#151D44",
                   fill = NA)
# ERA5_19_20 
ERA5<-BASE_DATA |>
  dplyr::mutate(Categoria=
                  cut(prec_era5,
                      breaks =c(1, 500, 1000, 1250, 1500, 1750,
                                2000, 2250, 2500, 3000, 4000),
                      labels = c("[0 - 500]","[500 -1000]",
                                 "[1000 - 1250]", "[1250 - 1500]",
                                 "[1500 - 1750]", "[1750 - 2000]",
                                 "[2000 - 2250]", "[2250 - 2500]",
                                 "[2500 - 3000]", "[3000 - 4000]")))|>
  ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes( fill=Categoria),
                                       color="NA",show.legend = TRUE)+
  ggplot2::ggtitle("ERA5")+
  ggplot2::labs(fill="precipitation")+
  ggplot2::scale_fill_manual(values = c("#DBDECF","#B6CBAF","#8DB997",
                                        "#5DA786","#2A937F","#117D79",
                                        "#18656F","#1C4D61","#1A3651",
                                        "#151D44"))+
  annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))+
  ggplot2::geom_sf(data = estado, mapping = ggplot2::aes(), color = "#151D44",
                   fill = NA)
# PERSIANN_19_20 
PERSIANN<-BASE_DATA |>
  dplyr::mutate(Categoria=
                  cut(prec_persiann,
                      breaks =c(1, 500, 1000, 1250, 1500, 1750,
                                2000, 2250, 2500, 3000, 4000),
                      labels = c("[0 - 500]","[500 -1000]",
                                 "[1000 - 1250]", "[1250 - 1500]",
                                 "[1500 - 1750]", "[1750 - 2000]",
                                 "[2000 - 2250]", "[2250 - 2500]",
                                 "[2500 - 3000]", "[3000 - 4000]")))|>
  ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes( fill=Categoria),
                                       color="NA",show.legend = TRUE)+
  ggplot2::ggtitle("PERSIANN")+
  ggplot2::labs(fill="precipitation")+
  ggplot2::scale_fill_manual(values = c("#DBDECF","#B6CBAF","#8DB997",
                                        "#5DA786","#2A937F","#117D79",
                                        "#18656F","#1C4D61","#1A3651",
                                        "#151D44"))+
  annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))+
  ggplot2::geom_sf(data = estado, mapping = ggplot2::aes(), color = "#151D44",
                   fill = NA)
# ufes_19_20 
ufes<-BASE_DATA |>
  dplyr::mutate(Categoria=
                  cut(prec_br_dwgd,
                      breaks =c(1, 500, 1000, 1250, 1500, 1750,
                                2000, 2250, 2500, 3000, 4000),
                      labels = c("[0 - 500]","[500 -1000]",
                                 "[1000 - 1250]", "[1250 - 1500]",
                                 "[1500 - 1750]", "[1750 - 2000]",
                                 "[2000 - 2250]", "[2250 - 2500]",
                                 "[2500 - 3000]", "[3000 - 4000]")))|>
  ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes( fill=Categoria),
                                       color="NA",show.legend = TRUE)+
  ggplot2::ggtitle("BR-DWGD")+
  ggplot2::labs(fill="precipitation")+
  ggplot2::scale_fill_manual(values = c("#DBDECF","#B6CBAF","#8DB997",
                                        "#5DA786","#2A937F","#117D79",
                                        "#18656F","#1C4D61","#1A3651",
                                        "#151D44"))+
  annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))+
  ggplot2::geom_sf(data = estado, mapping = ggplot2::aes(), color = "#151D44",
                   fill = NA)
# juntando mapas:
custom_theme <- theme(
  legend.title = element_text(size = 8, hjust = 0.5),   # Tamanho do título da legenda
  legend.text = element_text(size = 8),     # Tamanho do texto da legenda
  legend.key.size = unit(1, "lines"), 
  plot.title = element_text(size = 8, hjust = 0.5)      # Tamanho do título do gráfico
)

ufes1 <- ufes + custom_theme
CHIRPS1 <- CHIRPS + custom_theme
PERSIANN1 <- PERSIANN + custom_theme
ERA51 <- ERA5 + custom_theme

grafico <- ggpubr::ggarrange(ufes1, CHIRPS1, PERSIANN1, ERA51, ncol = 2, nrow = 2,
                             common.legend = T, legend = c("right") )

ggsave("results/figura_4_en.png", plot = grafico,
       width = 7.05, height = 5.4, dpi = 300) # so mudar o nome
#-------------------------------------------------------------------------------
# tabela com os resultados por regiao ------------------------------------------
#-------------------------------------------------------------------------------
BASE_DATA <- BASE_COM 
dicionario_regioes <- readRDS(file = "data_1/dicionario_regioes.rds")

BASE_DATA <- BASE_DATA |>
  dplyr::group_by(ID, lubridate::year(data))|>
  summarise(prec_chirps = sum(prec_chirps, na.rm = T),
            prec_br_dwgd = sum(prec_br_dwgd, na.rm = T),
            prec_era5 = sum(prec_era5, na.rm = T),
            prec_persiann = sum(prec_persiann, na.rm = T)) |>
  dplyr::ungroup()|>
  dplyr::left_join(dicionario_regioes, by="ID")

BASE_DATA <- BASE_DATA |>
  dplyr::group_by(ID, name_region)|>
  summarise(prec_chirps = mean(prec_chirps, na.rm = T),
            prec_br_dwgd = mean(prec_br_dwgd, na.rm = T),
            prec_era5 = mean(prec_era5, na.rm = T),
            prec_persiann = mean(prec_persiann, na.rm = T))|>
  dplyr::group_by(name_region)|>
  summarise(prec_chirps = mean(prec_chirps, na.rm = T),
            prec_br_dwgd = mean(prec_br_dwgd, na.rm = T),
            prec_era5 = mean(prec_era5, na.rm = T),
            prec_persiann = mean(prec_persiann, na.rm = T))|>
  dplyr::filter(!is.na(name_region))

openxlsx::write.xlsx(BASE_DATA, file = "results/tabela_1.xlsx")
