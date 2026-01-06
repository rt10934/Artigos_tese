# Exibir o gráfico

library(sf)
library(spData)
library(ggplot2)
library(cowplot)
library(rcartocolor)
library(dplyr)
library(ggspatial)
# Load necessary libraries
# Read the shapefile and rename the column
hex_grid_sf <- readRDS("data_1/shapefile_18_1.rds") %>%
  rename(ID = CLASE)

# Selecionar um hexágono para aplicar zoom
hex_zoom <- hex_grid_sf[6000, ]

us_states <- hex_grid_sf
north_carolina <- hex_grid_sf[6000, ]

# Transformar coordenadas para EPSG:4326
us_states_2163 = st_transform(us_states, crs = 4326)
north_carolina_2163 = st_transform(north_carolina, crs = 4326)

north_carolina_2163_bb = st_as_sfc(st_bbox(north_carolina_2163))
mainbox <- st_bbox(north_carolina_2163)

# Verificar a presença de 'BIR74' e adicionar uma variável de preenchimento fictícia se necessário
if (!"BIR74" %in% colnames(north_carolina_2163)) {
  north_carolina_2163$BIR74 <- runif(nrow(north_carolina_2163), min = 0, max = 100) # Exemplo de dados fictícios
}

# Gerar o gráfico com ajuste de coord_sf e a borda expandida

# Obter a bounding box dos dados
bbox <- st_bbox(north_carolina_2163)

# Aumentar as medidas da bounding box
expand_factor <- 0.1  # 10% maior
bbox["xmin"] <- bbox["xmin"] - (bbox["xmax"] - bbox["xmin"]) * expand_factor
bbox["xmax"] <- bbox["xmax"] + (bbox["xmax"] - bbox["xmin"]) * expand_factor
bbox["ymin"] <- bbox["ymin"] - (bbox["ymax"] - bbox["ymin"]) * expand_factor
bbox["ymax"] <- bbox["ymax"] + (bbox["ymax"] - bbox["ymin"]) * expand_factor

# Converter a bounding box expandida para sfc
north_carolina_2163_bb_expanded <- st_as_sfc(bbox)

# Painel de inserção
inset = ggplot() + 
  geom_sf(data = us_states_2163, fill = "white", size = 0.2 ) + 
  geom_sf(data = north_carolina_2163_bb_expanded, fill = "red", size = 1.2 )+
  annotation_scale(location = "bl", style = "bar", bar_cols = c("black", "white"), pad_x = unit(8, "cm"), pad_y = unit(1, "cm")) +
  theme_void()
# inset
# Gerar o gráfico com a nova bounding box expandida
main <- ggplot() + 
  geom_sf(data = north_carolina_2163, fill = "red") +
  geom_sf(data = north_carolina_2163_bb_expanded, fill = NA, color = "red", size = 1.2) +  # Usar a nova bounding box expandida
  theme_void() +
  annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"), pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  theme(legend.position = "none")
# Exibir o gráfico
print(main)

# Mapa com inserção
gg_inset_map1 = cowplot::ggdraw() +
  coord_equal(xlim = c(0, 20), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(inset), xmin = 0, xmax = 14, ymin = 0, ymax = 20) +
  annotation_custom(ggplotGrob(main), xmin = 14, xmax = 20, ymin = 5, ymax = 15) +
  geom_segment(aes(x = 7.7, xend = 14.25, y = 7.8, yend = 6.6), color = "red") +
  geom_segment(aes(x = 7.7, xend = 14.25, y = 7.8, yend = 13.4), color = "red")+
  geom_segment(aes(x = 17, xend = 19.2, y = 10, yend = 8.5), color = "blue", size = 1.2)+
  geom_text(aes(x = 18.2, y = 10, label = "R ≈ 19,5 Km"), color = "black", size = 3)

gg_inset_map1
# Exibir o mapa
# print(gg_inset_map1)
ggsave("results/figura_3.png", plot = gg_inset_map1,
       width = 10, height = 8, dpi = 300)
