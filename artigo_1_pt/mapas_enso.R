rm(list=ls())
library(raster)
library(sf)
library(ggplot2)
library(ggspatial)
# Passo 1: Carregar o Raster e o Shapefile dos Estados Brasileiros
bra_st <- geobr::read_state(year = 2020)
bounding_box <- extent(-73.99045, -28.83594, -33.75208, 5.271841)
elnino_acima<-raster("data_1/elnino_acima.tif")
elnino_abaixo<-raster("data_1/elnino_abaixo.tif")
lanina_acima<-raster("data_1/lanina_acima.tif")
lanina_abaixo<-raster("data_1/lanina_abaixo.tif")
# -----------------------------------------------------------------------------
# Manipulacao das bases de dados ----------------------------------------------
#------------------------------------------------------------------------------
# elnino_acima ----
# Cortar o raster com os limites definidos
elnino_acima_cortado <- crop(elnino_acima, bounding_box)
elnino_acima_cortado[elnino_acima_cortado==0] <- NA
# Converter o raster cortado para um dataframe para usar no ggplot
elnino_acima_df <- as.data.frame(rasterToPoints(elnino_acima_cortado), stringsAsFactors = FALSE)
colnames(elnino_acima_df) <- c("lon", "lat", "value")


# elnino_abaixo ----
# Cortar o raster com os limites definidos
elnino_abaixo_cortado <- crop(elnino_abaixo, bounding_box)
elnino_abaixo_cortado[elnino_abaixo_cortado==0] <- NA
# Converter o raster cortado para um dataframe para usar no ggplot
elnino_abaixo_df <- as.data.frame(rasterToPoints(elnino_abaixo_cortado), stringsAsFactors = FALSE)
colnames(elnino_abaixo_df) <- c("lon", "lat", "value")


# lanina_acima ----
# Cortar o raster com os limites definidos
lanina_acima_cortado <- crop(lanina_acima, bounding_box)
lanina_acima_cortado[lanina_acima_cortado==0] <- NA
# Converter o raster cortado para um dataframe para usar no ggplot
lanina_acima_df <- as.data.frame(rasterToPoints(lanina_acima_cortado), stringsAsFactors = FALSE)
colnames(lanina_acima_df) <- c("lon", "lat", "value")


# lanina_abaixo ----
# Cortar o raster com os limites definidos
lanina_abaixo_cortado <- crop(lanina_abaixo, bounding_box)
lanina_abaixo_cortado[lanina_abaixo_cortado==0] <- NA
# Converter o raster cortado para um dataframe para usar no ggplot
lanina_abaixo_df <- as.data.frame(rasterToPoints(lanina_abaixo_cortado), stringsAsFactors = FALSE)
colnames(lanina_abaixo_df) <- c("lon", "lat", "value")
#------------------------------------------------------------------------------
# factor 
# Seus valores e cores
values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
colors1 <- c("#E2C7BF","#DDB5A8","#D7A291","#D2907A","#C66B4E",
             "#C0583B","#B9452C","#A11D25","#901029","#7B0E29",
             "#510D1C","#3C0912")
colors2 <- c("#D0E7FF", "#A8D2FF", "#7FBFFF", "#559CFF", "#287AFF",
             "#0063FF", "#004ACB", "#003299", "#001C66", "#000D4D",
             "#000433", "#000122")


# Criar o fator
elnino_abaixo_df$color_factor <- factor(elnino_abaixo_df$value, levels = values, labels = colors1)
elnino_acima_df$color_factor <- factor(elnino_acima_df$value, levels = values, labels = colors2)
lanina_abaixo_df$color_factor <- factor(lanina_abaixo_df$value, levels = values, labels = colors1)
lanina_acima_df$color_factor <- factor(lanina_acima_df$value, levels = values, labels = colors2)
# Adicionando uma linha fictícia com o valor 12
# Primeiro, crie uma função para adicionar uma linha fictícia para um valor específico
add_fictitious_row <- function(df, value, levels, colors) {
  rbind(
    df,
    data.frame(
      lon = NA, 
      lat = NA, 
      value = factor(value, levels = as.character(levels)), 
      color_factor = factor(value, levels = as.character(levels), labels = colors)
    )
  )
}

# elnino_abaixo
for (val in values) {
  elnino_abaixo_df <- add_fictitious_row(elnino_abaixo_df, val, values, colors2)
}
#elnino_acima
for (val in values) {
  elnino_acima_df <- add_fictitious_row(elnino_acima_df, val, values, colors2)
}
# lanina_abaixo
for (val in values) {
  lanina_abaixo_df <- add_fictitious_row(lanina_abaixo_df, val, values, colors2)
}
#lanina_acima
for (val in values) {
  lanina_acima_df <- add_fictitious_row(lanina_acima_df, val, values, colors2)
}

# declarando
elnino_abaixo_df$value <- factor(elnino_abaixo_df$value, levels = values)
elnino_acima_df$value <- factor(elnino_acima_df$value, levels = values)
lanina_abaixo_df$value <- factor(lanina_abaixo_df$value, levels = values)
lanina_acima_df$value <- factor(lanina_acima_df$value, levels = values)

legenda_nome <- 
  theme(
    plot.title = element_text(size = 14,hjust = 0.5),
    legend.title = element_text(size = 13),   # Tamanho do título da legenda
    legend.text = element_text(size = 13),     # Tamanho do texto dos itens da legenda
    legend.key.size = unit(0.9, "cm"),        # Tamanho das caixas de cor na legenda
    legend.spacing.x = unit(0.3, "cm"),       # Espaçamento horizontal entre os itens da legenda
    legend.spacing.y = unit(0.3, "cm")        # Espaçamento vertical entre os itens da legenda
  )

# Plotando com ggplot
elnino_abaixo_gr <- ggplot() +
  ggtitle("El niño")+
  xlab("") +    ylab("") +
  geom_raster(data = elnino_abaixo_df, aes(x = lon, y = lat, fill = value), show.legend = TRUE) +
  scale_fill_manual(values = colors1, name = "Precipitação \n abaixo", labels = values, drop = FALSE,
                    guide = guide_legend(nrow = 1)) +
  geom_sf(data = bra_st, fill = "transparent", color = "black", size = 0.5) +
  theme_minimal()+
  legenda_nome+
  annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))

lanina_abaixo_gr <- ggplot() +
  xlab("") +    ylab("") +
  ggtitle("La niña")+
  geom_raster(data = lanina_abaixo_df, aes(x = lon, y = lat, fill = value), show.legend = TRUE) +
  scale_fill_manual(values = colors1, name = "Precipitação \n abaixo", labels = values, drop = FALSE,
                    guide = guide_legend(nrow = 1)) +
  geom_sf(data = bra_st, fill = "transparent", color = "black", size = 0.5) +
  theme_minimal()+
  legenda_nome+
  annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))

elnino_acima_gr <- ggplot() +
  xlab("") +    ylab("") +
  ggtitle("El niño")+
  geom_raster(data = elnino_acima_df, aes(x = lon, y = lat, fill = value), show.legend = TRUE) +
  scale_fill_manual(values = colors2, name = "Precipitação \n acima", labels = values, drop = FALSE,
                    guide = guide_legend(nrow = 1)) +
  geom_sf(data = bra_st, fill = "transparent", color = "black", size = 0.5) +
  theme_minimal()+
  legenda_nome+
  annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))

lanina_acima_gf <- ggplot() +
  xlab("") +    ylab("") +
  ggtitle("La niña")+
  geom_raster(data = lanina_acima_df, aes(x = lon, y = lat, fill = value), show.legend = TRUE) +
  scale_fill_manual(values = colors2, name = "Precipitação \n acima", labels = values, drop = FALSE,
                    guide = guide_legend(nrow = 1)) +
  geom_sf(data = bra_st, fill = "transparent", color = "black", size = 0.5) +
  theme_minimal()+
  legenda_nome+
  annotation_scale(location = "br", style = "bar", bar_cols = c("black", "white"))


grafico <- ggpubr::ggarrange(elnino_abaixo_gr, lanina_abaixo_gr, elnino_acima_gr,
                             lanina_acima_gf, ncol = 2,nrow = 2,common.legend = T,
                             legend = c("bottom") )

grafico1 <- ggpubr::ggarrange(elnino_abaixo_gr, lanina_abaixo_gr, ncol = 2,nrow = 1,common.legend = T,
                              legend = c("bottom") )
grafico2 <- ggpubr::ggarrange(elnino_acima_gr, lanina_acima_gf, ncol = 2,nrow = 1,common.legend = T,
                              legend = c("bottom") )
grafico3 <- ggpubr::ggarrange(grafico1, grafico2, ncol = 1,nrow = 2,common.legend = F,
                              legend = c("bottom") )

ggsave("results/Figura_2.png", plot = grafico3,
       width = 13, height = 13.5, dpi = 300)

