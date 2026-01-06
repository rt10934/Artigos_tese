library(raster)
library(ncdf4)
library(dplyr)
nc_data <- nc_open("data_1/data.nc")
prob_data <- ncvar_get(nc_data, "prob")
# Função para criar um RasterLayer a partir de dados NetCDF
criar_raster <- function(dados, lon_min = -179.75, lon_max = 179.75, lat_min = -89.75, lat_max = 89.75) {
  
  # Definir as coordenadas de longitude e latitude
  lon <- seq(from = lon_min, to = lon_max, by = 0.5)  # Longitude com 720 pontos
  lat <- seq(from = lat_min, to = lat_max, by = 0.5)  # Latitude com 360 pontos
  
  # Criar o RasterLayer usando as coordenadas exatas
  raster_obj <- raster(t(dados), 
                       xmn = min(lon), xmx = max(lon), 
                       ymn = min(lat), ymx = max(lat), 
                       crs = CRS("+proj=longlat +datum=WGS84"))
  
  # Corrigir a orientação, se necessário
  raster_obj <- flip(raster_obj, direction = 'y')
  
  # Retornar o objeto RasterLayer
  return(raster_obj)
}


# baixando para o El nino, com chuva acima da média-----------------------------
# Criar RasterLayer para cada mês
elnino_january_acima <- criar_raster(prob_data[,,1,1,1,1])
elnino_february_acima <- criar_raster(prob_data[,,2,1,1,1])
elnino_march_acima <- criar_raster(prob_data[,,3,1,1,1])
elnino_april_acima <- criar_raster(prob_data[,,4,1,1,1])
elnino_may_acima <- criar_raster(prob_data[,,5,1,1,1])
elnino_june_acima <- criar_raster(prob_data[,,6,1,1,1])
elnino_july_acima <- criar_raster(prob_data[,,7,1,1,1])
elnino_august_acima <- criar_raster(prob_data[,,8,1,1,1])
elnino_september_acima <- criar_raster(prob_data[,,9,1,1,1])
elnino_october_acima <- criar_raster(prob_data[,,10,1,1,1])
elnino_november_acima <- criar_raster(prob_data[,,11,1,1,1])
elnino_december_acima <- criar_raster(prob_data[,,12,1,1,1])

# baixando para o El nino, com chuva abaixo da média-----------------------------
# Criar RasterLayer para cada mês
elnino_january_abaixo <- criar_raster(prob_data[,,1,1,2,1])
elnino_february_abaixo <- criar_raster(prob_data[,,2,1,2,1])
elnino_march_abaixo <- criar_raster(prob_data[,,3,1,2,1])
elnino_april_abaixo <- criar_raster(prob_data[,,4,1,2,1])
elnino_may_abaixo <- criar_raster(prob_data[,,5,1,2,1])
elnino_june_abaixo <- criar_raster(prob_data[,,6,1,2,1])
elnino_july_abaixo <- criar_raster(prob_data[,,7,1,2,1])
elnino_august_abaixo <- criar_raster(prob_data[,,8,1,2,1])
elnino_september_abaixo <- criar_raster(prob_data[,,9,1,2,1])
elnino_october_abaixo <- criar_raster(prob_data[,,10,1,2,1])
elnino_november_abaixo <- criar_raster(prob_data[,,11,1,2,1])
elnino_december_abaixo <- criar_raster(prob_data[,,12,1,2,1])
plot(elnino_december_abaixo)

# baixando para o La nina, com chuva acima da média-----------------------------
# Criar RasterLayer para cada mês
lanina_january_acima <- criar_raster(prob_data[,,1,2,1,1])
lanina_february_acima <- criar_raster(prob_data[,,2,2,1,1])
lanina_march_acima <- criar_raster(prob_data[,,3,2,1,1])
lanina_april_acima <- criar_raster(prob_data[,,4,2,1,1])
lanina_may_acima <- criar_raster(prob_data[,,5,2,1,1])
lanina_june_acima <- criar_raster(prob_data[,,6,2,1,1])
lanina_july_acima <- criar_raster(prob_data[,,7,2,1,1])
lanina_august_acima <- criar_raster(prob_data[,,8,2,1,1])
lanina_september_acima <- criar_raster(prob_data[,,9,2,1,1])
lanina_october_acima <- criar_raster(prob_data[,,10,2,1,1])
lanina_november_acima <- criar_raster(prob_data[,,11,2,1,1])
lanina_december_acima <- criar_raster(prob_data[,,12,2,1,1])

# baixando para o La Nina nino, com chuva abaixo da média-----------------------------
# Criar RasterLayer para cada mês
lanina_january_abaixo <- criar_raster(prob_data[,,1,2,2,1])
lanina_february_abaixo <- criar_raster(prob_data[,,2,2,2,1])
lanina_march_abaixo <- criar_raster(prob_data[,,3,2,2,1])
lanina_april_abaixo <- criar_raster(prob_data[,,4,2,2,1])
lanina_may_abaixo <- criar_raster(prob_data[,,5,2,2,1])
lanina_june_abaixo <- criar_raster(prob_data[,,6,2,2,1])
lanina_july_abaixo <- criar_raster(prob_data[,,7,2,2,1])
lanina_august_abaixo <- criar_raster(prob_data[,,8,2,2,1])
lanina_september_abaixo <- criar_raster(prob_data[,,9,2,2,1])
lanina_october_abaixo <- criar_raster(prob_data[,,10,2,2,1])
lanina_november_abaixo <- criar_raster(prob_data[,,11,2,2,1])
lanina_december_abaixo <- criar_raster(prob_data[,,12,2,2,1])
plot(lanina_january_acima)

# ajustando para fazer as manipulacoes:-----------------------------------------
corte <- 0.60
## El nino acima:----
elnino_acima <- data.frame(january =getValues(elnino_january_acima),
                           february =getValues(elnino_february_acima ),
                           march =getValues(elnino_march_acima ),
                           april =getValues(elnino_april_acima ), 
                           may =getValues(elnino_may_acima ), 
                           june =getValues(elnino_june_acima ), 
                           july =getValues(elnino_july_acima ), 
                           august =getValues(elnino_august_acima ), 
                           september =getValues(elnino_september_acima ), 
                           october =getValues(elnino_october_acima ), 
                           november =getValues(elnino_november_acima ),
                           december =getValues(elnino_december_acima ),
                           coordinates(lanina_february_abaixo))
# Aplicar o corte a todas as colunas dos meses
elnino_acima[, 1:12] <- lapply(elnino_acima[, 1:12], function(x) ifelse(x >= corte, x, 0))
elnino_acima[, 1:12] <- lapply(elnino_acima[, 1:12], function(x) ifelse(x <= corte, x, 1))
# manipulacao:
elnino_acima <- elnino_acima %>%
  dplyr::mutate(mes_acima = rowSums(select(., january, february, march, april, may,
                                           june, july, august, september, october,
                                           november, december), na.rm = TRUE))
elnino_acima <- rasterFromXYZ(xyz = elnino_acima[,c('x','y','mes_acima')], crs = "+proj=longlat +datum=WGS84 +no_defs ")  
plot(elnino_acima)
## El nino abixo:----
elnino_abaixo <- data.frame(january =getValues(elnino_january_abaixo),
                            february =getValues(elnino_february_abaixo ),
                            march =getValues(elnino_march_abaixo ),
                            april =getValues(elnino_april_abaixo ), 
                            may =getValues(elnino_may_abaixo ), 
                            june =getValues(elnino_june_abaixo ), 
                            july =getValues(elnino_july_abaixo ), 
                            august =getValues(elnino_august_abaixo ), 
                            september =getValues(elnino_september_abaixo ), 
                            october =getValues(elnino_october_abaixo ), 
                            november =getValues(elnino_november_abaixo ),
                            december =getValues(elnino_december_abaixo ),
                            coordinates(lanina_february_abaixo))
# Aplicar o corte a todas as colunas dos meses
elnino_abaixo[, 1:12] <- lapply(elnino_abaixo[, 1:12], function(x) ifelse(x >= corte, x, 0))
elnino_abaixo[, 1:12] <- lapply(elnino_abaixo[, 1:12], function(x) ifelse(x <= corte, x, 1))
# manipulacao:
elnino_abaixo <- elnino_abaixo %>%
  dplyr::mutate(mes_abaixo = rowSums(select(., january, february, march, april, may,
                                            june, july, august, september, october,
                                            november, december), na.rm = TRUE))
elnino_abaixo <- rasterFromXYZ(xyz = elnino_abaixo[,c('x','y','mes_abaixo')], crs = "+proj=longlat +datum=WGS84 +no_defs ")  
plot(elnino_abaixo)

## LA nina acima:----
lanina_acima <- data.frame(january =getValues(lanina_january_acima),
                           february =getValues(lanina_february_acima ),
                           march =getValues(lanina_march_acima ),
                           april =getValues(lanina_april_acima ), 
                           may =getValues(lanina_may_acima ), 
                           june =getValues(lanina_june_acima ), 
                           july =getValues(lanina_july_acima ), 
                           august =getValues(lanina_august_acima ), 
                           september =getValues(lanina_september_acima ), 
                           october =getValues(lanina_october_acima ), 
                           november =getValues(lanina_november_acima ),
                           december =getValues(lanina_december_acima ),
                           coordinates(lanina_february_abaixo))
# Aplicar o corte a todas as colunas dos meses
lanina_acima[, 1:12] <- lapply(lanina_acima[, 1:12], function(x) ifelse(x >= corte, x, 0))
lanina_acima[, 1:12] <- lapply(lanina_acima[, 1:12], function(x) ifelse(x <= corte, x, 1))

# manipulacao:
lanina_acima <- lanina_acima %>%
  dplyr::mutate(mes_acima = rowSums(select(., january, february, march, april, may,
                                           june, july, august, september, october,
                                           november, december), na.rm = TRUE))
lanina_acima <- rasterFromXYZ(xyz = lanina_acima[,c('x','y','mes_acima')], crs = "+proj=longlat +datum=WGS84 +no_defs")  
plot(lanina_acima)
## La nina abixo:----
lanina_abaixo <- data.frame(january =getValues(lanina_january_abaixo),
                            february =getValues(lanina_february_abaixo ),
                            march =getValues(lanina_march_abaixo ),
                            april =getValues(lanina_april_abaixo ), 
                            may =getValues(lanina_may_abaixo ), 
                            june =getValues(lanina_june_abaixo ), 
                            july =getValues(lanina_july_abaixo ), 
                            august =getValues(lanina_august_abaixo ), 
                            september =getValues(lanina_september_abaixo ), 
                            october =getValues(lanina_october_abaixo ), 
                            november =getValues(lanina_november_abaixo ),
                            december =getValues(lanina_december_abaixo ),
                            coordinates(lanina_february_abaixo))
# Aplicar o corte a todas as colunas dos meses
lanina_abaixo[, 1:12] <- lapply(lanina_abaixo[, 1:12], function(x) ifelse(x >= corte, x, 0))
lanina_abaixo[, 1:12] <- lapply(lanina_abaixo[, 1:12], function(x) ifelse(x <= corte, x, 1))
# manipulacao:
lanina_abaixo <- lanina_abaixo %>%
  dplyr::mutate(mes_abaixo = rowSums(select(., january, february, march, april, may,
                                            june, july, august, september, october,
                                            november, december), na.rm = TRUE))
lanina_abaixo <- rasterFromXYZ(xyz = lanina_abaixo[,c('x','y','mes_abaixo')], crs = "+proj=longlat +datum=WGS84 +no_defs")  
plot(lanina_abaixo)
# salvando os raster:

writeRaster(elnino_acima, "data_1/elnino_acima.tif", format = 'GTiff', overwrite = T)
writeRaster(elnino_abaixo, "data_1/elnino_abaixo.tif", format = 'GTiff', overwrite = T)
writeRaster(lanina_acima, "data_1/lanina_acima.tif", format = 'GTiff', overwrite = T)
writeRaster(lanina_abaixo, "data_1/lanina_abaixo.tif", format = 'GTiff', overwrite = T)