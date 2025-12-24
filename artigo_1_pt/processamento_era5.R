#-------------------------------------------------------------------------------
# PROCESSANDO OS TIFs COLETADOS PARA O ERA5 ------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# otimizando com a criacao de funcões ------------------------------------------
#-------------------------------------------------------------------------------
library(terra)
library(dplyr)
library(geobr)
library(lubridate)
# dados que se repetem e prcisam ser buscados ----------------------------------
brasil_shape <-  geobr::read_country(year=2010)
shape <- readRDS("C:/Tese/shapefile_18_1.rds")

# Funcoes-----------------------------------------------------------------------
# Funcao etapa 1-----
# Função para baixar e processar dados CHIRPS para uma data específica
process_era_daily <- function(date) {
  # Carregar o shapefile do Brasil
  brasil_shape <- brasil_shape
  
  # Carregar o shapefile específico
  shape <- shape
  
  # Formatar a data
  date_str <- format(as.Date(date), "%Y-%m-%d")
  
  # Construir a URL do arquivo COG
  year <- format(as.Date(date), "%Y")
  url_cog <- paste0("C:/Tese/TIFs/ERA5/",
                    year, "/ERA5_", date_str, ".cog.tif")
  
  # Carregar o arquivo COG
  cog_data <- rast(url_cog)
  cog_data[cog_data == -9999] <- NA  # Substituir valores inválidos
  
  # Aplicar máscara usando o shapefile do Brasil
  cog_data_masked <- mask(cog_data, brasil_shape)
  
  # Extrair e calcular a média da precipitação
  precip_media <- extract(cog_data_masked, shape, fun=mean, na.rm=TRUE)
  
  # Criar o dataframe final com os resultados e a data
  names(precip_media)[2] <- "precipitacao"
  
  data_frame <- precip_media |>
    dplyr::mutate(data = date_str,
                  precipitacao = precipitacao*1000)
  
  
  return(data_frame)
}
# Funcao etapa 2----------------------------------------------------------------

# Função para processar e combinar resultados de múltiplas datas específicas
process_multiple_dates <- function(dates, brasil_shape, shape) {
  # Lista para armazenar os data frames de cada data
  results_list <- lapply(dates, function(date) {
    process_era_daily(date)
  })
  
  # Combinar todos os data frames da lista em um único data frame
  final_result <- bind_rows(results_list)
  return(final_result)
}
# Aplicacao para os anos da pesquisa--------------------------------------------
# conunto de datas :
# 2000----
all_dates_2000 <- seq(from = as.Date("2000-01-01"), to = as.Date("2000-12-31"), by = "day")
result_2000 <- process_multiple_dates(all_dates_2000)
saveRDS(result_2000,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2000.rds")
remove(all_dates_2000, result_2000)

# 2001----
all_dates_2001 <- seq(from = as.Date("2001-01-01"), to = as.Date("2001-12-31"), by = "day")
result_2001 <- process_multiple_dates(all_dates_2001)
saveRDS(result_2001,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2001.rds")
remove(all_dates_2001, result_2001)

# 2002----
all_dates_2002 <- seq(from = as.Date("2002-01-01"), to = as.Date("2002-12-31"), by = "day")
result_2002 <- process_multiple_dates(all_dates_2002)
saveRDS(result_2002,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2002.rds")
remove(all_dates_2002, result_2002)

# 2003----
all_dates_2003 <- seq(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), by = "day")
result_2003 <- process_multiple_dates(all_dates_2003)
saveRDS(result_2003,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2003.rds")
remove(all_dates_2003, result_2003)

# 2004----
all_dates_2004 <- seq(from = as.Date("2004-01-01"), to = as.Date("2004-12-31"), by = "day")
result_2004 <- process_multiple_dates(all_dates_2004)
saveRDS(result_2004,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2004.rds")
remove(all_dates_2004, result_2004)

# 2005----
all_dates_2005 <- seq(from = as.Date("2005-01-01"), to = as.Date("2005-12-31"), by = "day")
result_2005 <- process_multiple_dates(all_dates_2005)
saveRDS(result_2005,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2005.rds")
remove(all_dates_2005, result_2005)

# 2006----
all_dates_2006 <- seq(from = as.Date("2006-01-01"), to = as.Date("2006-12-31"), by = "day")
result_2006 <- process_multiple_dates(all_dates_2006)
saveRDS(result_2006,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2006.rds")
remove(all_dates_2006, result_2006)

# 2007----
all_dates_2007 <- seq(from = as.Date("2007-01-01"), to = as.Date("2007-12-31"), by = "day")
result_2007 <- process_multiple_dates(all_dates_2007)
saveRDS(result_2007,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2007.rds")
remove(all_dates_2007, result_2007)

# 2008----
all_dates_2008 <- seq(from = as.Date("2008-01-01"), to = as.Date("2008-12-31"), by = "day")
result_2008 <- process_multiple_dates(all_dates_2008)
saveRDS(result_2008,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2008.rds")
remove(all_dates_2008, result_2008)

# 2009----
all_dates_2009 <- seq(from = as.Date("2009-01-01"), to = as.Date("2009-12-31"), by = "day")
result_2009 <- process_multiple_dates(all_dates_2009)
saveRDS(result_2009,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2009.rds")
remove(all_dates_2009, result_2009)

# 2010----
all_dates_2010 <- seq(from = as.Date("2010-01-01"), to = as.Date("2010-12-31"), by = "day")
result_2010 <- process_multiple_dates(all_dates_2010)
saveRDS(result_2010,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2010.rds")
remove(all_dates_2010, result_2010)

# 2011----
all_dates_2011 <- seq(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "day")
result_2011 <- process_multiple_dates(all_dates_2011)
saveRDS(result_2011,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2011.rds")
remove(all_dates_2011, result_2011)

# 2012----
all_dates_2012 <- seq(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "day")
result_2012 <- process_multiple_dates(all_dates_2012)
saveRDS(result_2012,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2012.rds")
remove(all_dates_2012, result_2012)

# 2013----
all_dates_2013 <- seq(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), by = "day")
result_2013 <- process_multiple_dates(all_dates_2013)
saveRDS(result_2013,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2013.rds")
remove(all_dates_2013, result_2013)

# 2014----
all_dates_2014 <- seq(from = as.Date("2014-01-01"), to = as.Date("2014-12-31"), by = "day")
result_2014 <- process_multiple_dates(all_dates_2014)
saveRDS(result_2014,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2014.rds")
remove(all_dates_2014, result_2014)

# 2015----
all_dates_2015 <- seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = "day")
result_2015 <- process_multiple_dates(all_dates_2015)
saveRDS(result_2015,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2015.rds")
remove(all_dates_2015, result_2015)

# 2016----
all_dates_2016 <- seq(from = as.Date("2016-01-01"), to = as.Date("2016-12-31"), by = "day")
result_2016 <- process_multiple_dates(all_dates_2016)
saveRDS(result_2016,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2016.rds")
remove(all_dates_2016, result_2016)

# 2017----
all_dates_2017 <- seq(from = as.Date("2017-01-01"), to = as.Date("2017-12-31"), by = "day")
result_2017 <- process_multiple_dates(all_dates_2017)
saveRDS(result_2017,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2017.rds")
remove(all_dates_2017, result_2017)

# 2018----
all_dates_2018 <- seq(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by = "day")
result_2018 <- process_multiple_dates(all_dates_2018)
saveRDS(result_2018,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2018.rds")
remove(all_dates_2018, result_2018)

# 2019----
all_dates_2019 <- seq(from = as.Date("2019-01-01"), to = as.Date("2019-12-31"), by = "day")
result_2019 <- process_multiple_dates(all_dates_2019)
saveRDS(result_2019,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2019.rds")
remove(all_dates_2019, result_2019)

# 2020----
all_dates_2020 <- seq(from = as.Date("2020-01-01"), to = as.Date("2020-12-31"), by = "day")
result_2020 <- process_multiple_dates(all_dates_2020)
saveRDS(result_2020,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2020.rds")
remove(all_dates_2020, result_2020)

# 2021----
all_dates_2021 <- seq(from = as.Date("2021-01-01"), to = as.Date("2021-12-31"), by = "day")
result_2021 <- process_multiple_dates(all_dates_2021)
saveRDS(result_2021,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2021.rds")
remove(all_dates_2021, result_2021)

# 2022----
all_dates_2022 <- seq(from = as.Date("2022-01-01"), to = as.Date("2022-12-31"), by = "day")
result_2022 <- process_multiple_dates(all_dates_2022)
saveRDS(result_2022,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2022.rds")
remove(all_dates_2022, result_2022)

# 2023----
all_dates_2023 <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")
result_2023 <- process_multiple_dates(all_dates_2023)
saveRDS(result_2023,file = "C:/Tese/TIFs/ERA5_RESULTADOS/ERA_2023.rds")
remove(all_dates_2023, result_2023)





