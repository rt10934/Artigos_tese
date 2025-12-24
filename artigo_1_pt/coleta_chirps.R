################################################################################
#-------------------------------------------------------------------------------
# COLETA PELO RGEE DOS TIFs PARA O CHIRPS---------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# otimizando com a criacao de funcões ------------------------------------------
#-------------------------------------------------------------------------------
library(terra)
library(dplyr)
library(geobr)
library(lubridate)
library(rgee)
#-------------------------------------------------------------------------------
# importando apenas os TIFs-----------------------------------------------------
#-------------------------------------------------------------------------------
# Definir uma região de interesse (por exemplo, retângulo)
coords_brasil <- c(-73.982816, -33.751789, -34.729993, 5.271995)
geometry <- rgee::ee$Geometry$Rectangle(
  coords = c(coords_brasil[1] - 5, coords_brasil[2] - 5, 
             coords_brasil[3] + 5, coords_brasil[4] + 5),
  proj = "EPSG:4326",
  geodesic = FALSE
)
rm(coords_brasil)

# Função para baixar e processar dados CHIRPS para uma data específica
process_chirps_tif <- function(date) {
  # Formatar a data
  date_str <- format(as.Date(date), "%Y-%m-%d")
  # Construir a URL do arquivo COG
  colecao <- rgee::ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY") |> 
    rgee::ee$ImageCollection$filterDate(date_str) |>
    rgee::ee$ImageCollection$map(function(x) x$select("precipitation")) |> # Select only precipitation bands
    rgee::ee$ImageCollection$toBands()
  # impotando dados
  cog_data <- ee_as_rast(
    image = colecao,
    region = geometry,
    via = "drive"
  )
  # Defina o caminho para o arquivo de saída COG
  output_file <- paste0("~/Tese/CHIRPS/",year(date_str),"/CHIRPS_",date_str,".cog.tif")
  # Escreva o objeto SpatRaster no formato COG
  raster::writeRaster(cog_data, output_file,overwrite=TRUE)
  # apagar arquivos no drive
  ee_clean_container(name = "rgee_backup", type="drive")
  
  return(data_frame)
}
# Segunda funcao
# Função para processar e combinar resultados de múltiplas datas específicas
collet_multiple_dates <- function(dates, brasil_shape, shape) {
  # Lista para armazenar os data frames de cada data
  results_list <- lapply(dates, function(date) {
    process_chirps_tif(date)
  })}
# Coletando os dados -----------------------------------------------------------
# 2023----
all_dates_2023 <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")
collet_multiple_dates(all_dates_2023)

# 2022----
all_dates_2022 <- seq(from = as.Date("2022-01-01"), to = as.Date("2022-12-31"), by = "day")
collet_multiple_dates(all_dates_2022)

# 2021----#
all_dates_2021 <- seq(from = as.Date("2021-02-20"), to = as.Date("2021-12-31"), by = "day")
collet_multiple_dates(all_dates_2021)

# 2020----
all_dates_2020 <- seq(from = as.Date("2020-04-05"), to = as.Date("2020-12-31"), by = "day")
collet_multiple_dates(all_dates_2020)

# 2019----
all_dates_2019 <- seq(from = as.Date("2019-01-01"), to = as.Date("2019-12-31"), by = "day")
collet_multiple_dates(all_dates_2019)

# 2018----
all_dates_2018 <- seq(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by = "day")
collet_multiple_dates(all_dates_2018)

# 2017----
all_dates_2017 <- seq(from = as.Date("2017-01-01"), to = as.Date("2017-12-31"), by = "day")
collet_multiple_dates(all_dates_2017)

# 2016----
all_dates_2016 <- seq(from = as.Date("2016-01-01"), to = as.Date("2016-12-31"), by = "day")
collet_multiple_dates(all_dates_2016)

# 2015----
all_dates_2015 <- seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = "day")
collet_multiple_dates(all_dates_2015)

# 2014----
all_dates_2014 <- seq(from = as.Date("2014-01-01"), to = as.Date("2014-12-31"), by = "day")
collet_multiple_dates(all_dates_2014)

# 2013----
all_dates_2013 <- seq(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), by = "day")
collet_multiple_dates(all_dates_2013)

# 2012----
all_dates_2012 <- seq(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "day")
collet_multiple_dates(all_dates_2012)

# 2011----
all_dates_2011 <- seq(from = as.Date("2011-01-01"), to = as.Date("2011-12-31"), by = "day")
collet_multiple_dates(all_dates_2011)

# 2010----
all_dates_2010 <- seq(from = as.Date("2010-01-01"), to = as.Date("2010-12-31"), by = "day")
collet_multiple_dates(all_dates_2010)

# 2009----
all_dates_2009 <- seq(from = as.Date("2009-01-01"), to = as.Date("2009-12-31"), by = "day")
collet_multiple_dates(all_dates_2009)

# 2008----
all_dates_2008 <- seq(from = as.Date("2008-01-01"), to = as.Date("2008-12-31"), by = "day")
collet_multiple_dates(all_dates_2008)

# 2007----
all_dates_2007 <- seq(from = as.Date("2007-01-01"), to = as.Date("2007-12-31"), by = "day")
collet_multiple_dates(all_dates_2007)

# 2006----
all_dates_2006 <- seq(from = as.Date("2006-01-01"), to = as.Date("2006-12-31"), by = "day")
collet_multiple_dates(all_dates_2006)

# 2005----
all_dates_2005 <- seq(from = as.Date("2005-01-01"), to = as.Date("2005-12-31"), by = "day")
collet_multiple_dates(all_dates_2005)

# 2004----
all_dates_2004 <- seq(from = as.Date("2004-10-23"), to = as.Date("2004-12-31"), by = "day")
collet_multiple_dates(all_dates_2004)

# 2003----
all_dates_2003 <- seq(from = as.Date("2003-01-01"), to = as.Date("2003-12-31"), by = "day")
collet_multiple_dates(all_dates_2003)

# 2002----
all_dates_2002 <- seq(from = as.Date("2002-07-21"), to = as.Date("2002-12-31"), by = "day")
collet_multiple_dates(all_dates_2002)

# 2001----
all_dates_2001 <- seq(from = as.Date("2001-01-01"), to = as.Date("2001-12-31"), by = "day")
collet_multiple_dates(all_dates_2001)

# 2000----
all_dates_2000 <- seq(from = as.Date("2000-01-01"), to = as.Date("2000-12-31"), by = "day")
collet_multiple_dates(all_dates_2000)
