# Article 1

### OBS1: Data from this stage must be downloaded and placed in a folder named "dados_1", within the environment

### OBS2: A folder named "results" must also be created to store the outputs.

## Stages of figures and results:

## First stage, two phases:

### Construction of Figure 1 (La Niña and El Niño)

The data from "ENSO" were first processed and saved in raster format in the script "la_nina_el_nino"

Subsequently, the figure included in the manuscript was produced, corresponding to Figure 2, which is available in the script "mapas_enso"

### Construction of the methodology figure:

In the script "mapas_metodologia", Figure 3 was produced, which is included in the methodology and data section.

## Second stage, collection and organization of results (due to the data volume, they were not made available):

**-ERA5** For ERA5, two steps were required. The first was data collection using the script "coleta_era5", and subsequently the data were adjusted to be aggregated by year using the script "processamento_era5"

**-CHIRPS** For CHIRPS, two steps were required. The first was data collection using the script "coleta_chirps", and subsequently the data were adjusted to be aggregated by year using the script "processamento_chirps"

**-PERSIANN** For PERSIANN, two steps were required. The first was data collection using the script "coleta_persiann", and subsequently the data were adjusted to be aggregated by year using the script "processamento_persiann"

**-BR_DWGD** For BR_DWGD, two steps were required. The first was data collection using the script "coleta_br_dwgd", and subsequently the data were adjusted to be aggregated by year using the script "processamento_br_dwgd"

## Third stage, merging of the datasets (due to the data volume, they were not made available):

**-Data merging:** In the script "script_modelagem_v1", which resulted in a single dataset file, saved as "base_prec"

**-Regional dictionary-** As the construction of the hexagon grid was operationalized, it was necessary to identify which region each hexagon belongs to in the script "dicionario_regiao"


## Fourth stage, data analysis:
**-Precipitation results-** In the script "Script_modelagem_v2", Figure 4 and Table 1 of the manuscript were produced.

**-Overall and regional results-** In the script "Script_modelagem_v3", Figure 5 and Figure 7 were produced.

**-Hexagon-level results-** In the script "Script_modelagem_v4", Figure 6 was produced.

**-Climate seasons results-** In the script "Script_modelagem_v5", Figure 8 and Appendix Table 3 were produced.

**-PD results-** In the script "Script_modelagem_v6", Figure 9 and Appendix Table 4 were produced.

**-Overall results** In the script "Script_modelagem_v7", Appendix Table 2 was produced.
