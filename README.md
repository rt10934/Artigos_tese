# Artigo 1

## Primeira etapa, coleta e organização dos resultados:

**-ERA5** Para o ERA5 foi necessário duas etapas, a primeira foi a coleta dos dados com o script "coleta_era5", posteriormente foi ajustado para juntar os dados por ano com o script "processamento_era5"

**-CHIRPS** Para o CHIRPS foi necessário duas etapas, a primiera foi a coleta dos dados com o script "coleta_chirps", posteriormente foi ajustado para juntar os dados por ano com o script "processamento_chirps"

**-PERSIANN** Para o CHIRPS foi necessário duas etapas, a primiera foi a coleta dos dados com o script "coleta_persiann", posteriormente foi ajustado para juntar os dados por ano com o script "processamento_persiann"

**-BR_DWGD** Para o CHIRPS foi necessário duas etapas, a primiera foi a coleta dos dados com o script "coleta_br_dwgd", posteriormente foi ajustado para juntar os dados por ano com o script "processamento_br_dwgd"

## Segunda etapa, foi a junção das bases de dados:

**-Junção dos dados:** No script "script_modelagem_v1", a qual resultuou em um arquivo só do conjunto de dados, sendo salvo como "base_prec"

## Terceira etapa, analise dos dados: (parei aqui)

**-Analise preliminar:** No script "Script_modelagem_v2" foi realizada uma analise preliminar, com o objetivo de avaliar se os dados estão corretos, e obter resultados preliminares, com isso, tem-se o RMSE, coeficiente de corelação, probabilidade de detecção e mapas da distribuição das chuvas.

**-Testes com sub amostras** No script "Script_modelagem_v2" foi realizads testes que avaliam a capacidade de dos dados em certas circustâcias, como:

## OBSERVAÇÔES:

**ERA5** para os hexagonos: "5821", "5819", "5763" há problemas.... (versao 1 não tem probrlas)

**PERSIANN** Nos dias "2011-12-13", "2012-11-27", "2012-11-28", "2012-11-29" e "2021-07-08"

## Resultados e Metodologia:
**- Classe_bioma "dados":** No script "Classe_bioma" foi feito um dicionario para localizar os biomas e regiões nos hexagonos 

**-Figura "dados":** No script "Figuras_dados_metodologia" foi feita o mapa para a exemplificacao do como são os hexagonos

**-Figura "Não denomindada":** No script "Script_modelagem_v4" foi realizada uma analise com a montagem das figuras a cercas dos testes "ME", "MAE", "RMSE" e "PD" que foram incluidos no texto em uma figura conjunta, denomindada de "colocar nome" 

**-Script_modelagem_v5** Foi realizada uma analise e montagem das tabelas relacionadas aos "Resultado gerais sobre da comparação das estimativas dos SSP com os dados do BR-DWGD no período de 2000 a 2022" e aos resultados da tabela "".






