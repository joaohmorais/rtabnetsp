# rtabnetsp

The **rtabnetsp** package enables retrieval of health indicators data from São Paulo's TABNET system (http://portal.saude.sp.gov.br/links/matriz) in R. A list of available indicators is described below.

## Dependencies

Currently, the package has a few dependencies: **httr** (https://cran.r-project.org/web/packages/httr/httr.pdf) for HTTP requests, **xml2** (https://cran.r-project.org/web/packages/xml2/xml2.pdf) and **rvest** (https://cran.r-project.org/web/packages/rvest/rvest.pdf) for web scraping, **stringi** (https://cran.r-project.org/web/packages/stringi/stringi.pdf) for text manipulation, **purrr** (https://cran.r-project.org/web/packages/purrr/purrr.pdf) for treatment of exceptions, **tidyr** (https://cran.r-project.org/web/packages/tidyr/tidyr.pdf) and **dplyr** (https://cran.r-project.org/web/packages/dplyr/dplyr.pdf) for data manipulation, and **ggplot2** (https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf), **sf** (https://cran.r-project.org/web/packages/sf/sf.pdf), and **RColorBrewer** (https://cran.r-project.org/web/packages/RColorBrewer/
RColorBrewer.pdf) for graphics and mapping. 

## Installation

The current **rtabnetsp** version can be installed through this repository:

```r
install.packages("devtools")
devtools::install_github("joaohmorais/rtabnetsp")
```

## Usage

### Browsing available indicators

A list of available indicators can be obtained via `indicator_list()` function and searches can be made with `indicator_search()`. 

```r
indicator_list() # retrieve a list of available indicators
indicator_search("dengue") # search for indicators containing "dengue" in its name
```

### Retrieving indicator information

Before retrieving data from a selected indicator, it is important to check its **aggregation levels**, **time periods** and **subindicators** avaiable. This can be done through the `view_indicator()` function:

```r
view_indicator(12) # the indicator's index is the parameter
```

### Retrieving indicator data in dataframe form

Once you have chosen a indicator and checked its available aggregation levels, time periods and subindicators, a dataframe of desired content can be obtained through `indicator_df()` function:

```r
data <- indicator_df(
indicator_index = 12, # retrieve data from indicator with ID 12 
region = "Município", # in municipality aggregation level
subindicator = NULL, # if not specified, the last subindicator will be retrieved
years = c(2017, 2018) # desired time period
)
```

### Indicator mapping

Simple choropleth maps can be done with one command with **rtabnetsp**, with `tabnet_map()` function. The parameters are similar to the function listed above. 

```r
tabnet_map(
indicator_index = 12, # data from indicator with ID 12 will be represented in the map
region = "Município", # in municipality aggregation level
subindicator = NULL, # if not specified, the last subindicator will be retrieved
years = 2018, # desired time period
palette = "Purples" # RColorBrewer palette
)
```

There is also the `tabnet_map2()` function, which has the extra functionalities of displaying a north arrow and a scale bar on the map. This function however requires the **ggspatial** (https://cran.r-project.org/web/packages/ggspatial/ggspatial.pdf) package as well.

## Available indicators

| Indicator index (ID) |                                                                 Indicator name (TABNET)                                                               | # of subindicators | Available years |
|:-----------------------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------:|:------------------------:|:------------------:|
|                  1                  | 1b – População segundo a Fundação Sistema Estadual de Análise de Dados de São Paulo (SEADE)                                                      |             1            |      2000-2020     |
|                  2                  | 2 – Densidade demográfica segundo o Censo Demográfico da Fundação Instituto Brasileiro de Geografia e Estatística (IBGE)                         |             3            |      2000-2010     |
|                  3                  | 3 – Grau de urbanização segundo o Censo Demográfico da Fundação Instituto Brasileiro de Geografia e Estatística (IBGE)                           |             3            |      2000-2010     |
|                  4                  | 4 – Taxa de crescimento populacional (2000-2010) segundo o Censo Demográfico da Fundação Instituto Brasileiro de Geografia e Estatística (IBGE)  |             3            |      2010-2010     |
|                  5                  | 5 – Proporção de idosos                                                                                                                          |             3            |      2000-2018     |
|                  6                  | 6 – Taxa bruta de natalidade                                                                                                                     |             3            |      2000-2018     |
|                  7                  | 7 e 8 – Percentual de municípios com menos de 10 mil e com mais de 100 mil habitantes                                                            |             2            |      2010-2018     |
|                  8                  | Proporção (%) de municípios nos grupos 4 e 5, segundo Região de Saúde, DRS e RRAS do estado de São Paulo – 2008 a 2014 – versão 2016             |             1            |      2008-2014     |
|                  9                  | 10 – Produto interno bruto (PIB) per capita em reais (R$) corrente                                                                               |             3            |      2000-2017     |
|                  10                 | 11 a 13 – Taxa de mortalidade infantil e componentes                                                                                             |            11            |      2000-2019     |
|                  11                 | 14 – Razão de mortalidade materna                                                                                                                |             3            |      2000-2018     |
|                  12                 | 15 a 17 – Taxa de mortalidade por neoplasias                                                                                                     |            47            |      2000-2019     |
|                  13                 | 18 – Taxa de mortalidade por doenças do aparelho circulatório                                                                                    |             6            |      2000-2019     |
|                  14                 | 19 – Taxa de mortalidade por causas externas                                                                                                     |            17            |      2000-2019     |
|                  15                 | 20 a 23 – Percentual de óbitos por causas definidas                                                                                              |            18            |      2000-2019     |
|                  16                 | 24 e 25 – Percentual de nascidos vivos com baixo peso ao nascer                                                                                  |             5            |      2000-2019     |
|                  17                 | 26 – Percentual de partos em menores de 20 anos de idade                                                                                         |             3            |      2000-2018     |
|                  18                 | 27a – Taxa de letalidade por formas graves de dengue (FHD + SCD + DCC)                                                                           |             3            |      2000-2011     |
|                  19                 | 27b – Taxa de letalidade de dengue                                                                                                               |             3            |      2012-2018     |
|                  20                 | 28 – Taxa de incidência de aids                                                                                                                  |             3            |      2000-2018     |
|                  21                 | 29 – Taxa de incidência de sífilis congênita                                                                                                     |             3            |      2007-2018     |
|                  22                 | 30 – Prevalência de pacientes em diálise                                                                                                         |             3            |      2000-2017     |
|                  23                 | 31a – Leitos-SUS por 1000 (mil) habitantes                                                                                                       |             3            |      2005-2019     |
|                  24                 | 31b – Leitos-SUS por 1000 (mil) habitantes na população SUS-dependente                                                                           |             3            |      2005-2017     |
|                  25                 | 32 – Percentual de leitos-SUS em unidades de terapia intensiva (UTI)                                                                             |             3            |      2014-2019     |
|                  26                 | 33 – Cobertura da Atenção Básica em Saúde                                                                                                        |             3            |      2006-2018     |
|                  27                 | 34 – Cobertura de agente comunitário de saúde (ACS)                                                                                              |             3            |      2000-2017     |
|                  28                 | 35 – Proporção de nascidos vivos de mães com 7 ou mais consultas de pré-natal                                                                    |             3            |      2000-2019     |
|                  29                 | 36a – Percentual de internação SUS na população total residente                                                                                  |             3            |      2000-2018     |
|                  30                 | 36b – Percentual de internação SUS na população total residente para a população SUS-dependente                                                  |             3            |      2000-2018     |
|                  31                 | 37a – Razão de exames citopatológicos do colo do útero em mulheres de 25 a 64 anos de idade                                                      |             4            |      2008-2019     |
|                  32                 | 37b – Razão de exames citopatológicos do colo do útero em mulheres de 25 a 64 anos de idade na população SUS-dependente                          |             4            |      2008-2019     |
|                  33                 | 38 – Cobertura vacinal tetravalente (até 2012) e pentavalente (a partir de 2013) em menores de 1 ano de idade                                    |             3            |      2000-2018     |
|                  34                 | 39 – Média de consultas médicas por habitante nas especialidades básicas                                                                         |             3            |      2000-2017     |
|                  35                 | 40 – Proporção de consultas de urgência por consulta básica                                                                                      |             3            |      2000-2017     |
|                  36                 | 41 – Cobertura de 1ª consulta odontológica programática                                                                                          |             3            |      2000-2016     |
|                  37                 | 42 – Percentual de internação por condições sensíveis à Atenção Básica em saúde (ICSAB)                                                          |             3            |      2000-2018     |
|                  38                 | 43 – Taxa de internação por acidente vascular cerebral (AVC) em maiores de 40 anos de idade                                                      |             3            |      2000-2019     |
|                  39                 | 44 – Taxa de internação hospitalar por fratura de fêmur em maiores de 60 anos de idade                                                           |             3            |      2000-2018     |
|                  40                 | 45a – Percentual de partos cesáreos – Total (Sistema de Informações sobre Nascidos Vivos [Sinasc])                                               |             3            |      2000-2018     |
|                  41                 | 45b – Percentual de partos cesáreos pelo SUS (Sistema de Informações Hospitalares [SIH/SUS])                                                     |             4            |      2000-2018     |
|                  42                 | 46 – Cobertura de Centro de Atenção Psicossocial (CAPS)                                                                                          |             9            |      2012-2018     |
|                  43                 | 47a – Cobertura da Saúde Suplementar                                                                                                             |             3            |      2000-2019     |
|                  44                 | 47b – Estimativa da população SUS-dependente (com base na Saúde Suplementar)                                                                     |             4            |      2000-2019     |
|                  45                 | 48 – Proporção de cura de casos novos de tuberculose pulmonar bacilífera                                                                         |             3            |      2000-2018     |
|                  46                 | 49 – Proporção de cura de casos novos de hanseníase diagnosticados                                                                               |             3            |      2001-2017     |
|                  47                 | 50 a 52 – Índice de qualidade do tratamento da água nos parâmetros: bacteriológico, cloro e fluor                                                |             9            |      2008-2016     |
|                  48                 | 53 – Despesa em saúde per capita                                                                                                                 |             3            |      2000-2017     |
|                  49                 | 54a – Razão de mamografia de rastreamento em mulheres de 50 a 69 anos de idade                                                                   |             4            |      2010-2018     |
| 50                                  | 54a – Razão de mamografia de rastreamento em mulheres de 50 a 69 anos de idade SUS dependentes                                                   | 4                        | 2010-2019          |

## Contact

For questions or suggestions, contact me at `joao.tlp@gmail.com`.

Special thanks to Camila Bertini Martins, Arnaldo Sala, Thaís Konstantyner and Alvaro Fazenda.

This R package was developed for a final course assignment.
