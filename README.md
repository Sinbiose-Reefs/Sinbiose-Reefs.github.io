Resumo de política apresentado ao CNPq / Policy brief presented to the
Brazilian CNPq
================
Reef Synthesis Working Group (página feita por / webpage made by André
L. Luza)
2023-05-17

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

<img src="output/Logo Completa CORES GDE.png" width="60%" height="60%" style="display: block; margin: auto;" />

### Fontes de dados / data sources

Os dados sobre a representatividade das áreas marinhas protegidas dentro
da Zona Econômica Exclusiva do Brasil foram obtidos da seguinte fonte /
*Data about the representation of marine protected areas within the
Economic Exclusive Zone were gathered from the following source*:

Instituto Brasileiro do Meio Ambiente e dos Recursos Naturais
Renováveis. Relatório de qualidade do meio ambiente. \[recurso
eletrônico\]: RQMA Brasil 2020 / Hanry Alves Coelho, Andrea Alimandro
Corrêa (coordenação). – Brasília, DF: IBAMA, 2022. 558 p. Acessado em /
Accessed in: 08/05/2023. Disponível em / Available at:
\[<https://www.gov.br/ibama/pt-br/phocadownload/qualidadeambiental/relatorios/2022/2022-06-03_RQMA_Brasil_2020.pdf>\]

As porcentagens de representatividade de ecossistemas recifais dentro de
áreas marinhas protegidas foram obtidas de Rafael Magris, baseadas em
dados do Instituto Chico Mendes de Conservação da Biodiversidade
(ICMBio) / *The representativeness of reef ecosystems within marine
protected areas were obtained from Rafael Magris, based on data from the
Chico Mendes Institute for Biodiversity Conservation (ICMBio)*.

<!-- badges: start -->
<!-- badges: end -->

A lista de espécies de peixes recifais foi obtida da seguinte fonte /
*The reef fish checklist was gathered from the following reference*:

Pinheiro, HT, Rocha, LA, Macieira, RM, et al. South-western Atlantic
reef fishes: Zoogeographical patterns and ecological drivers reveal a
secondary biodiversity centre in the Atlantic Ocean. Divers Distrib.
2018; 24: 951– 965. <https://doi.org/10.1111/ddi.12729>

A lista de táxons bentônicos foi produzida segundo as seguintes fontes
de dados (alguns dados ainda não foram publicados) / *The list of
benthic taxa was produced using data from the following references (some
data are unpublished)*:

Aued AW, Smith F, Quimbayo JP, Cândido DV, Longo GO, Ferreira CEL, …
Segal B (2018). Large-scale patterns of benthic marine communities in
the Brazilian Province. PLoS ONE, 13(6), e0198452.
<https://doi.org/10.1371/journal.pone.0198452>

Santana, EFC, Mies, M, Longo, GO, Menezes, R, Aued, AW, Luza, AL, …
Francini-Filho, RB. (2023). Turbidity shapes shallow Southwestern
Atlantic benthic reef communities. Marine Environmental Research, 183,
105807. <https://doi.org/10.1016/j.marenvres.2022.105807>

Roos NC, Pennino MG, Carvalho AR, Longo GO (2019) Drivers of abundance
and biomass of Brazilian parrotfishes. Mar Ecol Prog Ser 623:117-130.
<https://doi.org/10.3354/meps13005>

Francini-Filho RB, Coni EOC, Meirelles PM, Amado-Filho GM, Thompson FL,
Pereira-Filho GH, et al. (2013) Dynamics of Coral Reef Benthic
Assemblages of the Abrolhos Bank, Eastern Brazil: Inferences on Natural
and Anthropogenic Drivers. PLoS ONE 8(1): e54260.
<https://doi.org/10.1371/journal.pone.0054260>

Cordeiro et al.: PELD Ilhas Oceânicas Brasileiras (dados não publicados)
/ *LTER Brazilian Oceanic Islands (unpublished data)*.

Todos os conjuntos de dados foram utilizados para delimitar o número de
táxons de corais (a nível de gênero e espécie), enquanto que somente os
dados de Aued et al. (2018) foram utilizados para delimitar o número de
táxons de outros grupos taxonômicos. Nós tomamos essa decisão porque a
resolução taxonômica dos corais é similar entre os diferentes conjuntos
de dados, enquanto difere substancialmente entre os conjuntos de dados
para os outros grupos / *All datasets were used to determine the number
of coral taxa (at the genus and species level), while only the dataset
of Aued et al. (2018) was used to determine the number of taxa for other
taxonomic groups. We made this decision because the taxonomic resolution
of corals is similar among the different datasets, while it differs
among datasets for the other groups.*

Os dados de desembarque foram obtidos da seguinte fonte / *Data of
fisheries’ landings were gathered from the following source*:

Freire KMF, Almeida da Silva Z, Trindade Amador J, Aragão JA, Araújo da
Rocha AR, Ávila-da-Silva AO, Bentes B, Carneiro MH, Chiquieri J,
Fernandes Freire C, Bezerra Figueiredo M, Hostim-Silva M, Jimenez EA,
Keunecke KA, Lopes PF de M, Mendonça J, Musiello- Fernandes J, Olavo G,
Primitivo C, Rotundo M, Santana R, Sant’Ana R, Scheidt G, Abdon da Silva
LM, Trindade-Santos I, Velasco G, Vianna M (2021) Reconstruction of
Marine Commercial Landings for the Brazilian Industrial and Artisanal
Fisheries From 1950 to 2015. Front Mar Sci 8:1–16. DOI:
10.3389/fmars.2021.659110

<!-- badges: start -->
<!-- badges: end -->

### Decisões analíticas importantes / *Important analytical decisions*:

- Dados de desembarque de 2000 pra diante foram utilizados para o
  cálculo da contribuição da pesca artesanal e industrial para o
  montante total desembarcado nas regiões / *Landing data from 2000
  onwards were used to calculate the contribution of artisanal and
  industrial sectors to the total amount landed in the regions*.

<!-- badges: start -->
<!-- badges: end -->

### O resumo foi produzido utilizando o seguinte software e pacotes / *This policy brief was produced using the following software and associated packages*:

    ## R version 4.3.0 (2023-04-21 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## time zone: America/Sao_Paulo
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] stars_0.6-1         abind_1.4-5         ggnewscale_0.4.8   
    ##  [4] elevatr_0.4.2       openxlsx_4.2.5.2    ggrepel_0.9.3      
    ##  [7] gridExtra_2.3       reshape_0.8.9       vctrs_0.6.2        
    ## [10] lubridate_1.9.2     forcats_1.0.0       stringr_1.5.0      
    ## [13] dplyr_1.1.2         purrr_1.0.1         readr_2.1.4        
    ## [16] tidyr_1.3.0         tibble_3.2.1        ggplot2_3.4.2      
    ## [19] tidyverse_2.0.0     raster_3.6-20       maptools_1.1-6     
    ## [22] rgeos_0.6-2         rgdal_1.6-6         sp_1.6-0           
    ## [25] rnaturalearth_0.3.2 here_1.0.1          sf_1.0-12          
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.3       xfun_0.39          lattice_0.21-8     tzdb_0.3.0        
    ##  [5] tools_4.3.0        generics_0.1.3     parallel_4.3.0     proxy_0.4-27      
    ##  [9] fansi_1.0.4        highr_0.10         pkgconfig_2.0.3    KernSmooth_2.23-20
    ## [13] lifecycle_1.0.3    compiler_4.3.0     munsell_0.5.0      terra_1.7-29      
    ## [17] codetools_0.2-19   htmltools_0.5.5    class_7.3-21       yaml_2.3.7        
    ## [21] pillar_1.9.0       classInt_0.4-9     lwgeom_0.2-11      zip_2.3.0         
    ## [25] tidyselect_1.2.0   digest_0.6.31      stringi_1.7.12     rprojroot_2.0.3   
    ## [29] fastmap_1.1.1      colorspace_2.1-0   cli_3.6.1          magrittr_2.0.3    
    ## [33] utf8_1.2.3         e1071_1.7-13       foreign_0.8-84     withr_2.5.0       
    ## [37] scales_1.2.1       timechange_0.2.0   rmarkdown_2.21     httr_1.4.6        
    ## [41] progressr_0.13.0   hms_1.1.3          evaluate_0.20      knitr_1.42        
    ## [45] rlang_1.1.1        Rcpp_1.0.10        glue_1.6.2         DBI_1.1.3         
    ## [49] rstudioapi_0.14    jsonlite_1.8.4     plyr_1.8.8         R6_2.5.1          
    ## [53] units_0.8-2
