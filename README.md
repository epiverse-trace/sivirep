
<!-- README.md is generated from README.Rmd. Please edit that file -->

## *sivirep*: Authomated reporting from epidemiological surveillance databases <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/readepi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/readepi/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/readepi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/readepi?branch=main)
[![lifecycle-concept](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-concept.svg)](https://www.reconverse.org/lifecycle.html#concept)
[![R-CMD-check](https://github.com/epiverse-trace/sivirep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/sivirep/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Current version of *sivirep* provides functions for data wrangling and
automated reports from
[SIVIGILA](https://www.ins.gov.co/Direcciones/Vigilancia/Paginas/SIVIGILA.aspx),
which is the official epidemiological surveillance system for Colombia,
South America.

Future versions of this package may include other epidemiological
surveillance systems in Latin America.

## Installation

You can install the **development version** of `sivirep` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("epiverse-trace/sivirep")
```

## Quick start

To view the list of diseases available to make a report with `sivirep`:

``` r
library(sivirep)

list_of_diseases <- list_avaliable_diseases_and_years()
list_of_diseases
#> $disease
#>  [1] "CÁNCER DE LA MAMA Y CUELLO UTERINO"                               
#>  [2] "CANCER INFANTIL"                                                  
#>  [3] "CANCER_EN MENORES DE 18 AÑOS"                                     
#>  [4] "CHAGAS"                                                           
#>  [5] "CHIKUNGUNYA"                                                      
#>  [6] "DENGUE"                                                           
#>  [7] "Dengue"                                                           
#>  [8] "DENGUE GRAVE"                                                     
#>  [9] "DESNUTRICION AGUDA EN MENORES DE 5 AÑOS"                          
#> [10] "DIFTERIA"                                                         
#> [11] "ENCEFALITIS DEL NILO OCCIDENTAL EN HUMANOS"                       
#> [12] "ENCEFALITIS EQUINA DEL OESTE EN HUMANOS"                          
#> [13] "ENCEFALITIS EQUINA VENEZOLANA EN HUMANOS"                         
#> [14] "ENDOMETRITIS PUERPERAL"                                           
#> [15] "ENFERMEDADES HUERFANAS - RARAS"                                   
#> [16] "ESI - IRAG (VIGILANCIA CENTINELA)"                                
#> [17] "EVENTO ADVERSO SEGUIDO A LA VACUNACION"                           
#> [18] "EXPOSICIÓN A FLUOR"                                               
#> [19] "FIEBRE AMARILLA"                                                  
#> [20] "FIEBRE TIFOIDEA Y PARATIFOIDEA"                                   
#> [21] "HEPATITIS A"                                                      
#> [22] "HEPATITIS B"                                                      
#> [23] "HEPATITIS C"                                                      
#> [24] "HIPOTIROIDISMO CONGENITO"                                         
#> [25] "INFECCION ASOCIADA A DISPOSITIVOS"                                
#> [26] "INFECCION RESPIRATORIA AGUDA GRAVE IRAG INUSITADA"                
#> [27] "INTENTO DE SUICIDIO"                                              
#> [28] "INTOXICACION POR FARMACOS"                                        
#> [29] "INTOXICACION POR METALES PESADOS"                                 
#> [30] "INTOXICACION POR METANOL"                                         
#> [31] "INTOXICACION POR MONOXIDO DE CARBONO Y OTROS GASES"               
#> [32] "INTOXICACION POR OTRAS SUSTANCIAS QUIMICAS"                       
#> [33] "INTOXICACION POR PLAGUICIDAS"                                     
#> [34] "INTOXICACION POR SOLVENTES"                                       
#> [35] "INTOXICACION POR SUSTANCIAS PSICOACTIVAS"                         
#> [36] "ISO"                                                              
#> [37] "LEISHMANIASIS CUTANEA"                                            
#> [38] "LEISHMANIASIS MUCOSA"                                             
#> [39] "LEISHMANIASIS VISCERAL"                                           
#> [40] "LEPRA"                                                            
#> [41] "LEPTOSPIROSIS"                                                    
#> [42] "LESIONES POR ARTEFACTOS EXPLOSIVOS (PÓLVORA Y MINAS ANTIPERSONAL)"
#> [43] "LEUCEMIA AGUDA PEDIATRICA LINFOIDE"                               
#> [44] "LEUCEMIA AGUDA PEDIATRICA MIELOIDE"                               
#> [45] "MALARIA ASOCIADA (FORMAS MIXTAS)"                                 
#> [46] "MALARIA COMPLICADA"                                               
#> [47] "MALARIA FALCIPARUM"                                               
#> [48] "MALARIA VIVAX"                                                    
#> [49] "MENINGITIS  OTROS"                                                
#> [50] "MENINGITIS MENINGOCÓCICA"                                         
#> [51] "MENINGITIS POR HAEMOPHILUS INFLUENZAE"                            
#> [52] "MENINGITIS POR NEUMOCOCO"                                         
#> [53] "MENINGITIS TUBERCULOSA"                                           
#> [54] "MORBILIDAD MATERNA EXTREMA"                                       
#> [55] "MORTALIDAD MATERNA"                                               
#> [56] "MORTALIDAD PERINATAL Y NEONATAL TARDIA"                           
#> [57] "MORTALIDAD POR DENGUE"                                            
#> [58] "MORTALIDAD POR DESNUTRICION"                                      
#> [59] "MORTALIDAD POR EDA 0-4 AÑOS"                                      
#> [60] "MORTALIDAD POR IRA"                                               
#> [61] "MORTALIDAD POR MALARIA"                                           
#> [62] "PARALISIS FLACIDA AGUDA (MENORES DE 15 AÑOS)"                     
#> [63] "PAROTIDITIS"                                                      
#> [64] "RABIA HUMANA"                                                     
#> [65] "RUBEOLA"                                                          
#> [66] "SARAMPION"                                                        
#> [67] "SINDROME DE RUBEOLA CONGENITA"                                    
#> [68] "TETANOS ACCIDENTAL"                                               
#> [69] "TETANOS NEONATAL"                                                 
#> [70] "TOS FERINA"                                                       
#> [71] "TRACOMA"                                                          
#> [72] "TUBERCULOSIS EXTRA PULMONAR"                                      
#> [73] NA                                                                 
#> [74] NA                                                                 
#> [75] NA                                                                 
#> 
#> $year
#>  [1] "2007"                                                        
#>  [2] "2008"                                                        
#>  [3] "2009"                                                        
#>  [4] "2010"                                                        
#>  [5] "2011"                                                        
#>  [6] "2012"                                                        
#>  [7] "2013"                                                        
#>  [8] "2014"                                                        
#>  [9] "2015"                                                        
#> [10] "2017"                                                        
#> [11] "2016"                                                        
#> [12] "2018"                                                        
#> [13] "2019"                                                        
#> [14] "2020"                                                        
#> [15] "2021"                                                        
#> [16] "AGRESIONES POR ANIMALES POTENCIALMENTE TRANSMISORES DE RABIA"
#> [17] "ANOMALIAS CONGENITAS"                                        
#> [18] "BAJO PESO AL NACER"
```

### Contributions

Contributions are welcome via [pull
requests](https://github.com/epiverse-trace/sivirep/pulls).

Contributors to the project include:

-   [Geraldine Gómez-Millán](https://github.com/GeraldineGomez) (author)

-   [Zulma M. Cucunubá](https://github.com/zmcucunuba) (author)

### Code of Conduct

Please note that the linelist project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
