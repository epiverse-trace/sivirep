
<!-- README.md is generated from README.Rmd. Please edit that file -->

## *sivirep*: Automated reporting from epidemiological surveillance databases <img src="man/figures/logo.png" align="right" width="120" />

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
knitr::kable(list_of_diseases)
```

| ENFERMEDAD                                                   | AA                                                                                             |
|:-------------------------------------------------------------|:-----------------------------------------------------------------------------------------------|
| ACCIDENTE OFIDICO                                            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| AGRESIONES POR ANIMALES POTENCIALMENTE TRANSMISORES DE RABIA | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| ANOMALIAS CONGENITAS                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| BAJO PESO AL NACER                                           | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021                                     |
| CÁNCER DE LA MAMA Y CUELLO UTERINO                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2019, 2020, 2021 |
| CANCER INFANTIL                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| CHAGAS                                                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| CHIKUNGUNYA                                                  | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| DENGUE                                                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| DENGUE GRAVE                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| DIFTERIA                                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021             |
| ENCEFALITIS DEL NILO OCCIDENTAL EN HUMANOS                   | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017                               |
| ENCEFALITIS EQUINA DEL OESTE EN HUMANOS                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017                               |
| ENCEFALITIS EQUINA VENEZOLANA EN HUMANOS                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017                               |
| ENDOMETRITIS PUERPERAL                                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017                               |
| ENFERMEDADES HUERFANAS - RARAS                               | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017                               |
| ESI - IRAG (VIGILANCIA CENTINELA)                            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| EVENTO ADVERSO SEGUIDO A LA VACUNACION                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| FIEBRE AMARILLA                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018                         |
| FIEBRE TIFOIDEA Y PARATIFOIDEA                               | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| HEPATITIS A                                                  | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| HEPATITIS C                                                  | 2018, 2019, 2020, 2021                                                                         |
| HIPOTIROIDISMO CONGENITO                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| INFECCION ASOCIADA A DISPOSITIVOS                            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017                               |
| INFECCION RESPIRATORIA AGUDA GRAVE IRAG INUSITADA            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| INTOXICACION POR FARMACOS                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| INTOXICACION POR METALES PESADOS                             | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| INTOXICACION POR METANOL                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| INTOXICACION POR MONOXIDO DE CARBONO Y OTROS GASES           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| INTOXICACION POR OTRAS SUSTANCIAS QUIMICAS                   | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| INTOXICACION POR PLAGUICIDAS                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| INTOXICACION POR SOLVENTES                                   | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| INTOXICACION POR SUSTANCIAS PSICOACTIVAS                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| ISO                                                          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017                               |
| LEISHMANIASIS CUTANEA                                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| LEISHMANIASIS MUCOSA                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| LEISHMANIASIS VISCERAL                                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| LEPRA                                                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| LEPTOSPIROSIS                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| LEUCEMIA AGUDA PEDIATRICA LINFOIDE                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| LEUCEMIA AGUDA PEDIATRICA MIELOIDE                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MALARIA ASOCIADA (FORMAS MIXTAS)                             | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MALARIA COMPLICADA                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MALARIA FALCIPARUM                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MALARIA VIVAX                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MENINGITIS OTROS                                             | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017                               |
| MENINGITIS MENINGOCÓCICA                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2019, 2020, 2021 |
| MENINGITIS POR HAEMOPHILUS INFLUENZAE                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2019, 2020, 2021 |
| MENINGITIS POR NEUMOCOCO                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2019, 2020, 2021 |
| MENINGITIS TUBERCULOSA                                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MORBILIDAD MATERNA EXTREMA                                   | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MORTALIDAD MATERNA                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MORTALIDAD PERINATAL Y NEONATAL TARDIA                       | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021             |
| MORTALIDAD POR DENGUE                                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MORTALIDAD POR EDA 0-4 AÑOS                                  | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2019, 2020, 2021 |
| MORTALIDAD POR IRA                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| MORTALIDAD POR MALARIA                                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| PARALISIS FLACIDA AGUDA (MENORES DE 15 AÑOS)                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020             |
| PAROTIDITIS                                                  | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| RABIA HUMANA                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2020                         |
| SARAMPION                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020             |
| SINDROME DE RUBEOLA CONGENITA                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017                               |
| TETANOS ACCIDENTAL                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| TETANOS NEONATAL                                             | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| TOS FERINA                                                   | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021       |
| TRACOMA                                                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2019             |
| TUBERCULOSIS EXTRA PULMONAR                                  | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015                                           |

### Contributions

Contributions are welcome via [pull
requests](https://github.com/epiverse-trace/sivirep/pulls).

Contributors to the project include:

-   [Geraldine Gómez-Millán](https://github.com/GeraldineGomez) (author)

-   [Zulma M. Cucunubá](https://github.com/zmcucunuba) (author)

-   [Hugo Gruson](https://github.com/Bisaloo) (contributor)

### Code of Conduct

Please note that the linelist project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
