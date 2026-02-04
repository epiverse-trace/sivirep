# Get started

``` r
library(sivirep)
```

## Description

Current version of *sivirep* provides functions for data wrangling and
automated reports based on data from SIVIGILA, which is the official
epidemiological surveillance system of Colombia, South America.

## Disclaimer

The use of this package, as well as the data, generated reports, and
other products derived from it, is the sole responsibility of the user.
Neither the authors of the package, nor the Pontificia Universidad
Javeriana, nor the source of the information assume any responsibility
for the results obtained or the use made of these products.

## Motivation

Latin America has progressed in the quality of epidemiological
notification and surveillance systems. Particularly, Colombia has
improved over the years the quality and openness of its official
epidemiological surveillance system, SIVIGILA. This system is regulated
by Colombia’s National Institute of Health and operated by thousands of
health workers at local secretaries of health, hospitals, and local
notification units.

However, some challenges remain particularly at local levels in terms of
timeliness and quality of epidemiological analytics and epidemiological
reports. These tasks may involve a great effor of manual labor
reinforced by limitations in training for data analytics, time,
technology and quality of internet access in some locations.

`sivirep` is aimed at providing a set of customisable functions for:

1.  Downloading, pre-processing and preparing SIVIGILA data for further
    analysis.
2.  Producing customisable epidemiological automated reports.
3.  Getting feedback on the surveillance system to the source provider.

## Potential users

- Public health professionals and field epidemiologist users of SIVIGILA
  source at local levels
- Epidemiology and public health students
- National and international researchers and data analysts

## Future versions

Future versions of `sivirep` may include:

- Interaction with other data sources in Colombia
- Other epidemiological surveillance systems in Latin America

## Contributions

Contributions are welcome via [pull
requests](https://github.com/epiverse-trace/sivirep/pulls).

Contributors to the project include:

**Autores**: [Geraldine
Gómez-Millán](https://github.com/GeraldineGomez), [Zulma M.
Cucunubá](https://github.com/zmcucunuba), Jennifer A. Mendez-Romero y
[Claudia Huguett-Aragón](https://github.com/chuguett)

**Contribuyentes**: [Hugo Gruson](https://github.com/Bisaloo), [Juanita
Romero-Garcés](https://github.com/juanitaromerog), [Andrés
Moreno](https://github.com/andresmore), [Miguel
Gámez](https://github.com/megamezl), [Laura
Gómez-Bermeo](https://github.com/lgbermeo), Johan Calderón, Lady
Flórez-Tapiero, Verónica Tangarife-Arredondo y Gerard Alarcon

## Code of conduct

Please note that the linelist project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Installation

You can install the development version of `sivirep` from GitHub with:

``` r
install.packages("pak")
pak::pak("epiverse-trace/sivirep")
```

Alternatively, you can also use:

``` r
install.packages("remotes")
remotes::install_github("epiverse-trace/sivirep")
```

``` r
install.packages("sivirep",
                 repos = c("https://epiverse-trace.r-universe.dev",
                           "https://cloud.r-project.org"))
```

## Quick start

You can start by importing the package after installation is complete
using the following command:

``` r
library(sivirep)
```

You can check the available diseases and years for free download using
the commands:

``` r
lista_eventos <- list_events()
knitr::kable(lista_eventos)
```

> 🦠**Listado de enfermedades (haz clic para ver)**
>
>   
>
> | Code | Disease                                                               | Year                                                                                                             |
> |:-----|:----------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------|
> | 100  | Accidente ofídico                                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 110  | Bajo Peso Al Nacer                                                    | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                                 |
> | 112  | Mortalidad Por Desnutrición                                           | 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                           |
> | 113  | Desnutrición Aguda En Menores De 5 Años                               | 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                             |
> | 155  | Cáncer De La Mama Y Cuello Uterino                                    | 2016, 2017, 2018, 2019, 2020, 2021, 2023, 2024                                                                   |
> | 205  | Chagas                                                                | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                     |
> | 210  | Dengue                                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 217  | Chikunguya                                                            | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                 |
> | 220  | Dengue Grave                                                          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 228  | Exposición A Flúor                                                    | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019                                                                   |
> | 230  | Difteria                                                              | 2018, 2019, 2021                                                                                                 |
> | 290  | Encefalitis Equina Venezolana En Humanos                              | 2024                                                                                                             |
> | 298  | Evento Adverso Grave Posterior A La Vacunación                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 300  | Agresiones Por Animales Potencialmente Transmisores De Rabia          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 305  | Tracoma                                                               | 2017, 2018, 2019, 2022, 2024                                                                                     |
> | 310  | Fiebre Amarilla                                                       | 2007, 2008, 2009, 2013, 2016, 2018, 2023, 2024                                                                   |
> | 320  | Fiebre Tifoidea Y Paratifoidea                                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 330  | Hepatitis A                                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 340  | Hepatitis B, C Y Coinfección Hepatitis B Y Delta                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 341  | Hepatitis C                                                           | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                 |
> | 343  | Hipotiroidismo congénito                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 345  | ESI - Irag (Vigilancia Centinela)                                     | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024             |
> | 346  | IRA por Virus Nuevo                                                   | 2024                                                                                                             |
> | 348  | Infección Respiratoria Aguda Grave Irag Inusitada                     | 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                   |
> | 349  | ETA Colectivo                                                         | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023                               |
> | 356  | Intento De Suicidio                                                   | 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                             |
> | 360  | Intoxicación Por Plaguicidas                                          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 370  | Intoxicación Por Medicamentos                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 380  | Intoxicación Por Metanol                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 390  | Intoxicación Por Metales Pesados                                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 400  | Intoxicación Por Solventes                                            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 410  | Intoxicación Por Otras Sustancias químicas                            | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                     |
> | 412  | Intoxicación Por Gases                                                | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                         |
> | 414  | Intoxicación Por Sustancias Psicoactivas                              | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                         |
> | 420  | Leishmaniasis cutánea                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 430  | Leishmaniasis Mucosa                                                  | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 440  | Leishmaniasis Visceral                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 450  | Lepra                                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 452  | Lesiones Por Artefactos Explosivos (pólvora Y Minas Antipersonal)     | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                 |
> | 455  | Leptospirosis                                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 456  | Leucemia Aguda Pediátrica Linfoide                                    | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024             |
> | 457  | Leucemia Aguda Pediátrica Mieloide                                    | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024             |
> | 458  | Lesiones Por pólvora Y Explosivos                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014                                                                   |
> | 459  | Cáncer Infantil                                                       | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                 |
> | 460  | Malaria Asociada (Formas Mixtas)                                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 470  | Malaria Falciparum                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 480  | Malaria Malarie                                                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014                                                                   |
> | 490  | Malaria Vivax                                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 495  | Malaria Complicada                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 500  | Meningitis Meningocócica                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 510  | Meningitis Por Haemophilus Influenzae                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 520  | Meningitis Por Neumococo                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 530  | Meningitis Tuberculosa                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 540  | Mortalidad por Malaria                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024 |
> | 549  | Morbilidad Materna Extrema                                            | 2012, 2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                           |
> | 550  | Mortalidad Materna                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 560  | Mortalidad Perinatal Y Neonatal Tardía                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 580  | Mortalidad Por Dengue                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 590  | Mortalidad Por Eda 0-4 Años                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 600  | Mortalidad Por Ira                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 610  | Parálisis flácida Aguda (Menores De 15 Años)                          | 2009, 2013, 2014, 2018                                                                                           |
> | 620  | Parotiditis                                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 670  | Rabia Humana                                                          | 2007, 2008, 2009, 2010, 2012, 2015, 2016, 2017, 2020, 2021                                                       |
> | 710  | Rubeola                                                               | 2007, 2008, 2009, 2011, 2012                                                                                     |
> | 730  | Sarampión                                                             | 2011, 2012, 2013, 2015, 2018, 2019, 2020                                                                         |
> | 735  | Anomalías congénitas                                                  | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                         |
> | 739  | Síndrome Inflamatorio Multisistémico En Niños Asociado A SARS-COV2    | 2022, 2023, 2024                                                                                                 |
> | 760  | Tétanos Accidental                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 770  | Tétanos Neonatal                                                      | 2007, 2008, 2009, 2010, 2012, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2024                         |
> | 780  | Tifus Epidémico Transmitido Por Piojos                                | 2014                                                                                                             |
> | 790  | Tifus Endémico Trasmitido Por Pulgas                                  | 2013, 2014                                                                                                       |
> | 800  | Tos Ferina                                                            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 810  | Tuberculosis Extra Pulmonar                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 820  | Tuberculosis Pulmonar                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 825  | Tuberculosis Farmacorresistente                                       | 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                               |
> | 831  | Varicela Individual                                                   | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 875  | Vigilancia En Salud Pública De La Violencia De Género E Intrafamiliar | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                     |
> | 895  | Zika                                                                  | 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                       |
> | 995  | Morbilidad por IRA                                                    | 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                               |
> | 998  | Morbilidad por EDA                                                    | 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                   |

## Automated report

Currently, `sivirep` provides a report template called  
`Reporte Evento {sivirep}`, which takes the following input parameters:

- Name of the disease  
- Year  
- Country  
- Name of the department *(optional)*  
- Name of the municipality *(optional)*
- Cache data *(checkbox - optional)* : choose whether the downloaded
  data for the event and incidence should be stored in the user’s
  session cache to avoid re-downloading it when generating the report
  again with the same inputs.

These parameters are used to download data from the SIVIGILA source.

To use the report template, you can follow these steps:

> 🎥 [How to generate a report with
> sivirep?](https://youtu.be/wsgXQKEeg8I)

The report generated using the `sivirep` template looks like this:

> 🎥 [sivirep report](https://youtu.be/NRUNwVrs4io)

If you want to generate the report in PDF format, you need to install
LaTeX.  
You can install it by following the instructions in the [R Markdown
Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/install-latex.html).

## Custom analysis

`sivirep` offers a suite of functions that can be used in different
scenarios —  
from downloading data to creating custom analyses or building full
pipelines.

To explore the available functions and examples of their suage, please
visit the [Custom
analysis](https://epiverse-trace.github.io/sivirep/articles/custom_analysis_en.html)page.
