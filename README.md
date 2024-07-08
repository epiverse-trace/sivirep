
<!-- README.md is generated from README.Rmd. Please edit that file -->

## *sivirep*: Generación automatizada de reportes a partir de bases de datos de vigilancia epidemiológica <img src="man/figures/logo.svg" align="right" width="120"/>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/sivirep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/sivirep/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/readepi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/readepi?branch=main)
[![lifecycle-maturing](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-maturing.svg)](https://www.reconverse.org/lifecycle.html#maturing)

<!-- badges: end -->

***sivirep*** es desarrollado por la [Pontificia Universidad
Javeriana](https://www.javeriana.edu.co/inicio) como parte de la
iniciativa [Epiverse](https://data.org/initiatives/epiverse/).

La versión actual de *sivirep* 0.0.9 proporciona funciones para la
manipulación de datos y la generación de reportes automatizados basados
en las bases de datos individualizadas de casos de
[SIVIGILA](https://www.ins.gov.co/Direcciones/Vigilancia/Paginas/SIVIGILA.aspx),
que es el sistema oficial de vigilancia epidemiológica de Colombia.

## Motivación

América Latina ha progresado en la calidad de sus sistemas de
notificación y vigilancia epidemiológica. En particular, Colombia ha
mejorado a lo largo de los años la calidad, la accesibilidad y la
transparencia de su sistema oficial de vigilancia epidemiológica,
[SIVIGILA](https://www.ins.gov.co/Direcciones/Vigilancia/Paginas/SIVIGILA.aspx).
Este sistema está regulado por el [Instituto Nacional de
Salud](https://www.ins.gov.co) de Colombia y es operado por miles de
trabajadores de la salud en las secretarías de salud locales, hospitales
y unidades primarias generadoras de datos.

Sin embargo, todavía existen desafíos, especialmente a nivel local, en
cuanto a la oportunidad y la calidad del análisis epidemiológico y de
los informes epidemiológicos. Estas tareas pueden requerir una gran
cantidad de trabajo manual debido a limitaciones en el entrenamiento
para el análisis de datos, el tiempo que se requiere invertir, la
tecnología y la calidad del acceso a internet en algunas regiones de
Colombia.

El objetivo de `sivirep` es proporcionar un conjunto de herramientas
para:

1)  Descargar, preprocesar y preparar los datos de SIVIGILA para su
    posterior análisis.
2)  Generar informes epidemiológicos automatizados adaptables al
    contexto.
3)  Proporcionar retroalimentación sobre el sistema de vigilancia al
    proveedor de la fuente de datos.

## Potenciales usuarios

- Profesionales de salud pública y de epidemiología de campo que
  utilizan la fuente de datos de SIVIGILA a nivel local.
- Estudiantes del área de la salud y epidemiología.
- Investigadores y analistas de datos a nivel nacional e internacional.

## Versiones futuras

Las versiones futuras de `sivirep` podrían incluir:

- Interacción con otras fuentes de datos en Colombia.
- Otros sistemas de vigilancia epidemiológica en América Latina.

## Contribuciones

Las contribuciones son bienvenidas via [pull
requests](https://github.com/epiverse-trace/sivirep/pulls).

Los contribuyentes al paquete incluyen:

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

## Código de conducta

Por favor, ten en cuenta que el proyecto `sivirep` se publica con un
[Código de Conducta para
Contribuyentes](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
Al contribuir a este proyecto, aceptas cumplir con sus términos.

## Instalación

Puedes instalar la versión de desarrollo de `sivirep` desde GitHub con
el siguiente comando:

``` r
install.packages("pak")
pak::pak("epiverse-trace/sivirep")
```

## Inicio rápido

Puedes iniciar importando el paquete después de finalizada su
instalación con el siguiente comando:

``` r
library(sivirep)
```

Puedes revisar las enfermedades y los años disponibles para su descarga
de forma libre utilizando:

``` r
lista_eventos <- list_events()
knitr::kable(lista_eventos)
```

|     | enfermedad                                                            | aa                                                                                                   |
|:----|:----------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------|
| 1   | Accidente Ofídico                                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 2   | Agresiones Por Animales Potencialmente Transmisores De Rabia          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 3   | Anomalías Congénitas                                                  | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                         |
| 4   | Bajo Peso Al Nacer                                                    | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                     |
| 5   | Cáncer De La Mama Y Cuello Uterino                                    | 2016, 2017, 2018, 2019, 2020, 2021                                                                   |
| 6   | Cáncer Infantil                                                       | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                                 |
| 7   | Chagas                                                                | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                     |
| 8   | Chikunguya                                                            | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                                 |
| 9   | Dengue                                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 10  | Dengue Grave                                                          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 11  | Esi - Irag (Vigilancia Centinela)                                     | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022             |
| 12  | Eta Colectivo                                                         | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                         |
| 13  | Evento Adverso Grave Posterior A La Vacunación                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 14  | Exposición A Flúor                                                    | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019                                                       |
| 15  | Fiebre Amarilla                                                       | 2007, 2008, 2009, 2013, 2016, 2018                                                                   |
| 16  | Fiebre Tifoidea Y Paratifoidea                                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 17  | Hepatitis A                                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 18  | Hepatitis B, C Y Coinfección Hepatitis B Y Delta                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 19  | Hepatitis C                                                           | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                                 |
| 20  | Hipotiroidismo Congénito                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 21  | Infección Respiratoria Aguda Grave Irag Inusitada                     | 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                   |
| 69  | Intento De Suicidio                                                   |                                                                                                      |
| 22  | Intoxicación Por Gases                                                | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                         |
| 23  | Intoxicación Por Medicamentos                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 24  | Intoxicación Por Metales Pesados                                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 25  | Intoxicación Por Metanol                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 26  | Intoxicación Por Otras Sustancias Químicas                            | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                     |
| 27  | Intoxicación Por Plaguicidas                                          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 28  | Intoxicación Por Solventes                                            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 29  | Intoxicación Por Sustancias Psicoactivas                              | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                         |
| 30  | Leishmaniasis Cutánea                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 31  | Leishmaniasis Mucosa                                                  | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 32  | Leishmaniasis Visceral                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 33  | Lepra                                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 34  | Lesiones Por Artefactos Explosivos (Pólvora Y Minas Antipersonal)     | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                                 |
| 35  | Lesiones Por Pólvora Y Explosivos                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014                                                       |
| 36  | Leucemia Aguda Pediátrica Linfoide                                    | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022             |
| 37  | Leucemia Aguda Pediátrica Mieloide                                    | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022             |
| 68  | Malaria                                                               |                                                                                                      |
| 38  | Malaria Asociada (Formas Mixtas)                                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 39  | Malaria Complicada                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 40  | Malaria Falciparum                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 41  | Malaria Malarie                                                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014                                                       |
| 42  | Malaria Vivax                                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 43  | Meningitis Meningocócica                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 44  | Meningitis Por Haemophilus Influenzae                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 45  | Meningitis Por Neumococo                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 46  | Meningitis Tuberculosa                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 47  | Morbilidad Por Eda                                                    | 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                   |
| 48  | Morbilidad Por Ira                                                    | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                     |
| 49  | Mortalidad Materna                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 50  | Mortalidad Perinatal Y Neonatal Tardía                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 51  | Mortalidad Por Dengue                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 52  | Mortalidad Por Desnutrición                                           | 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                           |
| 53  | Mortalidad Por Eda 0-4 Años                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 54  | Mortalidad Por Ira                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 55  | Mortalidad Por Malaria                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 |
| 56  | Parotiditis                                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 57  | Rubeola                                                               | 2007, 2008, 2009, 2011, 2012                                                                         |
| 58  | Sarampión                                                             | 2011, 2012, 2013, 2015, 2018, 2019, 2020                                                             |
| 59  | Tétanos Accidental                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 60  | Tétanos Neonatal                                                      | 2007, 2008, 2009, 2010, 2012, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                   |
| 61  | Tos Ferina                                                            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 62  | Tracoma                                                               | 2017, 2018, 2019, 2022                                                                               |
| 63  | Tuberculosis Extra Pulmonar                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 64  | Tuberculosis Farmacorresistente                                       | 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                               |
| 65  | Tuberculosis Pulmonar                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 66  | Varicela Individual                                                   | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022       |
| 67  | Vigilancia En Salud Pública De La Violencia De Género E Intrafamiliar | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                     |
| 70  | Zika                                                                  |                                                                                                      |

## Reporte automatizado

Actualmente, `sivirep` provee una plantilla de reporte llamada
`Reporte Básico {sivirep}`, la cual recibe los siguientes parámetros de
entrada: el nombre de la enfermedad, el año, el nombre de departamento
(opcional) y nombre del municipio (opcional) para descargar los datos de
la fuente de SIVIGILA.

Para hacer uso de la plantilla del reporte puedes seguir los siguientes
pasos:

<video controls autoplay width="100%" loop>
  <source src="https://github.com/epiverse-trace/sivirep/assets/10783929/acbb0111-4044-4698-8e0c-f8e902ddaba3" type="video/webm" label="fullHD">
</video>

El reporte que obtendrás al utilizar la plantilla de `sivirep` es este:

<video controls autoplay width="100%" loop>
  <source src="https://github.com/epiverse-trace/sivirep/assets/10783929/265f9bd5-8da5-45b7-b216-3d7648328037" type="video/webm" label="fullHD">
</video>

Si deseas generar el reporte en formato PDF debes instalar LateX. Puedes
instalarlo siguiendo las instrucciones que se encuentran en [R Markdown
Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/install-latex.html).
