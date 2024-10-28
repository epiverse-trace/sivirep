
<!-- README.md is generated from README.Rmd. Please edit that file -->

## *sivirep*: Generación automatizada de reportes a partir de bases de datos de vigilancia epidemiológica <img src="man/figures/logo.svg" align="right" width="120"/>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/license/mit)
[![R-CMD-check](https://github.com/epiverse-trace/sivirep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/sivirep/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/sivirep/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/sivirep?branch=main)
[![lifecycle-maturing](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-maturing.svg)](https://www.reconverse.org/lifecycle.html#maturing)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

***sivirep*** es desarrollado por la [Pontificia Universidad
Javeriana](https://www.javeriana.edu.co/inicio) como parte de la
iniciativa [Epiverse](https://data.org/initiatives/epiverse/).

La versión actual de *sivirep* 1.0.0 proporciona funciones para la
manipulación de datos y la generación de reportes automatizados basados
en las bases de datos individualizadas de casos de
[SIVIGILA](https://portalsivigila.ins.gov.co/), que es el sistema
oficial de vigilancia epidemiológica de Colombia.

## Exclusión de responsabilidad

El uso de esta librería, así como de los datos, reportes generados y
otros productos derivados de la misma, se realiza bajo la
responsabilidad exclusiva del usuario. Ni los autores de la librería, ni
la Pontificia Universidad Javeriana, ni la fuente de información asumen
responsabilidad alguna por los resultados obtenidos o el uso que se haga
de dichos productos.

## Motivación

América Latina ha progresado en la calidad de sus sistemas de
notificación y vigilancia epidemiológica. En particular, Colombia ha
mejorado a lo largo de los años la calidad, la accesibilidad y la
transparencia de su sistema oficial de vigilancia epidemiológica,
[SIVIGILA](https://portalsivigila.ins.gov.co/). Este sistema está
regulado por el [Instituto Nacional de
Salud](https://www.ins.gov.co/Paginas/Inicio.aspx) de Colombia y es
operado por miles de trabajadores de la salud en las secretarías de
salud locales, hospitales y unidades primarias generadoras de datos.

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
Romero-Garcés](https://github.com/juanitaromerog), [Jaime A.
Pavlich-Mariscal](https://github.com/jpavlich), [Andrés
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

Puedes instalar `sivirep` desde CRAN utilizando:

``` r
install.packages("sivirep")
```

Si deseas instalar la versión de desarrollo de `sivirep` desde GitHub
puedes hacerlos con el siguiente comando:

``` r
install.packages("pak")
pak::pak("epiverse-trace/sivirep")
```

También, puedes utilizar cualquiera de estas dos opciones:

``` r
install.packages("remotes")
remotes::install_github("epiverse-trace/sivirep")
```

``` r
install.packages("sivirep", repos = c("https://epiverse-trace.r-universe.dev", "https://cloud.r-project.org"))
```

## Inicio rápido

Puedes iniciar importando el paquete después de finalizada su
instalación con el siguiente comando:

``` r
library(sivirep)
```

Puedes revisar las enfermedades y los años disponibles para su descarga
de forma libre utilizando los comandos:

``` r
lista_eventos <- list_events()
knitr::kable(lista_eventos)
```

<blockquote style="color: #333; cursor:pointer;">
<details>
<summary>
🦠<b>Listado de enfermedades (haz clic para ver)</b>
</summary>

<br>

| Codigo | Enfermedad | Año |
|:---|:---|:---|
| 100 | Accidente Ofídico | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 300 | Agresiones Por Animales Potencialmente Transmisores De Rabia | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 735 | Anomalías Congénitas | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 110 | Bajo Peso Al Nacer | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 |
| 155 | Cáncer De La Mama Y Cuello Uterino | 2016, 2017, 2018, 2019, 2020, 2021, 2023 |
| 459 | Cáncer Infantil | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 205 | Chagas | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 217 | Chikunguya | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 210 | Dengue | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 220 | Dengue Grave | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 112 | Desnutrición Aguda En Menores De 5 Años | 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 230 | Difteria | 2018, 2019, 2021 |
| 345 | Esi - Irag (Vigilancia Centinela) | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 349 | Eta Colectivo | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 298 | Evento Adverso Grave Posterior A La Vacunación | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 228 | Exposición A Flúor | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 |
| 310 | Fiebre Amarilla | 2007, 2008, 2009 |
| 320 | Fiebre Tifoidea Y Paratifoidea | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 330 | Hepatitis A | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 340 | Hepatitis B, C Y Coinfección Hepatitis B Y Delta | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 341 | Hepatitis C | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 343 | Hipotiroidismo Congénito | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 348 | Infección Respiratoria Aguda Grave Irag Inusitada | 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 356 | Intento De Suicidio | 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 412 | Intoxicación Por Gases | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 370 | Intoxicación Por Medicamentos | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 390 | Intoxicación Por Metales Pesados | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 380 | Intoxicación Por Metanol | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 410 | Intoxicación Por Otras Sustancias Químicas | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 360 | Intoxicación Por Plaguicidas | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 400 | Intoxicación Por Solventes | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 420 | Leishmaniasis Cutánea | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 430 | Leishmaniasis Mucosa | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 440 | Leishmaniasis Visceral | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 450 | Lepra | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 455 | Leptospirosis | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 452 | Lesiones Por Artefactos Explosivos (Pólvora Y Minas Antipersonal) | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 458 | Lesiones Por Pólvora Y Explosivos | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 |
| 456 | Leucemia Aguda Pediátrica Linfoide | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 457 | Leucemia Aguda Pediátrica Mieloide | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 465 | Malaria | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 460 | Malaria Asociada (Formas Mixtas) | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 495 | Malaria Complicada | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 470 | Malaria Falciparum | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 490 | Malaria Vivax | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 500 | Meningitis Meningocócica | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 510 | Meningitis Por Haemophilus Influenzae | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 520 | Meningitis Por Neumococo | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 530 | Meningitis Tuberculosa | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 549 | Morbilidad Materna Extrema | 2012, 2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 998 | Morbilidad Por Eda | 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 995 | Morbilidad Por Ira | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 550 | Mortalidad Materna | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 560 | Mortalidad Perinatal Y Neonatal Tardía | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 580 | Mortalidad Por Dengue | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 590 | Mortalidad Por Eda 0-4 Años | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 600 | Mortalidad Por Ira | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 540 | Mortalidad Por Malaria | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 620 | Parotiditis | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 670 | Rabia Humana | 2007, 2008, 2009, 2010, 2012, 2015, 2016, 2017, 2020 |
| 710 | Rubeola | 2007, 2008, 2009, 2010, 2011, 2012 |
| 730 | Sarampión | 2011, 2012, 2013, 2015, 2018, 2019, 2020 |
| 739 | Síndrome Inflamatorio Multisistémico En Niños Asociado A Sars-Cov2 | 2022, 2023 |
| 760 | Tétanos Accidental | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 770 | Tétanos Neonatal | 2007, 2008, 2009, 2010, 2012, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 |
| 790 | Tifus Endémico Trasmitido Por Pulgas | 2013, 2014 |
| 780 | Tifus Epidémico Transmitido Por Piojos | 2014 |
| 800 | Tos Ferina | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 305 | Tracoma | 2017, 2018, 2019, 2022 |
| 810 | Tuberculosis Extra Pulmonar | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 825 | Tuberculosis Farmacorresistente | 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 820 | Tuberculosis Pulmonar | 2010, 2011, 2016, 2017, 2018, 2019, 2020, 2021, 2022 |
| 831 | Varicela Individual | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 875 | Vigilancia En Salud Pública De La Violencia De Género E Intrafamiliar | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |
| 895 | Zika | 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 |

</details>
</blockquote>

## Reporte automatizado

Actualmente, `sivirep` provee una plantilla de reporte llamada
`Reporte Básico {sivirep}`, la cual recibe los siguientes parámetros de
entrada: el nombre de la enfermedad, el año, el nombre del país, el
nombre del departamento (opcional) y el nombre del municipio (opcional)
para descargar los datos de la fuente de SIVIGILA.

Para hacer uso de la plantilla del reporte puedes seguir los siguientes
pasos:

> 🎥 [¿Cómo generar un reporte con
> sivirep?](https://youtu.be/pqzRw5YhP_g)

El reporte que obtendrás al utilizar la plantilla de `sivirep` es este:

> 🎥 [Reporte sivirep](https://youtu.be/1t_Di3fC4hM)

Si deseas generar el reporte en formato PDF debes instalar LateX. Puedes
instalarlo siguiendo las instrucciones que se encuentran en [R Markdown
Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/install-latex.html).
