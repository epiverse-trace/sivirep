# sivirep

## Descripción

La versión actual de *sivirep* proporciona funciones para la
manipulación de datos y la generación de reportes automatizados basados
en las bases de datos individualizadas de casos de SIVIGILA, el sistema
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
SIVIGILA. Este sistema está regulado por el Instituto Nacional de Salud
de Colombia y es operado por miles de trabajadores de la salud en las
secretarías de salud locales, hospitales y unidades primarias
generadoras de datos.

Sin embargo, todavía existen desafíos, especialmente a nivel local, en
cuanto a la oportunidad y la calidad del análisis epidemiológico y de
los informes epidemiológicos. Estas tareas pueden requerir una gran
cantidad de trabajo manual debido a limitaciones en el entrenamiento
para el análisis de datos, el tiempo que se requiere invertir, la
tecnología y la calidad del acceso a internet en algunas regiones de
Colombia.

El objetivo de `sivirep` es proporcionar un conjunto de herramientas
para:

1.  Descargar, preprocesar y preparar los datos de SIVIGILA para su
    posterior análisis.
2.  Generar reportes epidemiológicos automatizados adaptables al
    contexto.
3.  Proporcionar retroalimentación sobre el sistema de vigilancia al
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
los siguientes comandos:

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
de forma libre utilizando el comando:

``` r
lista_eventos <- list_events()
knitr::kable(lista_eventos)
```

> 🦠**Listado de enfermedades (haz clic para ver)**
>
>   
>
> | Codigo | Enfermedad                                                            | Año                                                                                                              |
> |:-------|:----------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------|
> | 100    | Accidente ofídico                                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 110    | Bajo Peso Al Nacer                                                    | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022                                                 |
> | 112    | Mortalidad Por Desnutrición                                           | 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                           |
> | 113    | Desnutrición Aguda En Menores De 5 Años                               | 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                             |
> | 155    | Cáncer De La Mama Y Cuello Uterino                                    | 2016, 2017, 2018, 2019, 2020, 2021, 2023, 2024                                                                   |
> | 205    | Chagas                                                                | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                     |
> | 210    | Dengue                                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 217    | Chikunguya                                                            | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                 |
> | 220    | Dengue Grave                                                          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 228    | Exposición A Flúor                                                    | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019                                                                   |
> | 230    | Difteria                                                              | 2018, 2019, 2021                                                                                                 |
> | 290    | Encefalitis Equina Venezolana En Humanos                              | 2024                                                                                                             |
> | 298    | Evento Adverso Grave Posterior A La Vacunación                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 300    | Agresiones Por Animales Potencialmente Transmisores De Rabia          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 305    | Tracoma                                                               | 2017, 2018, 2019, 2022, 2024                                                                                     |
> | 310    | Fiebre Amarilla                                                       | 2007, 2008, 2009, 2013, 2016, 2018, 2023, 2024                                                                   |
> | 320    | Fiebre Tifoidea Y Paratifoidea                                        | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 330    | Hepatitis A                                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 340    | Hepatitis B, C Y Coinfección Hepatitis B Y Delta                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 341    | Hepatitis C                                                           | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                 |
> | 343    | Hipotiroidismo congénito                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 345    | ESI - Irag (Vigilancia Centinela)                                     | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024             |
> | 346    | IRA por Virus Nuevo                                                   | 2024                                                                                                             |
> | 348    | Infección Respiratoria Aguda Grave Irag Inusitada                     | 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                   |
> | 349    | ETA Colectivo                                                         | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023                               |
> | 356    | Intento De Suicidio                                                   | 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                             |
> | 360    | Intoxicación Por Plaguicidas                                          | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 370    | Intoxicación Por Medicamentos                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 380    | Intoxicación Por Metanol                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 390    | Intoxicación Por Metales Pesados                                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 400    | Intoxicación Por Solventes                                            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 410    | Intoxicación Por Otras Sustancias químicas                            | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                     |
> | 412    | Intoxicación Por Gases                                                | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                         |
> | 414    | Intoxicación Por Sustancias Psicoactivas                              | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                         |
> | 420    | Leishmaniasis cutánea                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 430    | Leishmaniasis Mucosa                                                  | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 440    | Leishmaniasis Visceral                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 450    | Lepra                                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 452    | Lesiones Por Artefactos Explosivos (pólvora Y Minas Antipersonal)     | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                 |
> | 455    | Leptospirosis                                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 456    | Leucemia Aguda Pediátrica Linfoide                                    | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024             |
> | 457    | Leucemia Aguda Pediátrica Mieloide                                    | 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024             |
> | 458    | Lesiones Por pólvora Y Explosivos                                     | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014                                                                   |
> | 459    | Cáncer Infantil                                                       | 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                 |
> | 460    | Malaria Asociada (Formas Mixtas)                                      | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 470    | Malaria Falciparum                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 480    | Malaria Malarie                                                       | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014                                                                   |
> | 490    | Malaria Vivax                                                         | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 495    | Malaria Complicada                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 500    | Meningitis Meningocócica                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 510    | Meningitis Por Haemophilus Influenzae                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 520    | Meningitis Por Neumococo                                              | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 530    | Meningitis Tuberculosa                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 540    | Mortalidad por Malaria                                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024 |
> | 549    | Morbilidad Materna Extrema                                            | 2012, 2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                           |
> | 550    | Mortalidad Materna                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 560    | Mortalidad Perinatal Y Neonatal Tardía                                | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 580    | Mortalidad Por Dengue                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 590    | Mortalidad Por Eda 0-4 Años                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 600    | Mortalidad Por Ira                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 610    | Parálisis flácida Aguda (Menores De 15 Años)                          | 2009, 2013, 2014, 2018                                                                                           |
> | 620    | Parotiditis                                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 670    | Rabia Humana                                                          | 2007, 2008, 2009, 2010, 2012, 2015, 2016, 2017, 2020, 2021                                                       |
> | 710    | Rubeola                                                               | 2007, 2008, 2009, 2011, 2012                                                                                     |
> | 730    | Sarampión                                                             | 2011, 2012, 2013, 2015, 2018, 2019, 2020                                                                         |
> | 735    | Anomalías congénitas                                                  | 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                         |
> | 739    | Síndrome Inflamatorio Multisistémico En Niños Asociado A SARS-COV2    | 2022, 2023, 2024                                                                                                 |
> | 760    | Tétanos Accidental                                                    | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 770    | Tétanos Neonatal                                                      | 2007, 2008, 2009, 2010, 2012, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2024                         |
> | 780    | Tifus Epidémico Transmitido Por Piojos                                | 2014                                                                                                             |
> | 790    | Tifus Endémico Trasmitido Por Pulgas                                  | 2013, 2014                                                                                                       |
> | 800    | Tos Ferina                                                            | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 810    | Tuberculosis Extra Pulmonar                                           | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 820    | Tuberculosis Pulmonar                                                 | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 825    | Tuberculosis Farmacorresistente                                       | 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                               |
> | 831    | Varicela Individual                                                   | 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024       |
> | 875    | Vigilancia En Salud Pública De La Violencia De Género E Intrafamiliar | 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                     |
> | 895    | Zika                                                                  | 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                                                       |
> | 995    | Morbilidad por IRA                                                    | 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                               |
> | 998    | Morbilidad por EDA                                                    | 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024                   |

## Reporte automatizado

Actualmente, `sivirep` proporciona una plantilla de reporte llamada
`Reporte Evento {sivirep}`, la cual recibe los siguientes parámetros de
entrada:

- Nombre de la enfermedad  
- Año  
- País  
- Nombre del departamento *(opcional)*  
- Nombre del municipio *(opcional)*
- Datos en caché *(casilla de chequeo - opcional)*: elige si los datos
  descargados para el evento y la incidencia deben almacenarse en la
  caché de la sesión del usuario para evitar volver a descargarlos al
  generar el informe nuevamente con las mismas entradas.

Para hacer uso de la plantilla del reporte puedes seguir los siguientes
pasos:

> 🎥 [¿Cómo generar un reporte con
> sivirep?](https://youtu.be/wsgXQKEeg8I)

El reporte que obtendrás al utilizar la plantilla de `sivirep` es este:

> 🎥 [Reporte sivirep](https://youtu.be/NRUNwVrs4io)

Si deseas generar el reporte en formato PDF debes instalar LateX. Puedes
instalarlo siguiendo las instrucciones que se encuentran en [R Markdown
Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/install-latex.html).

## Análisis personalizados

`sivirep` ofrece un conjunto de funciones que pueden utilizarse en
distintos escenarios: desde la descarga de datos hasta la creación de
análisis personalizados o la construcción de flujos de trabajo
completos.

Para explorar las funciones disponibles y ejemplos de su uso, visita la
página de [Análisis
personalizados](https://epiverse-trace.github.io/sivirep/articles/custom_analysis.html).
