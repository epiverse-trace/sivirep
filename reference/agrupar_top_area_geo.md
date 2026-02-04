# Agrupar por área geográfica a nivel departamental o municipal

Función que agrupa los datos de una enfermedad o evento por área
geográfica a nivel departamental o municipal.

## Usage

``` r
agrupar_top_area_geo(
  data_event,
  dpto = NULL,
  col_area = "area",
  porcentaje = FALSE,
  top = 10
)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- dpto:

  Un \`character\` (cadena de caracteres) que contiene el nombre del
  departamento; su valor por defecto es \`NULL\`. Si se ingresa un valor
  en este parámetro se procederá agrupar los datos por los municipios
  del departamento y sus áreas geográficas. Si no se ingresa un valor en
  este parámetro validará si los datos ya están filtrados por algún
  departamento; si no lo están generará la agrupación por departamento.

- col_area:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene las áreas geográficas en los datos de la enfermedad o
  evento; su valor por defecto es \`"cod_mun_o"\`.

- porcentaje:

  Un \`logical\` (TRUE o FALSE) que indica si se debe agregar una
  columna con el porcentaje de casos; su valor por defecto es \`FALSE\`.

- top:

  Un \`numeric\` (numérico) que indica la cantidad de departamentos o
  municipios con mayor número de casos que se deben retornar; su valor
  por defecto es \`10\`.

## Value

Un \`data.frame\` con el top 10 de los datos de la enfermedad o evento
agrupados por áreas geográficas y número de casos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_top_area_geo(
  data_event = data_limpia,
  dpto = "Antioquia",
  col_area = "area",
  porcentaje = FALSE,
  top = 10
)
#> # A tibble: 10 × 9
#>    area  cod_dpto_o departamento_ocurrencia cod_mun_o municipio_ocurrencia
#>    <chr> <chr>      <chr>                   <chr>     <chr>               
#>  1 1     05         ANTIOQUIA               05001     MEDELLIN            
#>  2 1     05         ANTIOQUIA               05045     APARTADO            
#>  3 1     05         ANTIOQUIA               05809     TITIRIBI            
#>  4 1     05         ANTIOQUIA               05837     TURBO               
#>  5 2     05         ANTIOQUIA               05001     MEDELLIN            
#>  6 2     05         ANTIOQUIA               05129     CALDAS              
#>  7 2     05         ANTIOQUIA               05591     PUERTO TRIUNFO      
#>  8 2     05         ANTIOQUIA               05659     SAN JUAN DE URABA   
#>  9 3     05         ANTIOQUIA               05490     NECOCLI             
#> 10 3     05         ANTIOQUIA               05736     SEGOVIA             
#> # ℹ 4 more variables: cod_eve <chr>, nombre_evento <chr>, ano <chr>,
#> #   casos <int>
```
