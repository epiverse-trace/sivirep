# Agrupar por columnas y casos

Función que agrupa los datos de una enfermedad o evento por nombre de
columna(s) y número de casos.

## Usage

``` r
agrupar_cols_casos(data_event, nomb_cols, porcentaje = FALSE, estandar = TRUE)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de una enfermedad o evento.

- nomb_cols:

  Un \`character\` (cadena de caracteres) o \`array (arreglo) de
  character\` que contiene el nombre de la(s) columna(s) en los datos de
  la enfermedad o evento.

- porcentaje:

  Un \`logical\` (TRUE o FALSE) que indica si se debe agregar una
  columna con el porcentaje de casos; su valor por defecto es \`FALSE\`.

- estandar:

  Un \`logical\` (TRUE o FALSE) que indica si se debe utilizar el
  estándar de agrupación de los datos del evento o enfermedad propuesto
  por el paquete, es decir, que se incluyan estas columnas o variables
  como parte del resultado \`c("cod_eve", "nombre_evento", "ano")\`; su
  valor por defecto es \`TRUE\`, si su valor es \`FALSE\` agrupará los
  datos solamente por las columnas o variables enviadas en el parámetro
  \`nomb_cols\`.

## Value

Un \`data.frame\` con los datos de una enfermedad o evento agrupados por
el nombre de la(s) columna(s) y el número de casos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_cols_casos(
  data_event = data_limpia,
  nomb_cols = "sexo",
  porcentaje = TRUE
)
#> # A tibble: 2 × 6
#>   sexo  cod_eve nombre_evento ano   casos porcentaje
#>   <chr> <chr>   <chr>         <chr> <int>      <dbl>
#> 1 F     210     DENGUE        2020     22       46.8
#> 2 M     210     DENGUE        2020     25       53.2
agrupar_cols_casos(
  data_event = data_limpia,
  nomb_cols = c("sexo", "semana")
)
#> # A tibble: 16 × 6
#>    sexo  semana cod_eve nombre_evento ano   casos
#>    <chr> <chr>  <chr>   <chr>         <chr> <int>
#>  1 F     01     210     DENGUE        2020      1
#>  2 F     03     210     DENGUE        2020     15
#>  3 F     04     210     DENGUE        2020      1
#>  4 F     05     210     DENGUE        2020      1
#>  5 F     08     210     DENGUE        2020      1
#>  6 F     11     210     DENGUE        2020      1
#>  7 F     14     210     DENGUE        2020      1
#>  8 F     15     210     DENGUE        2020      1
#>  9 M     01     210     DENGUE        2020      1
#> 10 M     02     210     DENGUE        2020      1
#> 11 M     03     210     DENGUE        2020     18
#> 12 M     09     210     DENGUE        2020      1
#> 13 M     10     210     DENGUE        2020      1
#> 14 M     12     210     DENGUE        2020      1
#> 15 M     13     210     DENGUE        2020      1
#> 16 M     16     210     DENGUE        2020      1
```
