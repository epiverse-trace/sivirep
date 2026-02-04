# Agrupar por fecha de inicio de síntomas y casos

Función que agrupa los datos de una enfermedad o evento por fecha de
inicio de síntomas y número de casos.

## Usage

``` r
agrupar_fecha_inisintomas(data_event, col_fecha = "ini_sin")
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- col_fecha:

  Un \`character\` (cadena de caracteres) con el nombre de la columna de
  los datos de la enfermedad o evento que contiene las fechas de inicio
  de síntomas; su valor por defecto es \`"ini_sin"\`.

## Value

Un \`data.frame\` con los datos de la enfermedad o evento agrupados por
fecha de inicio de síntomas y número de casos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_fecha_inisintomas(
  data_event = data_limpia,
  col_fecha = "ini_sin"
)
#> # A tibble: 21 × 6
#>    ini_sin    semana cod_eve nombre_evento ano   casos
#>    <date>     <chr>  <chr>   <chr>         <chr> <int>
#>  1 2019-12-31 01     210     DENGUE        2020      1
#>  2 2020-01-04 01     210     DENGUE        2020      1
#>  3 2020-01-11 02     210     DENGUE        2020      1
#>  4 2020-01-12 03     210     DENGUE        2020      7
#>  5 2020-01-13 03     210     DENGUE        2020      2
#>  6 2020-01-14 03     210     DENGUE        2020      4
#>  7 2020-01-15 03     210     DENGUE        2020      6
#>  8 2020-01-16 03     210     DENGUE        2020      6
#>  9 2020-01-17 03     210     DENGUE        2020      3
#> 10 2020-01-18 03     210     DENGUE        2020      5
#> # ℹ 11 more rows
```
