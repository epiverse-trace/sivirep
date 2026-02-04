# Agrupar por área geográfica

Función que agrupa los datos de una enfermedad o evento por área
geográfica.

## Usage

``` r
agrupar_area_geo(data_event, col_area = "area", porcentaje = FALSE)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- col_area:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene las áreas geográficas en los datos de la enfermedad o
  evento; su valor por defecto es \`"area"\`.

- porcentaje:

  Un \`logical\` (TRUE o FALSE) que indica si se debe agregar una
  columna con el porcentaje de casos; su valor por defecto es \`FALSE\`.

## Value

Un \`data.frame\` con los datos de la enfermedad o evento agrupados por
área geográfica.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_area_geo(
  data_event = data_limpia,
  col_area = "area",
  porcentaje = FALSE
)
#> # A tibble: 3 × 5
#>   area  cod_eve nombre_evento ano   casos
#>   <chr> <chr>   <chr>         <chr> <int>
#> 1 1     210     DENGUE        2020     37
#> 2 2     210     DENGUE        2020      5
#> 3 3     210     DENGUE        2020      5
```
