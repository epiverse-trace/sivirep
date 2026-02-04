# Agrupar por departamento y casos

Función que agrupa los datos por códigos de departamento y número de
casos.

## Usage

``` r
agrupar_dpto(data_event, col_dpto = "cod_dpto_o", porcentaje = FALSE)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- col_dpto:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene los códigos de los departamentos en los datos de la
  enfermedad o evento; su valor por defecto es \`"cod_dpto_o"\`.

- porcentaje:

  Un \`logical\` (TRUE o FALSE) que indica si se debe agregar una
  columna con el porcentaje de casos; su valor por defecto es \`FALSE\`.

## Value

Un \`data.frame\` con los datos de la enfermedad o evento agrupados por
códigos de departamento y número de casos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_dpto(
  data_event = data_limpia,
  col_dpto = "cod_dpto_o",
  porcentaje = FALSE
)
#> # A tibble: 31 × 6
#>    cod_dpto_o departamento_ocurrencia cod_eve nombre_evento ano   casos
#>    <chr>      <chr>                   <chr>   <chr>         <chr> <int>
#>  1 01         EXTERIOR                210     DENGUE        2020      1
#>  2 05         ANTIOQUIA               210     DENGUE        2020     12
#>  3 08         ATLANTICO               210     DENGUE        2020      2
#>  4 13         BOLIVAR                 210     DENGUE        2020      1
#>  5 15         BOYACA                  210     DENGUE        2020      1
#>  6 17         CALDAS                  210     DENGUE        2020      1
#>  7 18         CAQUETA                 210     DENGUE        2020      1
#>  8 19         CAUCA                   210     DENGUE        2020      1
#>  9 20         CESAR                   210     DENGUE        2020      1
#> 10 23         CORDOBA                 210     DENGUE        2020      1
#> # ℹ 21 more rows
```
