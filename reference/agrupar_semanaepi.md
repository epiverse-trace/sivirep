# Agrupar por semana epidemiológica y casos

Función que agrupa los datos de una enfermedad o evento por semana
epidemiológica y número de casos.

## Usage

``` r
agrupar_semanaepi(data_event, col_semanaepi = "semana")
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de una enfermedad o evento.

- col_semanaepi:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene las semanas epidemiológicas en los datos de la enfermedad
  o evento; su valor por defecto es \`"semana"\`.

## Value

Un \`data.frame\` con los datos de una enfermedad o evento agrupados por
semana epidemiológica y número de casos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_semanaepi(
  data_event = data_limpia,
  col_semanaepi = "semana"
)
#> # A tibble: 53 × 2
#>    semana casos
#>    <chr>  <dbl>
#>  1 01         2
#>  2 02         1
#>  3 03        33
#>  4 04         1
#>  5 05         1
#>  6 08         1
#>  7 09         1
#>  8 10         1
#>  9 11         1
#> 10 12         1
#> # ℹ 43 more rows
```
