# Agrupar por años de una enfermedad o evento

Función que agrupa los casos por los años de una enfermedad o evento.

## Usage

``` r
agrupar_years(data_event, col_year = "ano")
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- col_year:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene los años en los datos de la enfermedad o evento; su valor
  por defecto es \`"ano"\`.

## Value

Un \`data.frame\` con los datos de la enfermedad o evento agrupados por
año.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_years(
  data_event = data_limpia,
  col_year = "ano"
)
#> # A tibble: 1 × 4
#>   ano   cod_eve nombre_evento casos
#>   <chr> <chr>   <chr>         <int>
#> 1 2020  210     DENGUE           47
```
