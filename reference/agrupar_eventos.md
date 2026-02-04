# Agrupar por tipo de enfermedad o evento

Función que agrupa los casos por tipo de enfermedad o evento.

## Usage

``` r
agrupar_eventos(data_event, col_event = "cod_eve")
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- col_event:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene los códigos de los eventos o de las enfermedades en los
  datos; su valor por defecto es \`"cod_eve"\`.

## Value

Un \`data.frame\` con los datos de la enfermedad o evento agrupados por
tipo.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_eventos(
  data_event = data_limpia,
  col_event = "cod_eve"
)
#> # A tibble: 1 × 4
#>   cod_eve nombre_evento ano   casos
#>   <chr>   <chr>         <chr> <int>
#> 1 210     DENGUE        2020     47
```
