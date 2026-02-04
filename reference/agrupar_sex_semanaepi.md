# Agrupar por sexo, semana epidemiológica y casos

Función que agrupa los datos de enfermedades por sexo, semana
epidemiológica y número de casos.

## Usage

``` r
agrupar_sex_semanaepi(
  data_event,
  cols_sex = c("sexo", "semana"),
  porcentaje = TRUE
)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- cols_sex:

  Un \`character\` (cadena de caracteres) o \`array\` (arreglo) de
  \`character\` con el nombre de la(s) columna(s) que contienen el sexo
  y las semanas epidemiológicas; su valor por defecto es \`c("sexo",
  "semana")\`.

- porcentaje:

  Un \`logical\` (TRUE o FALSE) que indica si se debe agregar una
  columna con el porcentaje de casos; su valor por defecto es \`TRUE\`.

## Value

Un \`data.frame\` con los datos de la enfermedad o evento agrupados por
sexo, semana epidemiológica y número de casos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_sex_semanaepi(
  data_event = data_limpia,
  cols_sex = c("sexo", "semana"),
  porcentaje = TRUE
)
#> # A tibble: 16 × 7
#>    sexo  semana cod_eve nombre_evento ano   casos porcentaje
#>    <chr> <chr>  <chr>   <chr>         <chr> <int>      <dbl>
#>  1 F     01     210     DENGUE        2020      1       2.13
#>  2 F     03     210     DENGUE        2020     15      31.9 
#>  3 F     04     210     DENGUE        2020      1       2.13
#>  4 F     05     210     DENGUE        2020      1       2.13
#>  5 F     08     210     DENGUE        2020      1       2.13
#>  6 F     11     210     DENGUE        2020      1       2.13
#>  7 F     14     210     DENGUE        2020      1       2.13
#>  8 F     15     210     DENGUE        2020      1       2.13
#>  9 M     01     210     DENGUE        2020      1       2.13
#> 10 M     02     210     DENGUE        2020      1       2.13
#> 11 M     03     210     DENGUE        2020     18      38.3 
#> 12 M     09     210     DENGUE        2020      1       2.13
#> 13 M     10     210     DENGUE        2020      1       2.13
#> 14 M     12     210     DENGUE        2020      1       2.13
#> 15 M     13     210     DENGUE        2020      1       2.13
#> 16 M     16     210     DENGUE        2020      1       2.13
```
