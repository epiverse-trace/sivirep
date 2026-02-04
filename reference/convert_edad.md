# Convertir edad a años

Función que convierte las edades a años según las unidades de medida del
SIVIGILA.

## Usage

``` r
convert_edad(
  data_event,
  col_edad = "edad",
  col_uni_med = "uni_med",
  uni_med = 1
)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de una enfermedad o evento.

- col_edad:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene las edades en los datos de la enfermedad o evento; su
  valor por defecto es \`"edad"\`.

- col_uni_med:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene las unidades de medida en los datos de una enfermedad o
  evento; su valor por defecto es \`"uni_med"\`.

- uni_med:

  Un \`numeric\` (numérico) o \`character\`(cadena de caracteres) que
  contiene la unidad de medida a la que se debe estandarizar la edad; su
  valor por defecto es \`1\`.

## Value

Un \`data.frame\` con las edades convertidas en años según las unidades
de medida del SIVIGILA.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
convert_edad(
  data_event = data_limpia,
  col_edad = "edad",
  col_uni_med = "uni_med",
  uni_med = 1
)
#> # A tibble: 47 × 73
#>    consecutive cod_eve fec_not    semana ano   cod_pre    cod_sub  edad uni_med
#>    <chr>       <chr>   <date>     <chr>  <chr> <chr>      <chr>   <dbl>   <dbl>
#>  1 7322720     210     2020-01-09 01     2020  0504509099 37          1       1
#>  2 7216180     210     2020-01-16 02     2020  0843300151 01         10       1
#>  3 7263660     210     2020-01-20 03     2020  1300101187 01         11       1
#>  4 7298154     210     2020-01-18 03     2020  1875300016 01         12       1
#>  5 7356421     210     2020-01-23 03     2020  7600103359 06         13       1
#>  6 7303963     210     2020-01-15 03     2020  2000100464 02         14       1
#>  7 7304575     210     2020-01-30 03     2020  2300100553 11         15       1
#>  8 7315545     210     2020-01-25 03     2020  4179100333 01         16       1
#>  9 7304407     210     2020-01-29 03     2020  5000101034 01         17       1
#> 10 7233656     210     2020-01-19 03     2020  5400100810 01         18       1
#> # ℹ 37 more rows
#> # ℹ 64 more variables: nacionalidad <chr>, nombre_nacionalidad <chr>,
#> #   sexo <chr>, cod_pais_o <chr>, cod_dpto_o <chr>, cod_mun_o <chr>,
#> #   area <chr>, ocupacion <chr>, tip_ss <chr>, cod_ase <chr>, per_etn <chr>,
#> #   gru_pob <chr>, nom_grupo <lgl>, estrato <chr>, gp_discapa <chr>,
#> #   gp_desplaz <chr>, gp_migrant <chr>, gp_carcela <chr>, gp_gestan <chr>,
#> #   sem_ges <lgl>, gp_indigen <chr>, gp_pobicfb <chr>, gp_mad_com <chr>, …
```
