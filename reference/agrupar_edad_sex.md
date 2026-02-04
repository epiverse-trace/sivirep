# Agrupar por edades, sexo y casos

Función que agrupa los datos de una enfermedad o evento por edades, sexo
y número de casos.

## Usage

``` r
agrupar_edad_sex(
  data_event,
  col_edad = "edad",
  col_sex = "sexo",
  porcentaje = TRUE,
  interval_edad = 10
)
```

## Arguments

- data_event:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento.

- col_edad:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene las edades en los datos de la enfermedad o evento; su
  valor por defecto es \`"edad"\`.

- col_sex:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene el sexo en los datos de la enfermedad o evento; su valor
  por defecto es \`"sexo\`.

- porcentaje:

  Un \`logical\` (TRUE o FALSE) que indica si se debe agregar una
  columna con el porcentaje de casos; su valor por defecto es \`TRUE\`.

- interval_edad:

  Un \`numeric\` (numérico) que contiene el intervalo del rango de
  edades; su valor por defecto es \`10\`.

## Value

Un \`data.frame\` con los datos de enfermedades agrupados por edades,
sexo y número de casos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_edad_sex(
  data_event = data_limpia,
  col_edad = "edad",
  col_sex = "sexo",
  porcentaje = TRUE
)
#>       edad sexo casos porcentaje
#> 1   (0,10]    F     4      8.511
#> 2   (0,10]    M     3      6.383
#> 3  (10,20]    F     6     12.766
#> 4  (10,20]    M    10     21.277
#> 5  (20,30]    F     4      8.511
#> 6  (20,30]    M     7     14.894
#> 7  (30,40]    F     4      8.511
#> 8  (30,40]    M     1      2.128
#> 9  (40,50]    F     1      2.128
#> 10 (40,50]    M     3      6.383
#> 11 (50,60]    F     1      2.128
#> 12 (60,70]    F     1      2.128
#> 13 (60,70]    M     1      2.128
#> 14    <NA>    F     1      2.128
```
